{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | DLL 的 C ABI 层：把注册表里的 114 个记号暴露成 C 函数。
--
--   设计要点（与 @docs/DLL.md@ 一一对应）：
--
--   * 记号名 -> 导出符号名的规则唯一实现在 'symbolOf'，生成器
--     @tools/GenDll.hs@ 与本模块的 @EXPANDER2_SYMBOL@ 都调它，
--     避免头文件与 DLL 里的名字漂移。
--   * 每个记号两个入口：@\<SYM\>@（fs 模式）与 @\<SYM\>_ALTER@（alter 模式），
--     语义与 CLI 的 @\<id\> fs|alter@ 完全一致（都走 'expandNotation'）。
--   * 字符串一律 UTF-8；返回的 @char*@ 由 GHC 侧 @malloc@ 分配，
--     必须用 @EXPANDER2_FREE@ 释放。
--   * 失败返回 @NULL@，错误文本放进程级槽位，用 @EXPANDER2_LAST_ERROR@ 取
--     （errno 风格：读一次就复制出独立字符串）。
--   * 任何 Haskell 异常都在这里兜住，绝不越过 DLL 边界（否则整个宿主进程退出）。
module OmegaY.Dll
  ( expandWith
  , symbolOf
  ) where

import Control.Exception (SomeException, evaluate, try)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Char (chr, toUpper)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

import OmegaY.Notation.Types (Caps (..), ExpandMode (..), Notation (..), expandNotation)
import OmegaY.Registry (allNotations, byId)

-- ---------------------------------------------------------------------------
-- 导出符号名
-- ---------------------------------------------------------------------------

-- | 记号 id -> C 导出符号名。
--
--   @pps@ -> @PPS@，@bms-v1@ -> @BMS_V1@，@-1y-(n+1)ss@ -> @_1Y_N_1_SS@。
--   规则：只保留 @[A-Za-z0-9]@（字母一律大写），其余字符变 @_@，
--   连续 @_@ 折成一个；若结果以数字开头则前置 @_@（保证是合法 C 标识符）。
symbolOf :: String -> String
symbolOf = fixLead . squeeze . map conv
  where
    conv c
      | c >= 'a' && c <= 'z' = toUpper c
      | c >= 'A' && c <= 'Z' = c
      | c >= '0' && c <= '9' = c
      | otherwise = '_'

    squeeze [] = []
    squeeze (c : cs) = c : go c cs
      where
        go _ [] = []
        go prev (x : xs)
          | x == '_' && prev == '_' = go prev xs
          | otherwise = x : go x xs

    fixLead s@(c : _)
      | c >= '0' && c <= '9' = '_' : s
    fixLead s = s

-- ---------------------------------------------------------------------------
-- UTF-8 ⇄ String（不依赖 bytestring / GHC 内部模块，纯 base 实现）
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "string.h strlen"
  c_strlen :: CString -> IO CSize

encodeUtf8 :: String -> [Word8]
encodeUtf8 = concatMap enc
  where
    enc c
      | o < 0x80 = [w o]
      | o < 0x800 = [0xC0 .|. w (o `shiftR` 6), 0x80 .|. w (o .&. 0x3F)]
      | o < 0x10000 =
          [ 0xE0 .|. w (o `shiftR` 12)
          , 0x80 .|. w ((o `shiftR` 6) .&. 0x3F)
          , 0x80 .|. w (o .&. 0x3F)
          ]
      | otherwise =
          [ 0xF0 .|. w (o `shiftR` 18)
          , 0x80 .|. w ((o `shiftR` 12) .&. 0x3F)
          , 0x80 .|. w ((o `shiftR` 6) .&. 0x3F)
          , 0x80 .|. w (o .&. 0x3F)
          ]
      where
        o = fromEnum c
        w :: Int -> Word8
        w = fromIntegral

decodeUtf8 :: [Word8] -> String
decodeUtf8 = go
  where
    go [] = []
    go (b0 : bs)
      | b0 < 0x80 = chr (fromIntegral b0) : go bs
      | b0 >= 0xC2 && b0 <= 0xDF = multi 2 b0 0x1F bs
      | b0 >= 0xE0 && b0 <= 0xEF = multi 3 b0 0x0F bs
      | b0 >= 0xF0 && b0 <= 0xF4 = multi 4 b0 0x07 bs
      | otherwise = '\xFFFD' : go bs

    multi n b0 mask bs =
      case splitAt (n - 1) bs of
        (cs, rest)
          | length cs == n - 1 && all isCont cs ->
              let acc = foldl step (fromIntegral (b0 .&. mask) :: Int) cs
               in if acc > 0x10FFFF then '\xFFFD' : go bs else chr acc : go rest
        _ -> '\xFFFD' : go bs
      where
        step a wbyte = (a `shiftL` 6) .|. fromIntegral (wbyte .&. 0x3F)

    isCont wbyte = wbyte >= 0x80 && wbyte <= 0xBF

-- | 读一个 C 字符串（UTF-8）。@NULL@ 视为空串。
peekUtf8 :: CString -> IO String
peekUtf8 cs
  | cs == nullPtr = pure ""
  | otherwise = do
      n <- c_strlen cs
      ws <- peekArray (fromIntegral n) (castPtr cs)
      pure (decodeUtf8 ws)

-- | 分配一个 UTF-8 C 字符串（malloc，长度含结尾 0）。
newUtf8 :: String -> IO CString
newUtf8 s = do
  let ws = encodeUtf8 s ++ [0]
  p <- mallocBytes (length ws)
  pokeArray (castPtr p) ws
  pure p

-- ---------------------------------------------------------------------------
-- 错误槽位（errno 风格）
-- ---------------------------------------------------------------------------

{-# NOINLINE errSlot #-}
errSlot :: IORef String
errSlot = unsafePerformIO (newIORef "")

setErr :: String -> IO ()
setErr = atomicWriteIORef errSlot

-- 深强制：把 Either 两侧的字符串整体求值，异常才会在这一层被 try 抓到
forceStr :: String -> String
forceStr [] = []
forceStr (c : cs) = c `seq` c : forceStr cs

forceEither :: Either String String -> Either String String
forceEither (Left e) = Left (forceStr e)
forceEither (Right s) = Right (forceStr s)

-- ---------------------------------------------------------------------------
-- 展开
-- ---------------------------------------------------------------------------

-- | 记号 id + 模式（0=fs，1=alter）+ 输入文本 -> 结果（失败 @NULL@）。
expandWith :: String -> Int -> CString -> Int -> Int -> IO CString
expandWith ident mode cs term param = do
  text <- peekUtf8 cs
  let md = if mode == 0 then Fs else FsAlter
      compute :: Either String String
      compute =
        case byId ident of
          Nothing -> Left ("未知记号：" ++ ident)
          Just n -> expandNotation n md text term param
  r <- try (evaluate (forceEither compute)) :: IO (Either SomeException (Either String String))
  case r of
    Left ex -> do
      setErr ("内部错误：" ++ show ex)
      pure nullPtr
    Right (Left err) -> do
      setErr err
      pure nullPtr
    Right (Right out) -> newUtf8 out

-- ---------------------------------------------------------------------------
-- 注册表自省
-- ---------------------------------------------------------------------------

notationAt :: CInt -> Maybe Notation
notationAt i
  | i < 0 = Nothing
  | fromIntegral i >= length allNotations = Nothing
  | otherwise = Just (allNotations !! fromIntegral i)

kindOf :: Notation -> Int
kindOf n = case nCaps n of
  SeqCaps {} -> 0
  TextCaps {} -> 1
  ParamCaps {} -> 2

cExpand :: CString -> CInt -> CInt -> CInt -> CString -> IO CString
cExpand idCS mode term param inputCS = do
  ident <- peekUtf8 idCS
  expandWith ident (fromIntegral mode) inputCS (fromIntegral term) (fromIntegral param)

cFree :: Ptr CChar -> IO ()
cFree = free

cLastError :: IO CString
cLastError = readIORef errSlot >>= newUtf8

cCount :: IO CInt
cCount = pure (fromIntegral (length allNotations))

badIndex :: CInt -> IO ()
badIndex i = setErr ("记号下标越界：" ++ show i)

-- | 自省类入口的公共骨架：越界 -> 置错误并返回 @fallback@。
withIndex :: CInt -> a -> (Notation -> IO a) -> IO a
withIndex i fallback k =
  case notationAt i of
    Nothing -> badIndex i >> pure fallback
    Just n -> k n

cId :: CInt -> IO CString
cId i = withIndex i nullPtr (newUtf8 . nId)

cName :: CInt -> IO CString
cName i = withIndex i nullPtr (newUtf8 . nName)

cSymbol :: CInt -> IO CString
cSymbol i = withIndex i nullPtr (newUtf8 . symbolOf . nId)

cKind :: CInt -> IO CInt
cKind i = withIndex i (-1) (pure . fromIntegral . kindOf)

cParamMin, cParamMax, cParamDefault :: CInt -> IO CInt
cParamMin = paramc (\(mn, _, _, _) -> mn)
cParamMax = paramc (\(_, mx, _, _) -> mx)
cParamDefault = paramc (\(_, _, d, _) -> d)

paramc :: ((Int, Int, Int, String) -> Int) -> CInt -> IO CInt
paramc pick i = withIndex i (-1) (\n -> pure (fromIntegral (maybe (-1) pick (nParam n))))

dllVersion :: String
dllVersion = "1.0"

backendName :: String
#ifdef EXPANDER2_LLVM_BACKEND
backendName = "llvm"
#else
backendName = "ncg"
#endif

cBuild :: IO CString
cBuild = newUtf8 ("expander2.haskell/dll " ++ dllVersion ++ " backend=" ++ backendName)

-- ---------------------------------------------------------------------------
-- C 入口（名字即 ABI，不要改）
-- ---------------------------------------------------------------------------

foreign export ccall "EXPANDER2_EXPAND"
  cExpand :: CString -> CInt -> CInt -> CInt -> CString -> IO CString

foreign export ccall "EXPANDER2_FREE"
  cFree :: Ptr CChar -> IO ()

foreign export ccall "EXPANDER2_LAST_ERROR"
  cLastError :: IO CString

foreign export ccall "EXPANDER2_COUNT"
  cCount :: IO CInt

foreign export ccall "EXPANDER2_ID"
  cId :: CInt -> IO CString

foreign export ccall "EXPANDER2_NAME"
  cName :: CInt -> IO CString

foreign export ccall "EXPANDER2_SYMBOL"
  cSymbol :: CInt -> IO CString

foreign export ccall "EXPANDER2_KIND"
  cKind :: CInt -> IO CInt

foreign export ccall "EXPANDER2_PARAM_MIN"
  cParamMin :: CInt -> IO CInt

foreign export ccall "EXPANDER2_PARAM_MAX"
  cParamMax :: CInt -> IO CInt

foreign export ccall "EXPANDER2_PARAM_DEFAULT"
  cParamDefault :: CInt -> IO CInt

foreign export ccall "EXPANDER2_BUILD"
  cBuild :: IO CString
