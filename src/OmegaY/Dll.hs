{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | C ABI 层。字符串为 UTF-8；返回的 char* 由 GHC 侧 malloc 分配，须用
--   EXPANDER2_FREE 释放。失败返回 NULL，错误经 EXPANDER2_LAST_ERROR 读取；
--   所有异常在本层拦截，不允许传播到宿主进程。
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

-- 导出符号名：只保留 ASCII 字母数字，驼峰折叠为下划线，数字开头补前缀；
-- 规则须与 tools/GenDll.hs 生成的头文件一致。
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

-- UTF-8 ⇄ String，纯 base 实现，避免 DLL 额外链接 bytestring。

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

peekUtf8 :: CString -> IO String
peekUtf8 cs
  | cs == nullPtr = pure ""
  | otherwise = do
      n <- c_strlen cs
      ws <- peekArray (fromIntegral n) (castPtr cs)
      pure (decodeUtf8 ws)

-- 返回 malloc 内存；调用方负责 EXPANDER2_FREE，长度含结尾 0。
newUtf8 :: String -> IO CString
newUtf8 s = do
  let ws = encodeUtf8 s ++ [0]
  p <- mallocBytes (length ws)
  pokeArray (castPtr p) ws
  pure p

-- 进程级错误槽。只做整串覆盖写，读侧复制出独立 String，因此用
-- unsafePerformIO 持有单个 IORef 即可，不需要加锁。
{-# NOINLINE errSlot #-}
errSlot :: IORef String
errSlot = unsafePerformIO (newIORef "")

setErr :: String -> IO ()
setErr = atomicWriteIORef errSlot

-- 深强制：把字符串的脊和每个字符都逼出来，异常才会在这一层被 try 抓到。
-- 注意只 evaluate 到 WHNF 是不够的：Either 的字段是惰性的，
-- `evaluate (Right s)` 只压到构造子，真正的展开异常会留到 newUtf8 里才爆，
-- 那时已经出了 try，会穿过 foreign export 变成 RTS 的 uncaught exception
-- （实测 1-mn + "(0,0)(1,1)" 直接 abort 宿主进程）。
forceStr :: String -> String
forceStr [] = []
forceStr (c : cs) = c `seq` c : forceStr cs

-- | 求值到"脊 + 字符"全强制，返回值本身仍是原串。
--   用 length 逼脊，用 forceStr 逼字符；顺序不能换，否则 length 只到脊。
deepForce :: String -> String
deepForce s = length (forceStr s) `seq` s

-- 展开

expandWith :: String -> Int -> CString -> Int -> Int -> IO CString
expandWith ident mode cs term param = do
  text <- peekUtf8 cs
  let md = if mode == 0 then Fs else FsAlter
      compute :: Either String String
      compute =
        case byId ident of
          Nothing -> Left ("未知记号：" ++ ident)
          Just n -> expandNotation n md text term param
  -- 强制求值必须发生在 try 里，并且必须是深求值：两侧都要先钉死再返回。
  r <-
    try
      ( case compute of
          Left e -> do
            _ <- evaluate (length (forceStr e))
            pure (Left e)
          Right s -> do
            _ <- evaluate (length (forceStr s))
            pure (Right s)
      )
      :: IO (Either SomeException (Either String String))
  case r of
    Left ex -> do
      setErr ("内部错误：" ++ show ex)
      pure nullPtr
    Right (Left err) -> do
      setErr (deepForce err)
      pure nullPtr
    Right (Right out) -> newUtf8 (deepForce out)

-- 注册表自省

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

-- C 入口（名字即 ABI，不要改）

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
