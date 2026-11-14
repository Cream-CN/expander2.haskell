-- | 本族共享的解析原语与错误文本，逐字对齐 C++ @bmlike_share.hpp@ 的
--   @kParseError@ 与各记号 Parser 的手写递归下降（空格跳过、数字、抛 "illegal"）。
module OmegaY.Fam.M1ynss.PP
  ( kParseError
  , P(..)
  , runP
  , sp
  , atEnd
  , peek
  , startsWith
  , advance
  , numTok
  , err
  ) where

import Control.Monad (ap, unless)

-- | 解析失败时统一返回的提示语（UTF-8，逐字照抄 C++）。
kParseError :: String
kParseError = "解析失败，请确认你没有使用全角括号，逗号"

-- | 轻量手写解析 monad：状态 = (源码, 下标)；任何错误都用左值 "illegal" 表示。
newtype P a = P { unP :: String -> Int -> Either String (a, Int) }

runP :: String -> P a -> Either String (a, Int)
runP s p = unP p s 0

instance Functor P where
  fmap f (P g) = P $ \s i -> case g s i of
    Left e -> Left e
    Right (a, i') -> Right (f a, i')

instance Applicative P where
  pure a = P $ \_ i -> Right (a, i)
  (<*>) = ap

instance Monad P where
  P g >>= h = P $ \s i -> case g s i of
    Left e -> Left e
    Right (a, i') -> unP (h a) s i'

-- | 抛出解析错误（对应 C++ @throw std::runtime_error("illegal")@）。
err :: P a
err = P $ \_ _ -> Left "illegal"

-- | 跳过空格（C++ 只跳 ' '，不跳 \\t 等）。
sp :: P ()
sp = P $ \s i -> Right ((), dropSp s i)
  where
    dropSp s i = case drop i s of
      (' ' : rest) -> dropSp s (i + 1)
      _ -> i

atEnd :: P Bool
atEnd = P $ \s i -> Right (i >= length s, i)

peek :: P (Maybe Char)
peek = P $ \s i -> Right (if i < length s then Just (s !! i) else Nothing, i)

-- | 从当前位置起是否匹配字面串 @lit@（不消费，先 sp 后比较由调用方处理）。
startsWith :: String -> P Bool
startsWith lit = P $ \s i -> Right (take (length lit) (drop i s) == lit, i)

advance :: P ()
advance = P $ \_ i -> Right ((), i + 1)

-- | 读取非负整数（C++ 数字只接受 0-9，无负号；空串即错）。
numTok :: P Int
numTok = P $ \s i0 -> do
  let i1 = dropSp s i0
      ds = takeWhile (\c -> c >= '0' && c <= '9') (drop i1 s)
  unless (not (null ds)) (Left "illegal")
  let v = foldl (\acc c -> acc * 10 + (fromEnum c - fromEnum '0')) 0 ds
  Right (v, i1 + length ds)
  where
    dropSp s i = case drop i s of
      (' ' : rest) -> dropSp s (i + 1)
      _ -> i
