{-# LANGUAGE OverloadedStrings #-}
-- | DBMS 族共享底层解析/显示助手。忠实移植 C++ 侧 sdbms_core.hpp / 各记号的
--   parse_column / skip_spaces / skip_index 骨架。
module OmegaY.Fam.Dbms.Common
  ( Err
  , errParse
  , errResource
  , skipSp
  , skipIdx
  , parseNum
  , expect
  , parseCol
  , dropTrailing
  , sgn
  , cmpI
  ) where

-- | 解析失败统一文本（C++ 逐字）。
errParse :: String
errParse = "解析失败，请确认你没有使用全角括号，逗号，分号"

-- | 资源上限文本（仅 TS2DBMS / TSωDBMS）。
errResource :: String
errResource = "展开超出资源上限（表达式过大或下标过大）"

type Err = String

cmpI :: Int -> Int -> Int
cmpI a b = if a < b then -1 else if a > b then 1 else 0

sgn :: Int -> Int
sgn v = if v < 0 then -1 else if v > 0 then 1 else 0

-- | 仅跳过空格（C++ skip_spaces 只认 ' '）。
skipSp :: String -> String
skipSp = dropWhile (== ' ')

-- | skip_index：':' 后接空白再吃数字。
skipIdx :: String -> String
skipIdx (':' : rest) = takeWhile (`elem` ("0123456789" :: String)) (skipSp rest)
skipIdx s = s

-- | parse_number：先跳过空格，再读无符号整数；无数字则失败。
parseNum :: String -> Maybe (Int, String)
parseNum s =
  case span (`elem` ("0123456789" :: String)) (skipSp s) of
    ([], _) -> Nothing
    (ds, rest) -> Just (read ds, rest)

-- | 期望某字符（不跳空格，由调用方负责）。
expect :: Char -> String -> Maybe String
expect c (x : rest) | x == c = Just rest
expect _ _ = Nothing

-- | 列解析骨架：跳过空格后必须见 '('，逗号分隔条目，随后 skip_index，再 ')'。
--   entryP 接收“条目起始处（已 skipSp）”的串，返回 (条目, 剩余)。
--   返回 (条目正向列表, ')' 之后的剩余串)。
parseCol :: (String -> Maybe (e, String)) -> String -> Maybe ([e], String)
parseCol entryP s0 = do
  s1 <- expect '(' (skipSp s0)
  let s2 = skipSp s1
  (entries, s3) <- case s2 of
    (')' : _) -> Just ([], s2)
    (':' : _) -> Just ([], s2)
    [] -> Just ([], s2)
    _ -> do
      (e, s2') <- entryP s2
      entriesLoop s2' [e]
  let s4 = skipIdx (skipSp s3)
  s5 <- expect ')' s4
  Just (entries, s5)
  where
    entriesLoop s acc =
      case skipSp s of
        (',' : r) ->
          let sr = skipSp r
           in case sr of
                (')' : _) -> Just (reverse acc, sr)
                _ -> do
                  (e, sr') <- entryP sr
                  entriesLoop sr' (e : acc)
        _ -> Just (reverse acc, skipSp s)

-- | 删去列尾 value==-1 的条目（C++ while(!col.empty() && col.back().value==-1) pop_back）。
dropTrailing :: [e] -> (e -> Int) -> [e]
dropTrailing entries valOf =
  reverse (dropWhile ((== (-1)) . valOf) (reverse entries))
