-- | MrSS（三角形嵌套序列）公共表示与 MrSS1.2.1 展开，对齐 C++ 侧
--   @common/mrssshare.hpp@ 与 @notation/MrSS121.hpp@。
module OmegaY.Mrss
  ( Expr (..)
  , mrssParse
  , mrssToString
  , exprToString
  , isOne
  , elementOrder
  , expressionOrder
  , firstValue
  , lastValue
  , addValue
  , fromIntSequence
  , toIntSequence
  , mrssExpand
  , mrssExpandText
  , mrssTruncate
  ) where

import Data.Char (isDigit, isSpace)

-- | 原子（整数）或子表达式序列。
data Expr = Atom Int | Node [Expr]
  deriving (Eq, Show)

-- ----------------------------------------------------------------------
-- 解析与序列化
-- ----------------------------------------------------------------------

-- | 解析 @1,(2,3),4@ 形式；失败返回 Nothing（C++ 抛/返回 nullopt）。
mrssParse :: String -> Maybe [Expr]
mrssParse s = do
  (seq, rest) <- parseSequenceUntil Nothing (dropWhile isSpace s)
  let rest' = dropWhile isSpace rest
  if null rest' then Just seq else Nothing

mrssToString :: [Expr] -> String
mrssToString [] = "[]"
mrssToString xs = intercalateComma (map exprToString xs)

exprToString :: Expr -> String
exprToString (Atom v) = show v
exprToString (Node cs) = "(" ++ intercalateComma (map exprToString cs) ++ ")"

intercalateComma :: [String] -> String
intercalateComma [] = ""
intercalateComma [x] = x
intercalateComma (x : xs) = x ++ "," ++ intercalateComma xs

-- | @end@ 为终止字符（@Just ')'@ 或 Nothing 表示读到结尾）。
parseSequenceUntil :: Maybe Char -> String -> Maybe ([Expr], String)
parseSequenceUntil end s0 = go [] (dropWhile isSpace s0)
 where
  go acc s =
    case s of
      [] -> Just (reverse acc, [])
      (c : _) | Just c == end -> Just (reverse acc, s)
      _ -> do
        (e, rest) <- parseElement s
        let rest1 = dropWhile isSpace rest
        case rest1 of
          [] -> Just (reverse (e : acc), [])
          (c : _) | Just c == end -> Just (reverse (e : acc), rest1)
          (',' : rest2) ->
            let rest3 = dropWhile isSpace rest2
             in case rest3 of
                  -- C++：逗号后必须还有一个元素（或紧接终止符）；
                  -- 输入就此结束则整体解析失败（"1,2," 之类）。
                  [] -> Nothing
                  (c : _) | Just c == end -> Just (reverse (e : acc), rest3)
                  _ -> go (e : acc) rest3
          _ -> Nothing

parseElement :: String -> Maybe (Expr, String)
parseElement s0 =
  case dropWhile isSpace s0 of
    [] -> Nothing
    ('(' : rest) -> do
      (seq, rest1) <- parseSequenceUntil (Just ')') rest
      case rest1 of
        (')' : rest2) -> Just (Node seq, rest2)
        _ -> Nothing
    s -> do
      (v, rest) <- parseInt s
      Just (Atom v, rest)

parseInt :: String -> Maybe (Int, String)
parseInt s0 =
  case dropWhile isSpace s0 of
    (c : _) | isDigit c ->
      let (ds, rest) = span isDigit (dropWhile isSpace s0)
       in Just (foldl (\a d -> a * 10 + fromEnum d - 48) 0 ds, rest)
    _ -> Nothing

-- ----------------------------------------------------------------------
-- 序与结构工具
-- ----------------------------------------------------------------------

isOne :: Expr -> Bool
isOne (Atom 1) = True
isOne _ = False

elementOrder :: Expr -> Int
elementOrder (Atom 1) = 0
elementOrder (Atom _) = 1
elementOrder (Node cs) = 1 + maximum (-1 : map elementOrder cs)

expressionOrder :: [Expr] -> Int
expressionOrder seq = maximum (-1 : map elementOrder seq)

firstValue :: Expr -> Int
firstValue (Atom v) = v
firstValue (Node []) = 0
firstValue (Node (c : _)) = firstValue c

lastValue :: Expr -> Int
lastValue (Atom v) = v
lastValue (Node []) = 0
lastValue (Node cs) = lastValue (last cs)

-- | 只在最右侧分支加 delta（对齐 C++ @add_value@）。
addValue :: Expr -> Int -> Expr
addValue (Atom v) d = Atom (v + d)
addValue (Node []) _ = Node []
addValue (Node cs) d = Node (init cs ++ [addValue (last cs) d])

fromIntSequence :: [Int] -> [Expr]
fromIntSequence = map Atom

toIntSequence :: [Expr] -> Maybe [Int]
toIntSequence = mapM f
 where
  f (Atom v) = Just v
  f (Node _) = Nothing

-- ----------------------------------------------------------------------
-- MrSS1.2.1 展开
-- ----------------------------------------------------------------------

-- | @startsWith a b@：a 的最左下降链以 b 开头（对齐 C++ @starts_with@）。
startsWith :: Expr -> Expr -> Bool
startsWith (Atom a) (Atom b) = a == b
startsWith (Node cs) (Atom b) = maybe False (`startsWith` Atom b) (safeHead cs)
startsWith (Atom _) (Node _) = False
startsWith (Node as) (Node bs) =
  length as >= length bs && and (zipWith startsWith as bs)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | 找“坏根”：最后一个元素之前，第一个更小或最左链为其前缀者。
findBadRoot :: [Expr] -> Maybe Int
findBadRoot seq
  | length seq < 2 = Nothing
  | otherwise = go (length seq - 2)
 where
  lastE = last seq
  go i
    | i < 0 = Nothing
    | otherwise =
        let cand = seq !! i
         in case (cand, lastE) of
              -- 两个原子：只有 cand < last 才算坏根；相等/更大都继续往前找
              (Atom cv, Atom lv) -> if cv < lv then Just i else go (i - 1)
              _ -> if startsWith lastE cand then Just i else go (i - 1)

difference :: Expr -> Expr -> Int
difference lastE root = lastValue lastE - lastValue root - 1

-- | 展开主体：@bad@ 段按 @diff * i@ 平移 @term@ 次，并对根值做补足。
replaceLast :: [Expr] -> [Expr] -> Int -> Int -> [Expr]
replaceLast seq bad diff term =
  let result0 = init seq
      copies = [addValue e (diff * i) | i <- [1 .. term], e <- bad]
      result1 = result0 ++ copies
   in if not (null bad) && not (null result1)
        then
          let rootVal = lastValue (head bad)
              loop r extra
                | lastValue (last r) <= rootVal && extra < term =
                    loop (r ++ [addValue e (diff * (term + extra + 1)) | e <- bad]) (extra + 1)
                | otherwise = r
           in loop result1 0
        else result1

-- | 记号极限以外的一步展开；结构与 C++ @expand_y_1@ / @expand_high@ 相同。
mrssStep :: [Expr] -> Int -> [Expr]
mrssStep seq term
  | null seq = []
  | isOne (last seq) = init seq
  | otherwise =
      case findBadRoot seq of
        Nothing -> init seq
        Just root ->
          let diff = difference (last seq) (seq !! root)
              bad = take (length seq - 1 - root) (drop root seq)
           in replaceLast seq bad diff term

mrssExpand :: [Expr] -> Int -> [Expr]
mrssExpand seq term0
  | null seq = []
  | otherwise =
      let term = max 1 term0
       in mrssStep seq term

-- | 文本入口：解析失败返回 @[]@（对齐 C++ @expand_string@）。
mrssExpandText :: String -> Int -> String
mrssExpandText text term0 =
  case mrssParse text of
    Nothing -> "[]"
    Just e -> mrssToString (mrssExpand e (max 1 term0))

-- | INotation 的默认 FS 截断：按 MrSS 解析，成功且长度 > 1 时去掉末元素。
mrssTruncate :: String -> Int -> String
mrssTruncate out _ =
  case mrssParse out of
    Just parsed | length parsed > 1 -> mrssToString (init parsed)
    _ -> out
