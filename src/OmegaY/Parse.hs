-- | 文本 ⇄ 数据的解析与序列化，对齐 C++ 侧 omegay::common 的 sequence / Matrix。
module OmegaY.Parse
  ( parseSequence
  , parseIntStrict
  , seqToString
  , parseMatrix
  , matrixToString
  , parseBMCols
  , digitsToInt
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)
import OmegaY.Matrix

-- | 逗号分隔整数序列。
--
--   与 C++ @parse_sequence@ 一致：按 ',' 切分，去掉分段内所有空白，
--   空分段跳过，@std::stoi@ 语义（可带符号、遇非数字停止），失败的分段
--   被静默丢弃。
parseSequence :: String -> [Int]
parseSequence s =
  [ v
  | part <- splitOn ',' s
  , let tok = filter (not . isSpace) part
  , not (null tok)
  , Just v <- [stoi tok]
  ]

-- | 对齐 @std::stoi@ 且带溢出判定（C++ 侧 out_of_range 也进 catch 分支）。
parseIntStrict :: String -> Maybe Int
parseIntStrict s0 =
  let hs = dropWhile isSpace s0
      (sign, rest0) = case hs of
        ('-' : r) -> (-1, r)
        ('+' : r) -> (1, r)
        _ -> (1, hs)
      (ds, _) = span isDigit rest0
   in if null ds
        then Nothing
        else
          let v = sign * digitsToInt ds
           in if v < -2147483648 || v > 2147483647 then Nothing else Just v

-- | 对齐 @std::stoi@：解析前导可选符号 + 十进制数字；无数字则失败。
stoi :: String -> Maybe Int
stoi s0 =
  case s0 of
    ('-' : rest) -> negate <$> unsigned rest
    ('+' : rest) -> unsigned rest
    _ -> unsigned s0
 where
  unsigned s =
    let (ds, _) = span isDigit s
     in if null ds then Nothing else Just (digitsToInt ds)

digitsToInt :: String -> Int
digitsToInt = foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0

splitOn :: Char -> String -> [String]
splitOn sep s = go s
 where
  go xs =
    let (a, b) = break (== sep) xs
     in case b of
          [] -> [a]
          (_ : rest) -> a : go rest

-- | 空序列输出 @[]@，否则逗号连接。
seqToString :: [Int] -> String
seqToString [] = "[]"
seqToString xs = intercalate' "," (map show xs)
 where
  intercalate' _ [] = []
  intercalate' _ [x] = x
  intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

-- | 解析形如 @(0,0)(1,1)[3]@ 的 BMS 表达式，返回矩阵与可选的 [n]。
--
--   行数取所有列中出现过的最大行数；短列补 0；@'['@ 之后为 term。
--   与 C++ @parse_matrix@ 逐字符行为一致（空白跳过，非法字符重置数字累积）。
parseMatrix :: String -> (Matrix, Maybe Int)
parseMatrix s =
  let nr = maxRowCount s
   in if nr <= 0
        then (emptyMatrix, Nothing)
        else
          let (cols, mterm) = secondPass nr s
           in (fromColumns cols, mterm)

-- 第一遍：只统计 '(' … ')' 内逗号数（在 '[' 之前）。
maxRowCount :: String -> Int
maxRowCount = go 0 0 False
 where
  go :: Int -> Int -> Bool -> String -> Int
  go nr rowCnt inParen [] = nr
  go nr rowCnt inParen (c : cs)
    | c == '[' = nr
    | c == '(' = go nr 1 True cs
    | c == ',' = go nr (if inParen then rowCnt + 1 else rowCnt) inParen cs
    | c == ')' = go (if inParen && rowCnt > nr then rowCnt else nr) rowCnt False cs
    | otherwise = go nr rowCnt inParen cs

-- 第二遍：逐字符累积数字，遇 ')' 落列。
--
-- 注意 C++ 原实现里 @sign/neg@ 只被赋值、从未参与计算（@(void)sign@），
-- 因此本函数同样忽略负号，只累加十进制数字，逐位与原实现对齐。
secondPass :: Int -> String -> ([[Int]], Maybe Int)
secondPass nr s0 = go 0 [] Nothing (replicate nr 0) False s0
 where
  go :: Int -> [[Int]] -> Maybe Int -> [Int] -> Bool -> String -> ([[Int]], Maybe Int)
  go _ cols mterm _ _ [] = (reverse cols, mterm)
  go row cols mterm cur hasDigit (c : rest) =
    case c of
      '(' -> go 0 cols mterm (replicate nr 0) False rest
      ',' -> go (row + 1) cols mterm cur False rest
      ')' ->
        let col = take nr (cur ++ repeat 0)
         in go 0 (col : cols) mterm (replicate nr 0) False rest
      '[' ->
        let ds = takeWhile isDigit rest
         in (reverse cols, Just (if null ds then 0 else digitsToInt ds))
      _ | isDigit c ->
            if row >= 128
              then go row cols mterm cur hasDigit rest
              else
                let prev = if not hasDigit || row >= nr then 0 else cur !! row
                    v = 10 * prev + (fromEnum c - fromEnum '0')
                    cur' = if row < nr then replaceAt row v cur else cur
                 in go row cols mterm cur' True rest
        | c == '-' ->
            -- 与 C++ 一致：负号只影响未使用的 sign，实际按非负解析
            go row cols mterm cur True rest
        | c `elem` " \t\n\r" -> go row cols mterm cur hasDigit rest
        | otherwise -> go row cols mterm cur False rest

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- | 序列化为 @(a,b)(c,d)...@；term > 0 时追加 @[term]@（对齐 C++）。
matrixToString :: Matrix -> Int -> String
matrixToString m term =
  concat
    [ "(" ++ intercalate' "," ([show (matAt m r c) | r <- [0 .. matRows m - 1]]) ++ ")"
    | c <- [0 .. matCols m - 1]
    ]
    ++ if term > 0 then "[" ++ show term ++ "]" else ""
 where
  intercalate' _ [] = []
  intercalate' _ [x] = x
  intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

-- | 把 @(1,2)(3)@ 解析成列的列表（不补 0，仅去掉 @[n]@ 尾巴）。
parseBMCols :: String -> [[Int]]
parseBMCols s =
  let body = takeWhile (/= '[') s
   in [ [ digitsToInt ds | ds <- numberGroups col ]
      | col <- parens body
      ]
 where
  parens :: String -> [String]
  parens [] = []
  parens xs =
    case break (== '(') xs of
      (_, []) -> []
      (_, _ : rest) ->
        let (inside, after) = break (== ')') rest
         in inside : parens (drop 1 after)

  numberGroups :: String -> [String]
  numberGroups str = filter (not . null) (splitOn ',' str)
