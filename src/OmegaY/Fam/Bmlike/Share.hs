-- | BM-like 公共骨架，逐行对齐 C++ @Header/notation/bmlike_share.hpp@。
module OmegaY.Fam.Bmlike.Share
  ( BmExpr (..)
  , kParseError
  , bmAt
  , columnHeight
  , normalizedColumn
  , normalizeMatrix
  , standardizeMatrix
  , parentsOf
  , parentAtOf
  , numberCompare
  , vectorCompare
  , columnCompare
  , compareMat
  , isLimit
  , dropLastColumn
  , ascendingThreshold
  , bm4Expand
  , parseBm
  , matrixToBmString
  , exprToString
  ) where

import Data.Char (isDigit, isSpace)
import OmegaY.Matrix

data BmExpr = BmExpr
  { beMat :: !Matrix
  } deriving (Eq, Show)

-- | 解析失败统一提示（与 C++ @kParseError@ 逐字一致）。
kParseError :: String
kParseError = "解析失败，请确认你没有使用全角括号，逗号"

-- | 越界读 0（对齐 @bmlike::at@）。
bmAt :: Matrix -> Int -> Int -> Int
bmAt = matAt

columnHeight :: Matrix -> Int -> Int
columnHeight m c = go (matRows m - 1)
 where
  go r
    | r < 0 = 0
    | matAt m r c > 0 = r + 1
    | otherwise = go (r - 1)

normalizedColumn :: Matrix -> Int -> [Int]
normalizedColumn m c = [ matAt m r c | r <- [0 .. h - 1] ]
 where
  h = columnHeight m c

normalizeMatrix :: Matrix -> Matrix
normalizeMatrix m
  | matCols m == 0 = emptyMatrix
  | otherwise =
      Matrix rows (matCols m)
        [ [ matAt m r c | r <- [0 .. h' c - 1] ]
        | c <- [0 .. matCols m - 1]
        ]
 where
  hs = [ columnHeight m c | c <- [0 .. matCols m - 1] ]
  rows = maximum (0 : hs)
  h' c = min rows (columnHeight m c)

standardizeMatrix :: Int -> Matrix -> Matrix
standardizeMatrix minRows m
  | matCols m == 0 = m
  | rows == matRows m = m
  | otherwise = Matrix rows (matCols m) oldCols
 where
  rows = max (matRows m) minRows
  oldCols = matData m

-- | 父项链 P[i][j]（对齐 @bmlike::parents@）。
--
-- 严格按行 j 迭代：第 j 行只依赖第 j-1 行，避免惰性 knot-tying 死锁。
parentsOf :: Matrix -> [[Int]]
parentsOf m = pTab
 where
  rows = matRows m
  nc = matCols m
  -- pRows !! j !! i = 列 i 第 j 行的父项
  pRows = [ row j | j <- [0 .. rows - 1] ]
  row j = [ walk i | i <- [0 .. nc - 1] ]
   where
    prev = if j == 0 then replicate nc 0 else pRows !! (j - 1)
    startP i = if j == 0 then i - 1 else prev !! i
    walk i = go (startP i)
     where
      go p
        | p < 0 = p
        | matAt m j p < matAt m j i = p
        | otherwise = go (if j == 0 then p - 1 else prev !! p)
  -- 转回列主序：pTab !! i !! j
  pTab = [ [ pRows !! j !! i | j <- [0 .. rows - 1] ] | i <- [0 .. nc - 1] ]

parentAtOf :: [[Int]] -> Int -> Int -> Int
parentAtOf pTab c r
  | c < 0 || c >= length pTab = -1
  | r < 0 || r >= length (pTab !! c) = -1
  | otherwise = (pTab !! c) !! r

numberCompare :: Int -> Int -> Int
numberCompare a b = compareI a b
 where
  compareI x y = if x == y then 0 else if x < y then (-1) else 1

vectorCompare :: [Int] -> [Int] -> Int
vectorCompare a b = go 0
 where
  n = min (length a) (length b)
  go i
    | i < n =
        let x = a !! i
            y = b !! i
         in if x /= y then numberCompare x y else go (i + 1)
    | otherwise = numberCompare (length a) (length b)

columnCompare :: Matrix -> Int -> Matrix -> Int -> Int
columnCompare a ca b cb =
  vectorCompare (normalizedColumn a ca) (normalizedColumn b cb)

compareMat :: Matrix -> Matrix -> Int
compareMat a b = go 0
 where
  n = min (matCols a) (matCols b)
  go c
    | c < n =
        let r = columnCompare a c b c
         in if r /= 0 then r else go (c + 1)
    | otherwise = numberCompare (matCols a) (matCols b)

isLimit :: Matrix -> Bool
isLimit m =
  matCols m > 0 && matRows m > 0 && matAt m 0 (matCols m - 1) > 0

dropLastColumn :: Matrix -> Matrix
dropLastColumn m
  | matCols m <= 1 = Matrix (matRows m) 0 []
  | otherwise = Matrix (matRows m) (matCols m - 1) (init (matData m))

ascendingThreshold :: [[Int]] -> Int -> Int -> [Int]
ascendingThreshold pTab r jmax = [ result i | i <- [0 .. nc - 1] ]
 where
  nc = length pTab
  result i
    | i == r = jmax
    | i < r = 0
    | otherwise =
        let found =
              headOr jmax
                [ j
                | j <- [0 .. jmax - 1]
                , let pij = parentAtOf pTab i j
                , pij < 0 || pij < r || j >= result pij
                ]
         in found
  headOr d xs = case xs of
    (x : _) -> x
    [] -> d

-- | BM4 核心展开（对齐 @bmlike::bm4_expand@）。
bm4Expand :: Matrix -> Int -> Bool -> Matrix
bm4Expand m index shorter
  | matCols m == 0 = m
  | topmost < 0 = dropLastColumn m
  | r < 0 = dropLastColumn m
  | otherwise = normalizeMatrix out
 where
  nc = matCols m
  right = nc - 1
  rows = matRows m
  topmost = lastNonzeroRow right
  lastNonzeroRow c = go (rows - 1)
   where
    go r
      | r < 0 = (-1)
      | matAt m r c > 0 = r
      | otherwise = go (r - 1)
  pTab = parentsOf m
  r = parentAtOf pTab right topmost
  aTab = ascendingThreshold pTab r topmost
  offset j = matAt m j right - bmAt m j r
  baseCols = [ normalizedColumn m c | c <- [0 .. right - 1] ]
  outCols =
    baseCols
      ++ [ colVals w i
         | w <- [1 .. index + 1]
         , not (shorter && w == index + 1)
         , i <- [r .. right - 1]
         , not (w == index + 1 && i > r)
         ]
   where
    colVals w i =
      let len = max rows (aTab !! i)
       in [ if y < (aTab !! i)
              then bmAt m y i + offset y * w
              else bmAt m y i
          | y <- [0 .. len - 1]
          ]
  out = packColumns outCols

packColumns :: [[Int]] -> Matrix
packColumns cols =
  let rows = maximum (0 : map length cols)
   in Matrix rows (length cols)
        [ take rows (c ++ repeat 0) | c <- cols ]

-- | 解析 BM 文本（对齐 @bmlike::parse@）。
parseBm :: String -> Maybe BmExpr
parseBm text0 =
  let s = trimSpaces text0
   in if null s
        then Just (BmExpr emptyMatrix)
        else do
              cols <- parseCols s
              let heights = [ trimmedHeight c | c <- cols ]
                  rows = maximum (0 : heights)
                  mat = Matrix rows (length cols)
                    [ [ (cols !! c) !! r
                      | r <- [0 .. min rows (length (cols !! c)) - 1]
                      ]
                    | c <- [0 .. length cols - 1]
                    ]
              Just (BmExpr mat)
 where
  trimSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

  trimmedHeight c =
    let h = length c
     in go h
   where
    go h = if h > 0 && c !! (h - 1) == 0 then go (h - 1) else h

  -- 解析整个字符串为列列表
  parseCols s0 = go s0 []
   where
    go s acc =
      let s1 = dropWhile (== ' ') s
       in case s1 of
            [] -> Just (reverse acc)
            ('(' : rest) ->
              let rest1 = dropWhile (== ' ') rest
               in case rest1 of
                    (')' : rest2) -> go rest2 ([] : acc)
                    _ -> do
                      (col, rest2) <- parseCol rest1
                      go rest2 (col : acc)
            _ -> Nothing

  parseCol s0 = go s0 []
   where
    go s acc =
      let s1 = dropWhile (== ' ') s
       in case s1 of
            (ch : _) | isDigit ch ->
              let (ds, rest) = span isDigit s1
                  v = foldl (\a d -> a * 10 + digit d) 0 ds
                  rest1 = dropWhile (== ' ') rest
               in case rest1 of
                    (',' : rest2) -> go rest2 (acc ++ [v])
                    (')' : rest2) -> Just (acc ++ [v], rest2)
                    _ -> Nothing
            _ -> Nothing
    digit d = fromEnum d - fromEnum '0'

-- | 矩阵 → BM 文本；空列显示为 @(0)@。
matrixToBmString :: Matrix -> String
matrixToBmString m =
  concat
    [ '(' : (if h == 0 then "0" else joinVals c h) ++ ")"
    | c <- [0 .. matCols m - 1]
    , let h = columnHeight m c
    ]
 where
  joinVals c h =
    let vals = [ show (matAt m r c) | r <- [0 .. h - 1] ]
     in joinComma vals
  joinComma [] = []
  joinComma [x] = x
  joinComma (x : xs) = x ++ "," ++ joinComma xs

exprToString :: BmExpr -> String
exprToString = matrixToBmString . beMat
