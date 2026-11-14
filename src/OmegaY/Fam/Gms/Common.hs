-- | GMS / LPMS 族共享底层工具。对齐 C++ @gms_detail@ / @lpms_detail@ 中的
--   FMat（列向量表）操作、BM 文本解析与序列化。
--
--   * FMat = 列的列表（每列可能等长为 rows，或在 LPMS 引擎里 ragged）；
--   * 越界单元格一律读 0（对齐 TS 的 @m[r][c] ?? 0@）；
--   * 解析复用 BM-like 公共解析（与 C++ @bmlike::parse@ 同一实现）。
module OmegaY.Fam.Gms.Common
  ( FMat
  , Col
  , GmsM
  , throwGms
  , val
  , rowCount
  , cloneCol
  , clone
  , zeroCol
  , isZeroCol
  , addCols
  , addMat
  , scaleMat
  , colCompare
  , matCompare
  , suffixCompare
  , compareI
  , highestNonzeroRow
  , maxEntry
  , trimZeroRows
  , fmatToString
  , fromMatrix
  , parseBmExpr
  , kParseError
  , BmExpr (..)
  ) where

import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share (BmExpr (..), kParseError, parseBm)

type Col = [Int]
type FMat = [Col]

-- | GMS 内部错误统一坍缩为 @()@：边界处全部映射到 'kParseError'，
--   与 C++ @catch (const GmsError&) -> kParseError@ 一致。
type GmsM = Maybe

throwGms :: GmsM a
throwGms = Nothing

-- | 越界读 0。
val :: Col -> Int -> Int
val col r
  | r < 0 || r >= length col = 0
  | otherwise = col !! r

-- | C++ @row_count@：非空时初值为 1（quirk：全空列也按 1 行算）。
rowCount :: FMat -> Int
rowCount a
  | null a = 0
  | otherwise = foldr max 1 (map length a)

cloneCol :: Col -> Int -> Col
cloneCol col rows = [ val col r | r <- [0 .. rows - 1] ]

clone :: FMat -> Int -> FMat
clone a rows = [ cloneCol col rows | col <- a ]

zeroCol :: Int -> Col
zeroCol n = replicate n 0

isZeroCol :: Col -> Int -> Bool
isZeroCol col rows = all (\r -> val col r == 0) [0 .. rows - 1]

addCols :: Col -> Col -> Int -> Col
addCols a b rows = [ val a r + val b r | r <- [0 .. rows - 1] ]

-- | C++ @add_mat@：列数不一致时抛错。
addMat :: FMat -> FMat -> Int -> GmsM FMat
addMat a b rows
  | length a /= length b = throwGms
  | otherwise = Just [ addCols (a !! c) (b !! c) rows | c <- [0 .. length a - 1] ]

scaleMat :: FMat -> Int -> Int -> FMat
scaleMat a s rows = [ [ val col r * s | r <- [0 .. rows - 1] ] | col <- a ]

compareI :: Int -> Int -> Int
compareI x y = if x == y then 0 else if x < y then (-1) else 1

colCompare :: Col -> Col -> Int -> Int
colCompare a b rows = go 0
 where
  go r
    | r >= rows = 0
    | av /= bv = compareI av bv
    | otherwise = go (r + 1)
   where
    av = val a r
    bv = val b r

matCompare :: FMat -> FMat -> Int
matCompare a b = go 0
 where
  rows = max (rowCount a) (rowCount b)
  common = min (length a) (length b)
  go c
    | c < common =
        let cmp = colCompare (a !! c) (b !! c) rows
         in if cmp /= 0 then cmp else go (c + 1)
    | otherwise = compareI (length a) (length b)

suffixCompare :: Col -> Col -> Int -> Int -> Int
suffixCompare a b start rows = go start
 where
  go r
    | r >= rows = 0
    | av /= bv = compareI av bv
    | otherwise = go (r + 1)
   where
    av = val a r
    bv = val b r

highestNonzeroRow :: Col -> Int -> Int
highestNonzeroRow col rows = go (rows - 1)
 where
  go r
    | r < 0 = 0
    | val col r /= 0 = r + 1
    | otherwise = go (r - 1)

maxEntry :: FMat -> Int -> Int
maxEntry a rows = maximum (0 : [ val col r | col <- a, r <- [0 .. rows - 1] ])

trimZeroRows :: FMat -> FMat
trimZeroRows a
  | null a = []
  | otherwise =
      let rows = rowCount a
          kept = [ r | r <- [0 .. rows - 1], any (\col -> val col r /= 0) a ]
          kept' = if null kept then [0] else kept
       in [ [ val col r | r <- kept' ] | col <- a ]

-- | C++ @fmat_to_string@：每列截尾随 0，空列显示 @(0)@。
fmatToString :: FMat -> String
fmatToString a = concatMap fmt a
 where
  fmt col =
    let h = trimH (length col)
        trimH x = if x > 0 && col !! (x - 1) == 0 then trimH (x - 1) else x
     in '('
          : ( if h == 0 then "0" else joinVals [ show (col !! r) | r <- [0 .. h - 1] ] )
              ++ ")"
  joinVals [] = []
  joinVals [x] = x
  joinVals (x : xs) = x ++ "," ++ joinVals xs

-- | 解析后的矩形 Matrix -> 列向量表（与 C++ @from_matrix@ 同构）。
fromMatrix :: Matrix -> FMat
fromMatrix m =
  [ [ matAt m r c | r <- [0 .. matRows m - 1] ]
  | c <- [0 .. matCols m - 1]
  ]

parseBmExpr :: String -> Maybe BmExpr
parseBmExpr = parseBm
