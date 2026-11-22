-- | 列主序整数矩阵，语义对齐 C++ 侧 omegay::common::Matrix。
module OmegaY.Matrix
  ( Matrix (..)
  , emptyMatrix
  , buildMatrix
  , fromColumns
  , toColumns
  , matRows
  , matCols
  , matAt
  , matSet
  , matResizeCols
  , matPushCol
  , matEmpty
  ) where

import Prelude

data Matrix = Matrix
  { matRows :: !Int
  , matCols :: !Int
  , matData :: [[Int]]
  } deriving (Eq, Show)

emptyMatrix :: Matrix
emptyMatrix = Matrix 0 0 []

-- | 构造矩阵：@buildMatrix rows cols f@ 中元素 @(r,c)@ 取 @f r c@。
buildMatrix :: Int -> Int -> (Int -> Int -> Int) -> Matrix
buildMatrix rows cols f
  | rows <= 0 || cols <= 0 = Matrix (max 0 rows) 0 []
  | otherwise =
      Matrix rows cols [ [ f r c | r <- [0 .. rows - 1] ] | c <- [0 .. cols - 1] ]

-- | 由列列表构造：行数取最长列，不足者补 0（对齐 BM-likes 的 @normalize@ 用法）。
fromColumns :: [[Int]] -> Matrix
fromColumns cols0 =
  let rows = maximum (0 : map length cols0)
      cols = [ take rows (c ++ repeat 0) | c <- cols0 ]
   in Matrix rows (length cols) cols

toColumns :: Matrix -> [[Int]]
toColumns = matData

-- | 越界读到 0。
matAt :: Matrix -> Int -> Int -> Int
matAt m r c
  | r < 0 || c < 0 || r >= matRows m || c >= matCols m = 0
  | otherwise =
      let col = matData m !! c
          len = length col
       in if r < len then col !! r else 0

-- | 修改单个元素（越界忽略）。
matSet :: Matrix -> Int -> Int -> Int -> Matrix
matSet m r c v
  | r < 0 || c < 0 || r >= matRows m || c >= matCols m = m
  | otherwise =
      let before = take c (matData m)
          col = matData m !! c
          after = drop (c + 1) (matData m)
          col' = take r col ++ [v] ++ drop (r + 1) col
       in m { matData = before ++ [col'] ++ after }

matResizeCols :: Int -> Matrix -> Matrix
matResizeCols newCols m
  | newCols < 0 = m
  | newCols == matCols m = m
  | otherwise =
      let rows = matRows m
          keep = min newCols (matCols m)
          kept = take keep (matData m)
          pad = replicate (newCols - keep) (replicate rows 0)
       in Matrix rows newCols (kept ++ pad)

-- | 追加一列（长度不足补 0，超出截断）。
matPushCol :: [Int] -> Matrix -> Matrix
matPushCol values m =
  let rows = matRows m
      col = take rows (values ++ repeat 0)
   in Matrix rows (matCols m + 1) (matData m ++ [col])

matEmpty :: Matrix -> Bool
matEmpty m = matRows m == 0 || matCols m == 0
