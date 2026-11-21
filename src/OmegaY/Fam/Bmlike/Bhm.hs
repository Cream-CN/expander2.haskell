-- | BHM 与 nBM-BHM。对齐 @Header/notation/bhm.hpp@。
module OmegaY.Fam.Bmlike.Bhm
  ( expandFs       -- ^ BHM FS
  , expandAlter    -- ^ BHM FS_alter
  , expandNFs      -- ^ nBM-BHM FS（参数 n）
  , expandNAlter   -- ^ nBM-BHM FS_alter（参数 n）
  ) where

import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share

-- | FS 公共文本封装（对齐 @bmlike::expand_text_with@）。
fsWith :: String -> Int -> Bool -> (Matrix -> Int -> Bool -> Matrix) -> String
fsWith text term shorter core =
  case parseBm text of
    Nothing -> kParseError
    Just e
      | matCols m == 0 -> ""
      | not (isLimit m) -> matrixToBmString (dropLastColumn m)
      | otherwise -> matrixToBmString (normalizeMatrix (core m term shorter))
     where
      m = beMat e

expandFs :: String -> Int -> String
expandFs text term =
  fsWith text term True (\m idx _ -> bhmExpand m idx True)

expandAlter :: String -> Int -> String
expandAlter text term =
  fsWith text term False (\m idx _ -> bhmExpand m idx False)

expandNFs :: Int -> String -> Int -> String
expandNFs n text term =
  fsWith text term True (\m idx _ -> bmBhmExpand m idx n True)

expandNAlter :: Int -> String -> Int -> String
expandNAlter n text term =
  fsWith text term False (\m idx _ -> bmBhmExpand m idx n False)

-- ---------------------------------------------------------------------

-- | 把（可能不等长）列打包为规范化矩形。
packColumns :: [[Int]] -> Matrix
packColumns cols =
  let rows = maximum (0 : map length cols)
   in normalizeMatrix
        (Matrix rows (length cols)
           [ take rows (c ++ repeat 0) | c <- cols ])

-- | ascension_thresholds。
ascensionThresholds :: [[Int]] -> Int -> [Int] -> Int -> [Int]
ascensionThresholds pTab r roots b = [ result i | i <- [0 .. nc - 1] ]
 where
  nc = length pTab
  result i
    | i == r = b
    | i `elem` roots = b
    | otherwise =
        let threshold = go 0
         in threshold
   where
    go t
      | t < length (pTab !! i) && t < b =
          let pp = parentAtOf pTab i t
           in if pp < 0 || t >= resultAt pp
                then t
                else go (t + 1)
      | otherwise = t
    resultAt pp = ascensionThresholds pTab r roots b !! pp  -- 递归（同 C++ result[pp]）

-- | ascension_vector：末列相对根列 r 前 b 行的差。
ascensionVector :: Matrix -> Int -> Int -> [Int]
ascensionVector m r b =
  let right = matCols m - 1
   in [ matAt m y right - bmAt m y r | y <- [0 .. b - 1] ]

-- | ascend_vector：列 + w·V（仅前 A 行）。
ascendVector :: Matrix -> Int -> [Int] -> Int -> Int -> [Int]
ascendVector m col v a w =
  let rows = matRows m
      len = max rows a
   in [ let base = bmAt m y col
         in if y < a
              then base + w * (if y < length v then v !! y else 0)
              else base
      | y <- [0 .. len - 1]
      ]

-- | compute_expansion。
computeExpansion :: Matrix -> Int -> [Int] -> [Int] -> Int -> Bool -> Matrix
computeExpansion m r v a index shorter =
  packColumns cols
 where
  rows = matRows m
  right = matCols m - 1
  baseCols = [ [ matAt m y c | y <- [0 .. rows - 1] ] | c <- [0 .. right - 1] ]
  added =
    [ ascendVector m i v (a !! i) w
    | w <- [1 .. index + 1]
    , not (shorter && w > index)
    , i <- [r .. right - 1]
    , not (w > index) || i == r
    ]
  cols = baseCols ++ added

-- | extend：一次较短展开后再追加提升后的末列。
extend :: Matrix -> Int -> [Int] -> [Int] -> Matrix
extend m r v a =
  let base = computeExpansion m r v a 1 True
      cols = [ normalizedColumn base c | c <- [0 .. matCols base - 1] ]
      lastCol = ascendVector m (matCols m - 1) v (a !! (matCols m - 1)) 1
   in packColumns (cols ++ [lastCol])

-- | BHM.ts BHM_expand。
bhmExpand :: Matrix -> Int -> Bool -> Matrix
bhmExpand input index shorter =
  let m = standardizeMatrix 0 input
      nc = matCols m
   in if nc == 0
        then m
        else body m index shorter

body :: Matrix -> Int -> Bool -> Matrix
body m index shorter = result
 where
  rows = matRows m
  right = matCols m - 1
  top = rows - 1
  pTab = parentsOf m
  pr = parentAtOf pTab right top
  specialRoot = if pr < 0 then (-1) else parentAtOf pTab pr top

  rawRoots = takeWhile (\i -> i > specialRoot) (goR right)
   where
    goR i = i : goR (if top > 0 then parentAtOf pTab i (top - 1) else i - 1)
  roots =
    [ i
    | i <- rawRoots
    , parentAtOf pTab i top == specialRoot
    ]

  result
    | top < 0 = dropLastColumn m
    | null roots = dropLastColumn m
    | otherwise = final

  aMap r = ascensionThresholds pTab r roots top
  vMap r = ascensionVector m r top
  threshold = extend m (head roots) (vMap (head roots)) (aMap (head roots))

  final =
    let chosen = pick 0
        rActual = roots !! (chosen - 1)
     in computeExpansion m rActual (vMap rActual) (aMap rActual) index shorter

  pick i
    | i >= length roots = length roots
    | otherwise =
        let r = roots !! i
            cand = extend m r (vMap r) (aMap r)
         in if compareMat cand threshold < 0
              then i
              else pick (i + 1)

-- | BM_BHM_expand：顶行小于 n 走 BM4，否则走 BHM。
bmBhmExpand :: Matrix -> Int -> Int -> Bool -> Matrix
bmBhmExpand input index n shorter =
  let m = standardizeMatrix 0 input
   in if matCols m == 0
        then m
        else
          let top = matRows m - 1
           in if top < 0
                then dropLastColumn m
                else
                  if top < n
                    then bm4Expand m index shorter
                    else bhmExpand m index shorter
