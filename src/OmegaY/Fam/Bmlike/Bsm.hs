-- | BSM —— Bashicu Sudden Matrix。对齐 @Header/notation/bsm.hpp@。
module OmegaY.Fam.Bmlike.Bsm
  ( expandText
  ) where

import qualified Data.Map.Strict as Map
import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share

-- | 对外入口：BSMNotation::expand_text。
expandText :: String -> Int -> String
expandText text term =
  case parseBm text of
    Nothing -> kParseError
    Just e
      | matCols (beMat e) == 0 -> ""
      | otherwise ->
          matrixToBmString
            (bsmExpand (beMat e) (if term < 1 then 1 else term))

-- ---------------------------------------------------------------------

type AscMem = Map.Map (Int, Int) Bool

-- | ascending(r,x,y)，对齐 BSM.ts（memo 以 (x,y) 为键）。
ascending :: [[Int]] -> [Int] -> AscMem -> Int -> Int -> Int
          -> (AscMem, Bool)
ascending pTab roots mem r x y
  | x < r || x < 0 = (mem, False)
  | otherwise =
      case Map.lookup (x, y) mem of
        Just v -> (mem, v)
        Nothing ->
          let (mem1, v)
                | x `elem` roots = (mem, True)
                | otherwise =
                    let p = parentAtOf pTab x y
                     in ascending pTab roots mem r p y
           in (Map.insert (x, y) v mem1, v)

-- | delta：末列相对根列 r 的增量；LNZ 行额外减 1。
delta :: Matrix -> Int -> Int -> Int -> [Int]
delta m endCol r lnz =
  [ if y < lnz
      then matAt m y endCol - bmAt m y r
      else if y == lnz
             then matAt m y endCol - bmAt m y r - 1
             else 0
  | y <- [0 .. matRows m - 1]
  ]

-- | 复制 [r, end_col) 段 n 次，第 a 段按 a*delta 仅在 ascending 行上加。
expansion :: Matrix -> Int -> Int -> Int -> [Int] -> [[Int]] -> [Int]
          -> Matrix
expansion m r n endCol del pTab roots =
  Matrix rows (endCol + n * seg) (baseCols ++ addedCols)
 where
  rows = matRows m
  seg = endCol - r
  baseCols =
    [ [ matAt m y c | y <- [0 .. rows - 1] ] | c <- [0 .. endCol - 1] ]
  addedCols = snd (foldl step (Map.empty, []) pairs)
   where
    pairs = [ (a, x) | a <- [1 .. n], x <- [r .. endCol - 1] ]
    step (memo, cols) (a, x) =
      let (memo', col) = buildCol memo a x
       in (memo', cols ++ [col])
    buildCol memo a x =
      foldl stepY (memo, []) [0 .. rows - 1]
     where
      stepY (mm, ys) y =
        let (mm', v) = ascending pTab roots mm r x y
         in (mm', ys ++ [matAt m y x + (if v then a * (del !! y) else 0)])

-- | expansion(r,1) 后再追加提升后的末列。
expansionAppend :: Matrix -> Int -> Int -> [Int] -> [[Int]] -> [Int]
                -> Matrix
expansionAppend m r endCol del pTab roots =
  Matrix rows (baseColsCount + 1) (baseCols ++ [lastCol])
 where
  rows = matRows m
  base = expansion m r 1 endCol del pTab roots
  baseCols = matData base
  baseColsCount = length baseCols
  lastCol =
    snd (foldl stepY (Map.empty, []) [0 .. rows - 1])
   where
    stepY (mm, ys) y =
      let (mm', v) = ascending pTab roots mm r endCol y
       in (mm', ys ++ [matAt m y endCol + (if v then del !! y else 0)])

-- | BSM.ts expand。
bsmExpand :: Matrix -> Int -> Matrix
bsmExpand input term =
  let m = standardizeMatrix 0 input
      nc = matCols m
   in if nc == 0
        then m
        else expandBody m term

expandBody :: Matrix -> Int -> Matrix
expandBody m term = result
 where
  rows = matRows m
  endCol = matCols m - 1
  yMax = rows - 1
  pTab = parentsOf m
  prefix = Matrix rows endCol
    [ [ matAt m y c | y <- [0 .. rows - 1] ] | c <- [0 .. endCol - 1] ]

  lnz = lastNonzero endCol
  lastNonzero c = go (rows - 1)
   where
    go r
      | r < 0 = (-1)
      | matAt m r c > 0 = r
      | otherwise = go (r - 1)

  -- special_roots：末列 LNZ 行父项链（含末端 -1）。
  specialRoots = go endCol
   where
    go cur =
      let p = parentAtOf pTab cur lnz
       in p : (if p < 0 then [] else go p)

  -- roots：从 special_roots[0] 沿 LNZ-1 行回溯。
  headSpecial = if null specialRoots then (-1) else head specialRoots
  rawRoots = takeWhile (/= (-1)) (goR headSpecial)
   where
    goR cur = cur : goR (if lnz > 0 then parentAtOf pTab cur (lnz - 1) else cur - 1)
  roots =
    [ cur
    | cur <- rawRoots
    , parentAtOf pTab cur lnz `elem` specialRoots
    ]

  result
    | lnz < 0 = prefix
    | null roots = prefix
    | otherwise = pickAndExpand

  r0 = head roots
  del0 = delta m endCol r0 lnz
  threshold = expansionAppend m r0 endCol del0 pTab roots
  testRoot = [ bmAt m y r0 | y <- [lnz + 1 .. rows - 1] ]

  pickAndExpand =
    let chosen = pickRoot 0
        rActual = roots !! (chosen - 1)
        del = delta m endCol rActual lnz
        res = expansion m rActual term endCol del pTab roots
        res1 =
          if yMax > 0 && allZeroRow res yMax
            then Matrix yMax (matCols res) [ take yMax col | col <- matData res ]
            else res
     in normalizeMatrix res1

  allZeroRow res r = all (== 0) [ matAt res r c | c <- [0 .. matCols res - 1] ]

  pickRoot i
    | i >= length roots = length roots
    | otherwise =
        let r = roots !! i
            smaller
              | r `elem` specialRoots =
                  any
                    ( \dy -> bmAt m (lnz + 1 + dy) r /= (testRoot !! dy) )
                    [0 .. length testRoot - 1]
              | otherwise =
                  let delr = delta m endCol r lnz
                      cand = expansionAppend m r endCol delr pTab roots
                   in compareMat cand threshold < 0
         in if smaller then i else pickRoot (i + 1)
