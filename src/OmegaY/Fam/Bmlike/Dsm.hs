-- | DSM —— Diagonal Sudden Matrix。对齐 @Header/notation/dsm.hpp@。
module OmegaY.Fam.Bmlike.Dsm
  ( expandFs
  , expandAlter
  ) where

import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share

-- | 对外入口。
expandFs :: String -> Int -> String
expandFs text term = run text term True

expandAlter :: String -> Int -> String
expandAlter text term = run text term False

run :: String -> Int -> Bool -> String
run text term shorter =
  case parseBm text of
    Nothing -> kParseError
    Just e
      | beInfinity e -> matrixToBmString (triangularInfinityFs term)
      | matCols m == 0 -> ""
      | not (isLimit m) -> matrixToBmString (dropLastColumn m)
      | otherwise ->
          let longer = expandNormal (toCols m) term
           in colsToString
                (if shorter && not (null longer) then init longer else longer)
     where
      m = beMat e

-- ---------------------------------------------------------------------
-- 基础工具
-- ---------------------------------------------------------------------

type Cols = [[Int]]

vAt :: Cols -> Int -> Int -> Int
vAt m r c
  | c < 0 || c >= length m = 0
  | r < 0 || r >= length (m !! c) = 0
  | otherwise = (m !! c) !! r

type PRM = [[Int]]

pAt :: PRM -> Int -> Int -> Int
pAt p r c
  | r < 0 || r >= length p = (-1)
  | c < 0 || c >= length (p !! r) = (-1)
  | otherwise = (p !! r) !! c

data Position = Position !Int !Int
  deriving (Eq, Show)

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- | get_predecessor。
getPredecessor :: PRM -> Int -> Int -> Maybe Position
getPredecessor prm r c
  | pAt prm r c /= (-1) || r == 0 = Nothing
  | otherwise =
      let upRow = r - 1
          go curr =
            if curr == (-1)
              then Just (Position upRow c)
              else
                let next = pAt prm upRow curr
                 in if next == (-1)
                      then Just (Position upRow curr)
                      else
                        if pAt prm upRow curr /= (-1) && pAt prm r curr == (-1)
                          then Just (Position r curr)
                          else go next
       in go (pAt prm upRow c)

-- | construct_matrix_values：列优先父项矩阵 → 数值矩阵。
constructMatrixValues :: [[Int]] -> Cols
constructMatrixValues pcm = result
 where
  cols = length pcm
  rows = if cols > 0 then length (pcm !! 0) else 0
  result = [ col c | c <- [0 .. cols - 1] ]
  col c =
    [ let p = pcm !! c !! r
       in if p == (-1) then 0 else (result !! p !! r) + 1
    | r <- [0 .. rows - 1] ]

toRowMajor :: [[Int]] -> PRM
toRowMajor pcm =
  let cols = length pcm
      rows = if cols > 0 then length (pcm !! 0) else 0
   in [ [ pcm !! c !! r | c <- [0 .. cols - 1] ] | r <- [0 .. rows - 1] ]

-- ---------------------------------------------------------------------
-- generate_expansion
-- ---------------------------------------------------------------------

generateExpansion :: [[Int]] -> Int -> Int -> Int -> Bool -> [[Int]]
generateExpansion pcm badRow badCol times strong = finalCm
 where
  rows = length (pcm !! 0)
  cols = length pcm
  lastCol = cols - 1
  s = badCol
  e = lastCol
  segDist = e - s
  parents = toRowMajor pcm

  targetRow = goT (rows - 1)
   where
    goT r
      | r < 0 = (-1)
      | pcm !! lastCol !! r /= (-1) = r
      | otherwise = goT (r - 1)

  finalCm
    | targetRow == badRow = smallExpansion
    | otherwise = fullExpansion

  -- ---------------- 小展开 ----------------
  smallExpansion =
    let expanded0 = [ [ parents !! r !! c | c <- [0 .. cols - 1] ]
                    | r <- [0 .. rows - 1] ]
        expandedF = foldl addIter expanded0 [1 .. times]
        width = maximum (0 : map length expandedF)
        padded = [ take width (expandedF !! r ++ repeat (-1))
                 | r <- [0 .. rows - 1] ]
     in [ [ padded !! r !! c | r <- [0 .. rows - 1] ]
        | c <- [0 .. width - 1] ]
   where
    addIter curRows i =
      let shift = i * segDist
       in [ foldl (setCell r shift) (curRows !! r) [s .. e]
          | r <- [0 .. rows - 1] ]
    setCell r shift row c =
      let newC = c + shift
          orig = parents !! r !! c
          np
            | c == s && r < targetRow = parents !! r !! e + shift - segDist
            | orig >= badCol = orig + shift
            | otherwise = orig
          padded = take (newC + 1) (row ++ repeat (-1))
       in replaceAt newC np padded

  -- ---------------- 完整展开 ----------------
  fullExpansion =
    let result0 = [ [ parents !! r !! c | c <- [0 .. cols - 1] ]
                  | r <- [0 .. rows - 1] ]
        result1 = foldl addCols result0 [1 .. times]
        currentCols = if null result1 then 0 else length (result1 !! 0)

        rBound = targetRow - badRow
        cBound = lastCol - badCol
        finalRows = rows + rBound * times

        padded =
          [ take currentCols (result1 !! r ++ repeat (-1))
          | r <- [0 .. rows - 1] ]
            ++ replicate (max 0 (finalRows - rows))
                 (replicate currentCols (-1))

        risingSet = closureRising
        baseSet = closureBase

        written = foldl (writeIter risingSet baseSet) padded [1 .. times]

        width = if null written then 0 else length (written !! 0)
     in [ [ written !! r !! c | r <- [0 .. finalRows - 1] ]
        | c <- [0 .. width - 1] ]
   where
    addCols curRows i =
      let shift = i * segDist
       in [ curRows !! r
              ++ [ if r == targetRow && c == e
                     then (-1)
                     else let orig = parents !! r !! c
                           in if orig >= badCol then orig + shift else orig
                 | c <- [s + 1 .. e] ]
          | r <- [0 .. rows - 1] ]

    isSet set (r, c) = (r, c) `elem` set

    becomeRising set r c =
      let lp = if strong then lpLc c else (-1)
          pp = parents !! r !! c
          up = if r > badRow then parents !! (r - 1) !! c else (-1)
          down = if r < rows - 1 then parents !! (r + 1) !! c else (-1)
       in (r == badRow && strong && lp /= (-1) && isSet set (r, lp))
            || (pp /= (-1) && isSet set (r, pp))
            || (r > badRow && up /= (-1) && isSet set (r - 1, up))
            || (r < rows - 1 && isSet set (r + 1, c))
     where
      lpLc c = if badRow > 0 then parents !! (badRow - 1) !! c else c - 1

    closureRising = go [(badRow, badCol)]
     where
      go front =
        let next =
              [ (r, c)
              | r <- [badRow .. rows - 1]
              , c <- [0 .. cols - 1]
              , (r, c) `notElem` front
              , becomeRising front r c
              ]
         in if null next then front else go (front ++ next)

    lpOf c = if badRow > 0 then parents !! (badRow - 1) !! c else c - 1

    baseInitial =
      [ c
      | c <- [0 .. cols - 1]
      , (strong && lpOf c == badCol)
          || (not strong && parents !! badRow !! c == badCol)
      ]

    closureBase = go baseInitial
     where
      go queue =
        let next =
              [ c
              | c <- [0 .. cols - 1]
              , c `notElem` queue
              , any (\cp -> hit c cp) queue
              ]
         in if null next then queue else go (queue ++ next)
      hit c cp
        | strong = lpOf c == cp
        | otherwise = parents !! badRow !! c == cp

    updateCell rowsMap r c v =
      [ if rr == r
          then replaceAt c v (rowsMap !! rr)
          else rowsMap !! rr
      | rr <- [0 .. length rowsMap - 1] ]

    writeIter risingSet baseSet rowsMap i =
      let rowShift = (targetRow - badRow) * i
          colShift = (lastCol - badCol) * i
          rows1 = foldl (writeRising rowShift colShift) rowsMap
                    [ (r, c)
                    | r <- [0 .. rows - 1]
                    , c <- [0 .. cols - 1]
                    ]
          rows2 = foldl (writeBase rowShift colShift) rows1 baseSet
       in rows2
     where
      writeRising rowShift colShift rowsMap (r, c) =
        if (r, c) `elem` risingSet
          then
            let nr = r + rowShift
                nc = c + colShift
                val = parents !! r !! c
                val' = if val /= (-1) then val + colShift else (-1)
             in updateCell rowsMap nr nc val'
          else rowsMap
      writeBase rowShift colShift rowsMap c =
        let nc = c + colShift
            baseParent =
              if strong
                then lpOf c
                else parents !! badRow !! c
            nbp = if baseParent /= (-1) then baseParent + colShift else (-1)
         in foldl (\rs k -> updateCell rs (badRow + k) nc nbp)
                  rowsMap [0 .. rowShift - 1]

-- ---------------------------------------------------------------------
-- cols_compare
-- ---------------------------------------------------------------------

colsCompare :: Cols -> Cols -> Int
colsCompare a b = goC 0
 where
  n = min (length a) (length b)
  goC c
    | c < n =
        let ha = normH (a !! c)
            hb = normH (b !! c)
            k = min ha hb
            cmp = goY c k 0
         in if cmp /= 0
              then cmp
              else if ha /= hb then numberCompare ha hb else goC (c + 1)
    | otherwise = numberCompare (length a) (length b)
   where
    goY c k y
      | y < k =
          let x = (a !! c) !! y
              yv = (b !! c) !! y
           in if x /= yv then numberCompare x yv else goY c k (y + 1)
      | otherwise = 0
  normH col = goH (length col)
   where
    goH h = if h > 0 && col !! (h - 1) == 0 then goH (h - 1) else h

-- ---------------------------------------------------------------------
-- get_bad_item_info
-- ---------------------------------------------------------------------

data BadInfo = BadInfo !Int !Int [[Int]]

getBadItemInfo :: Cols -> Maybe BadInfo
getBadItemInfo matrix
  | null matrix = Nothing
  | otherwise =
      let cols = length matrix
          rows = maximum (0 : map length matrix)
          mrm = [ [ vAt matrix r c | c <- [0 .. cols - 1] ]
                | r <- [0 .. rows - 1] ]
          prm = buildPrm mrm rows cols
          targetRow = goT prm cols (rows - 1)
          targetCol = cols - 1
       in if targetRow < 0
            then Nothing
            else
              let parentCol = prm !! targetRow !! targetCol
               in if parentCol < 0
                    then Nothing
                    else Just (buildBad prm rows cols targetRow parentCol)
 where
  buildPrm mrm rows cols = prm
   where
    prm = row0 : [ fillRow r | r <- [1 .. rows - 1] ]
    row0 = [ if c == 0 then (-1) else findK c (mrm !! 0 !! c)
           | c <- [0 .. cols - 1] ]
    findK c val = goK (c - 1)
     where
      goK k
        | k < 0 = (-1)
        | mrm !! 0 !! k < val = k
        | otherwise = goK (k - 1)
    fillRow r =
      [ goChain c c
      | c <- [0 .. cols - 1] ]
     where
      val c = mrm !! r !! c
      goChain chain c
        | chain == (-1) = (-1)
        | chain /= c && mrm !! r !! chain < val c = chain
        | otherwise =
            let next = if chain /= (-1) then prm !! (r - 1) !! chain else (-1)
             in if next == (-1) then (-1) else goChain next c

  goT prm cols r
    | r < 0 = (-1)
    | otherwise =
        let targetCol = cols - 1
         in if prm !! r !! targetCol /= (-1) then r else goT prm cols (r - 1)

  buildBad prm rows cols targetRow parentCol = badInfo
   where
    targetCol = cols - 1
    curr0 = Position targetRow parentCol

    pool0 = curr0 : unfoldPred curr0
     where
      unfoldPred curr =
        case getPredecessor prm (prR curr) (prC curr) of
          Just p -> p : unfoldPred p
          Nothing -> []
    prR (Position r _) = r
    prC (Position _ c) = c

    pool = reverse (sortByC pool0)
    sortByC = foldr ins []
     where
      ins p [] = [p]
      ins p (q : qs) =
        if prC p >= prC q then p : q : qs else q : ins p qs

    (options, _) = foldl step ([curr0], curr0) pool
     where
      step (opts, prev) item =
        if prR item < prR prev
          then (opts ++ [item], item)
          else (opts, prev)

    pcm = [ [ prm !! r !! c | r <- [0 .. rows - 1] ] | c <- [0 .. cols - 1] ]
    standardSeg =
      constructMatrixValues (generateExpansion pcm targetRow parentCol 1 True)

    badInfo = BadInfo (prR chosen) (prC chosen) pcm
     where
      chosen = findChosen pool
      findChosen [] = last options
      findChosen (cand : rest) =
        let candSeg =
              constructMatrixValues
                (generateExpansion pcm (prR cand) (prC cand) 1 True)
         in if colsCompare candSeg standardSeg < 0
              then
                let matches =
                      sortByC [ opt
                              | opt <- options
                              , prC opt > prC cand ]
                 in case matches of
                      [] -> last options
                      (m : _) -> m
              else findChosen rest

-- ---------------------------------------------------------------------
-- normalize / serialize / expand_normal
-- ---------------------------------------------------------------------

normalizeCols :: Cols -> Cols
normalizeCols = map trim
 where
  trim col = take (go (length col)) col
   where
    go h = if h > 0 && col !! (h - 1) == 0 then go (h - 1) else h

colsToString :: Cols -> String
colsToString = concatMap fmt
 where
  fmt col =
    let h = go (length col)
     in '(' : (if h == 0 then "0" else joinComma [ show (col !! y) | y <- [0 .. h - 1] ])
          ++ ")"
   where
    go h = if h > 0 && col !! (h - 1) == 0 then go (h - 1) else h
    joinComma [] = []
    joinComma [x] = x
    joinComma (x : xs) = x ++ "," ++ joinComma xs

toCols :: Matrix -> Cols
toCols m =
  [ [ matAt m r c | r <- [0 .. matRows m - 1] ]
  | c <- [0 .. matCols m - 1] ]

expandNormal :: Cols -> Int -> Cols
expandNormal matrix times
  | null matrix = []
  | all (== 0) (last matrix) = take (length matrix - 1) matrix
  | otherwise =
      case getBadItemInfo matrix of
        Nothing -> []
        Just (BadInfo br bc pcm) ->
          let full =
                constructMatrixValues (generateExpansion pcm br bc times False)
              full1 = if null full then [] else init full
           in normalizeCols full1
