{-# LANGUAGE OverloadedStrings #-}
-- | smnshare 完整移植（fill_ghost / extend / magma_indices / subtract_1 / clear_ghost）。
-- Expr 用 0-based value（与 MnUp2 一致）；C++ 是 1-based，本模块内部做转换。
module OmegaY.Fam.Upmn.SmnShare
  ( smnExpand
  ) where

import Data.List (foldl')
import Debug.Trace (trace)

type Sep = Int
type Entry = (Int, Sep)  -- (value 0-based, sep)
type Col = [Entry]
type Mountain = [Col]
type Vertical = [Sep]
type Pos = (Int, Int)

kUndefSep :: Sep
kUndefSep = 2147483647

-- vertical_increase: v = takeWhile (>= s) v, then snoc s
verticalIncrease :: Vertical -> Sep -> Vertical
verticalIncrease v s =
  let n = length v
      go i | i >= 1 && (v !! (i - 1)) < s = go (i - 1)
           | otherwise = take i v ++ [s]
   in go n

columnVerticals :: Col -> [Vertical]
columnVerticals c = scanl verticalIncrease [] (map snd c)

-- find_index_below: 二分查找第一个 Vi[j-1] < v 的位置
findIndexBelow :: [Vertical] -> Vertical -> Int
findIndexBelow vi v = go 0 (length vi)
  where
    go l r
      | l >= r = l
      | otherwise =
          let j = (l + r + 1) `div` 2
              vij = if j == 0 then [] else vi !! (j - 1)
           in if verticalCompare vij v < 0
                then go j r
                else go l (j - 1)

ord2Int :: Ordering -> Int
ord2Int LT = -1
ord2Int EQ = 0
ord2Int GT = 1

verticalCompare :: Vertical -> Vertical -> Int
verticalCompare a b = go 0
  where
    n0 = min (length a) (length b)
    go k
      | k >= n0 = ord2Int (compare (length a) (length b))
      | otherwise =
          let c = sepCompare (a !! k) (b !! k)
           in if c /= 0 then c else go (k + 1)

sepCompare :: Sep -> Sep -> Int
sepCompare a b
  | a == kUndefSep || b == kUndefSep =
      if a == b then 0 else if a == kUndefSep then 1 else -1
  | otherwise = ord2Int (compare a b)

-- parent: pi = value (0-based, since our Expr is already 0-based)
parent :: Mountain -> [[Vertical]] -> Pos -> Pos
parent m vv (i, j) =
  let value = fst (m !! i !! j)
      pi = value
      pj = if pi == -1 then 0 else findIndexBelow (vv !! pi) (vv !! i !! j)
   in (pi, pj)

magmaIndices :: Mountain -> [[Vertical]] -> Pos -> [[Int]] -> [[Int]]
magmaIndices m vv (ri, rj) initial = go (length m) initial
  where
    go n result
      | length result >= n = result
      | otherwise =
          let i = length result
              row = if i <= ri then [] else buildRow i
           in go n (result ++ [row])
    buildRow i = go 0 []
      where
        len = length (m !! i)
        go j acc
          | j >= len = acc
          | otherwise =
              let (pi, pj) = parent m vv (i, j)
               in if pi < ri
                    then acc  -- break
                    else if pi == ri
                      then go (j + 1) (acc ++ [if pj < rj then pj else rj])
                      else
                        let pj' = if pj == length (m !! pi) then pj - 1 else pj
                         in if pj' >= length (accOf pi)
                              then acc  -- break
                              else go (j + 1) (acc ++ [accOf pi !! pj'])
        accOf pi = magmaRows !! pi
    magmaRows = go (length m) initial

fillGhost :: Mountain -> Mountain
fillGhost m0 = result
  where
    m = m0
    vv = map columnVerticals m
    n = length m
    result = foldl' (processCol m vv) m [0 .. n - 1]

processCol :: Mountain -> [[Vertical]] -> Mountain -> Int -> Mountain
processCol m vv acc i =
  foldl' (processEntry m vv i) acc [0 .. length (m !! i) - 1]

processEntry :: Mountain -> [[Vertical]] -> Int -> Mountain -> Int -> Mountain
processEntry m vv i acc j =
  let (pi, pj) = parent m vv (i, j)
      parentColSize = length (m !! pi)
   in if pj /= parentColSize
        then acc
        else
          let vParent = if pj == 0 then [] else (vv !! pi) !! (pj - 1)
              v = (vv !! i) !! j
              sep = snd (m !! i !! j)
           in if verticalCompare (verticalIncrease vParent sep) v < 0
                then
                  let undefSep = if length v >= 2 then v !! (length v - 2) else kUndefSep
                      newParentCol = (acc !! pi) ++ [(-1, undefSep)]
                      acc' = take pi acc ++ [newParentCol] ++ drop (pi + 1) acc
                   in acc'
                else acc

clearGhost :: Mountain -> Mountain
clearGhost = map (filter (\(v, _) -> v /= -1))

subtract1 :: Mountain -> Mountain
subtract1 m0 = result
  where
    n = length m0
    right = n - 1
    rc = m0 !! right
    top = length rc - 1
    topSep = snd (rc !! top)
    vv = map columnVerticals m0
    (ri, rj) = parent m0 vv (right, top)
    result0 = m0
    newRc0 = init rc
    newRc =
      if topSep > 0
        then
          let newSep = topSep - 1
              vParent = if rj == 0 then [] else (vv !! ri) !! (rj - 1)
              vBottom = if top == 0 then [] else (vv !! right) !! (top - 1)
           in if verticalCompare (verticalIncrease vParent newSep) vBottom > 0
                then newRc0 ++ [(ri, newSep)]
                else newRc0
        else newRc0
    appended = drop rj (m0 !! ri)
    finalRc = newRc ++ appended
    result = take right result0 ++ [finalRc]

copyColumnSmn :: Col -> [Int] -> Col -> [Int] -> Pos -> Int -> Col
copyColumnSmn m0i mi0i mr mir (ri, rj) offset = result
  where
    n = length m0i
    result = loop 0 (-1) 0 []
    loop j lastMi refJ acc
      | j >= n = reverse acc
      | j >= length mi0i =
          let (v, sep) = m0i !! j
              v' = if v >= ri then v + offset else v
           in loop (j + 1) lastMi refJ ((v', sep) : acc)
      | otherwise =
          let (v, sep) = m0i !! j
              newVal = v + offset
              currentMi = mi0i !! j
           in if currentMi /= lastMi
                then
                  let (refJ', acc') = liftRows refJ currentMi newVal acc
                   in loop (j + 1) currentMi refJ' ((newVal, sep) : acc')
                else loop (j + 1) lastMi refJ ((newVal, sep) : acc)
    liftRows refJ currentMi newVal acc
      | refJ >= length mir = (refJ, acc)
      | mir !! refJ /= currentMi = (refJ, acc)
      | otherwise =
          let isRowLifting =
                  currentMi == rj ||
                  (refJ + 1 < length mir && mir !! (refJ + 1) == currentMi)
              acc' = if isRowLifting then ((newVal, snd (mr !! refJ)) : acc) else acc
           in liftRows (refJ + 1) currentMi newVal acc'

extend :: Mountain -> Mountain
extend m0 = result
  where
    n = length m0
    right = n - 1
    rc = m0 !! right
    top = length rc - 1
    vv0 = map columnVerticals m0
    p = parent m0 vv0 (right, top)
    mi0 = magmaIndices m0 vv0 p []
    m = subtract1 m0
    vv = take right vv0 ++ [columnVerticals (m !! right)]
    miPartial = take right mi0
    mi = magmaIndices m vv p miPartial
    offset = right - fst p
    result = foldl' addCol m [fst p + 1 .. right]
    addCol acc i =
      acc ++ [copyColumnSmn (m0 !! i) (mi0 !! i) (acc !! right) (mi !! right) p offset]

smnExpand :: Mountain -> Int -> Bool -> Mountain
smnExpand m index shorter
  | null m = m
  | null (last m) = init m
  | otherwise = final
  where
    current0 = fillGhost m
    current1 = foldl' (\acc _ -> extend acc) current0 [1 .. index]
    current2 = if shorter then init current1 else subtract1 current1
    final = clearGhost current2
