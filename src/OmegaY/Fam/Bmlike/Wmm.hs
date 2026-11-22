-- | wMM —— Weak Mutant Matrix。对齐 @Header/notation/wmm.hpp@。
module OmegaY.Fam.Bmlike.Wmm
  ( expandFs
  , expandAlter
  ) where

import qualified Data.Map.Strict as Map
import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share

expandFs :: String -> Int -> String
expandFs text term = run text term True

expandAlter :: String -> Int -> String
expandAlter text term = run text term False

run :: String -> Int -> Bool -> String
run text term shorter =
  case parseBm text of
    Nothing -> kParseError
    Just e
      | matCols m == 0 -> ""
      | not (isLimit m) -> matrixToBmString (dropLastColumn m)
      | otherwise ->
          let a = wmmExpand m term
              a' = if shorter then markLastNonexistent a else a
           in gridToString a'
     where
      m = beMat e

-- Grid：稀疏列网格

data Grid = Grid
  { gCols :: [[Int]]
  , gExists :: [Bool]
  } deriving (Eq, Show)

gridHas :: Grid -> Int -> Bool
gridHas g x = x >= 0 && x < length (gExists g) && (gExists g !! x)

gridAt :: Grid -> Int -> Int -> Int
gridAt g x y
  | not (gridHas g x) = 0
  | y < 0 || y >= length (gCols g !! x) = 0
  | otherwise = (gCols g !! x) !! y

gridColLen :: Grid -> Int -> Int
gridColLen g x = if gridHas g x then length (gCols g !! x) else 0

lastNonzero :: Grid -> Int -> Int
lastNonzero g x
  | not (gridHas g x) = (-1)
  | otherwise = go (length (gCols g !! x) - 1)
 where
  go y
    | y < 0 = (-1)
    | (gCols g !! x) !! y /= 0 = y
    | otherwise = go (y - 1)

ensure :: Int -> Grid -> Grid
ensure x g =
  let n = length (gCols g)
   in if x >= n
        then g { gCols = gCols g ++ replicate (x + 1 - n) []
               , gExists = gExists g ++ replicate (x + 1 - n) False
               }
        else g

setEmptyColumn :: Int -> Grid -> Grid
setEmptyColumn x g0 =
  let g = ensure x g0
   in g { gCols = take x (gCols g) ++ [[]] ++ drop (x + 1) (gCols g)
        , gExists = take x (gExists g) ++ [True] ++ drop (x + 1) (gExists g)
        }

assignColumn :: Int -> [Int] -> Grid -> Grid
assignColumn x v g0 =
  let g = ensure x g0
   in g { gCols = take x (gCols g) ++ [v] ++ drop (x + 1) (gCols g)
        , gExists = take x (gExists g) ++ [True] ++ drop (x + 1) (gExists g)
        }

truncateTrailingZeros :: Int -> Grid -> Grid
truncateTrailingZeros x g =
  let i = lastNonzero g x
      g0 = ensure x g
      col = take (i + 1) (gCols g0 !! x)
   in g0 { gCols = take x (gCols g0) ++ [col] ++ drop (x + 1) (gCols g0)
         , gExists = take x (gExists g0) ++ [True] ++ drop (x + 1) (gExists g0)
         }

-- vertical 序列

type Vert = [Int]

verticalCompare :: Vert -> Vert -> Int
verticalCompare a b =
  if length a /= length b
    then numberCompare (length a) (length b)
    else go (length a - 1)
 where
  go i
    | i < 0 = 0
    | otherwise =
        if a !! i /= b !! i
          then numberCompare (a !! i) (b !! i)
          else go (i - 1)

verticalIncrease :: Vert -> Int -> Vert
verticalIncrease y d =
  let c0 = if length y < d + 1 then y ++ replicate (d + 1 - length y) 0 else y
      c1 = replicate d 0 ++ drop d c0
      c2 = take d c1 ++ [if d < length y then y !! d + 1 else 1] ++ drop (d + 1) c1
   in c2

buildVertical :: Grid -> [[Vert]]
buildVertical g =
  [ if gridHas g x then vertCol x else []
  | x <- [0 .. length (gCols g) - 1] ]
 where
  vertCol x =
    let col = gCols g !! x
     in [ computeV x y | y <- [0 .. length col - 1] ]
  computeV x y =
    let i = goI (y - 1)
        base = if i >= 0 then buildVertical g !! x !! i else []
     in verticalIncrease base (y - i - 1)
   where
    goI i
      | i >= 0 && gridAt g x y == gridAt g x i = goI (i - 1)
      | otherwise = i

verticalAt :: Grid -> [[Vert]] -> Int -> Int -> Vert
verticalAt g vert x y =
  if x < length vert && y < length (vert !! x)
    then vert !! x !! y
    else
      let (ending, base) =
            if x < length vert
              then ( length (vert !! x) - 1
                   , if length (vert !! x) - 1 >= 0
                       then vert !! x !! (length (vert !! x) - 1)
                       else [] )
              else (0, [])
       in verticalIncrease base (y - ending - 1)

-- parent

data Cell = Cell !Int !Int
  deriving (Eq, Ord, Show)

cx :: Cell -> Int
cx (Cell x _) = x

cy :: Cell -> Int
cy (Cell _ y) = y

parentOn :: Grid -> [[Vert]] -> Cell -> Cell
parentOn g vert cur =
  if gridAt g (cx cur) (cy cur) == 0
    then Cell (-1) (cy cur)
    else
      let p = parentCheck g vert cur
       in if gridAt g (cx p) (cy p) == gridAt g (cx cur) (cy cur) - 1
            then p
            else parentOn g vert p
 where
  cx (Cell x _) = x
  cy (Cell _ y) = y

parentCheck :: Grid -> [[Vert]] -> Cell -> Cell
parentCheck g vert (Cell x y) =
  if y == 0
    then Cell (x - 1) 0
    else
      let p = cx (parentOn g vert (Cell x (y - 1)))
          target = gridAt g x y
          i0 = max y (gridColLen g p - 1)
          vTarget = verticalAt g vert x y
          i' = fix i0
          fix i
            | i < 0 = (-1)
            | gridAt g p i < target - 1
                || verticalCompare (verticalAt g vert p i) vTarget > 0 =
                fix (i - 1)
            | otherwise = i
       in Cell p i'

-- 主展开

wmmExpand :: Matrix -> Int -> Grid
wmmExpand input fsterm = result
 where
  rows = matRows input
  cols = matCols input
  m0 = Grid (matData input) (replicate cols True)

  lnzX = cols - 1
  lnzY = lastNonzero m0 lnzX
  lnz = gridAt m0 lnzX lnzY

  -- 收集末列各项沿父链到达 LNZ-1 的位置
  vertM = buildVertical m0
  collection0 = go [] (Cell lnzX lnzY)
   where
    go coll working =
      if cy working < 0
        then coll
        else
          let cell = walkToVal working
              coll' = insertFront cell coll
           in go coll' (Cell (cx cell) (cy cell - 1))
    insertFront (Cell x' y') coll =
      let padded = coll ++ replicate (x' - length coll + 1) []
       in take x' padded
            ++ [ y' : (padded !! x') ]
            ++ drop (x' + 1) padded
    walkToVal c =
      if gridAt m0 (cx c) (cy c) == lnz - 1
        then c
        else walkToVal (parentOn m0 vertM c)
  collection = collection0 ++ replicate (lnzX + 1 - length collection0) []
  collExists = map (not . null) collection
  counts = 1 : [ length (collection !! x)
               | x <- [0 .. lnzX]
               , collExists !! x ]
  columns = [ x | x <- [0 .. lnzX], collExists !! x ]
  r = length counts - 1

  root =
    if counts !! r == 1
      then parentOn m0 vertM (Cell lnzX lnzY)
      else Cell (last columns) (head (collection !! last columns))

  width = lnzX - cx root
  height = lnzY - cy root

  -- A = M 的副本
  a0 = m0
  a1 = assignColumn lnzX
         (take lnzY (gCols a0 !! lnzX)
            ++ [gridAt a0 lnzX lnzY - 1]
            ++ drop (lnzY + 1) (gCols a0 !! lnzX))
         a0
  -- 根列自 root.y 起的后缀拷到末列自 LNZy 起
  a2 = appendRootSuffix a1
   where
    appendRootSuffix g =
      let rootCol = gCols g !! cx root
          dyMax = length rootCol - cy root
          newLast = foldl appendY (gCols g !! lnzX) [0 .. dyMax - 1]
           where
            appendY col dy =
              let dst = lnzY + dy
                  v = gridAt m0 (cx root) (cy root + dy)
               in take dst col ++ [v] ++ drop (dst + 1) col
       in g { gCols = take lnzX (gCols g) ++ [newLast] ++ drop (lnzX + 1) (gCols g) }

  result = loopN a2 vertA 1
   where
    vertA = buildVertical a2

    loopN g vert n =
      if n > fsterm
        then g
        else
          let g1 = oneIter g vert n
           in loopN g1 (buildVertical g1) (n + 1)

    oneIter g vert n =
      let reference = referenceMap g vert n
       in foldl (addColumn vert n reference) g [1 .. width]

    referenceMap g vert n = ref
     where
      ref = go Map.empty 0 0
      go refMap y1 y2
        | y2 > cy root + height * n = refMap
        | otherwise =
            let cmp = verticalCompare
                        (verticalAt g vert (cx root) (y1 + 1))
                        (verticalAt g vert (cx root + width * n) y2)
             in if cmp > 0 || y1 >= cy root
                  then go (Map.insert y1 y2 refMap) y1 (y2 + 1)
                  else go refMap (y1 + 1) y2

    refGet ref y = Map.findWithDefault (-1) y ref

    addColumn vert n reference g dx =
      let x = cx root + dx
          targetX = x + width * n
          g0 = setEmptyColumn targetX g
          src = gCols g !! x
          (_, col) = foldl (pushRow vert n reference g x) ((-1), []) (zip src [0 ..])
       in g0 { gCols = take targetX (gCols g0) ++ [col]
                      ++ drop (targetX + 1) (gCols g0) }

    pushRow vert n reference g x (lastmagma, acc) (val, y) =
      let a = ascGet g vert (Cell x y)
       in if a >= 0
            then
              if a <= cy root
                   && verticalCompare (verticalAt g vert (cx root) a)
                                      (verticalAt g vert x y) <= 0
                then
                  let jlo = refGet reference (a - 1) + 1
                      jhi = refGet reference a
                      newVals =
                        [ val - gridAt g (cx root) a
                            + gridAt g (cx root + width * n) j
                        | j <- [jlo .. jhi] ]
                   in (a, acc ++ newVals)
                else
                  let baseY = if lastmagma >= 0 then lastmagma else 0
                      v = if lastmagma >= 0
                            then val - gridAt g (cx root) lastmagma
                                   + gridAt g (cx root + width * n)
                                       (refGet reference lastmagma)
                            else val - gridAt g (cx root) 0
                                   + gridAt g (cx root + width * n) 0
                   in (lastmagma, acc ++ [v])
            else (lastmagma, acc ++ [val])

    ascGet g vert = ascendMemo Map.empty
     where
      ascendMemo memo cell =
        case Map.lookup cell memo of
          Just v -> v
          Nothing ->
            let v = if cx cell < cx root
                      then (-1)
                      else if cx cell == cx root
                        then cy cell
                        else ascendMemo' (Map.insert cell 0 memo)
                               (parentOn g vert cell)
            in v
      ascendMemo' memo p =
        case Map.lookup p memo of
          Just v -> v
          Nothing ->
            let v = if cx p < cx root
                      then (-1)
                      else if cx p == cx root
                        then cy p
                        else ascendMemo' (Map.insert p 0 memo)
                               (parentOn g vert p)
            in v

markLastNonexistent :: Grid -> Grid
markLastNonexistent g =
  let last = go (length (gExists g) - 1)
   in if last < 0
        then g
        else g { gExists = take last (gExists g) ++ [False]
                           ++ drop (last + 1) (gExists g) }
 where
  go x
    | x < 0 = (-1)
    | gExists g !! x = x
    | otherwise = go (x - 1)

gridToString :: Grid -> String
gridToString g0 =
  let g = foldl (\gg x -> truncateTrailingZeros x gg) g0
            [x | x <- [0 .. length (gExists g0) - 1], gExists g0 !! x]
   in concat
        [ let col = gCols g !! x
              h = goH col (length col)
           in '(' : (if h == 0 then "0"
                       else joinComma [ show (col !! y) | y <- [0 .. h - 1] ])
                ++ ")"
        | x <- [0 .. length (gExists g) - 1]
        , gExists g !! x ]
 where
  goH col h = if h > 0 && col !! (h - 1) == 0 then goH col (h - 1) else h
  joinComma [] = []
  joinComma [x] = x
  joinComma (x : xs) = x ++ "," ++ joinComma xs
