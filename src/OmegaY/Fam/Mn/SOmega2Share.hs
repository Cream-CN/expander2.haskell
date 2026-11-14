-- | s_omega2_mn 共享模块：Sep 为 [Int]（多维分隔符），对齐 C++ s_omega2_mn.hpp。
module OmegaY.Fam.Mn.SOmega2Share
  ( Sep
  , Entry(..)
  , Column
  , Mountain
  , Vertical
  , Position(..)
  , StretchData(..)
  -- * 比较
  , numCompare
  , sepCompare
  , verticalCompare
  , entryCompare
  , columnCompare
  , mountainCompare
  -- * 显示
  , sepDisplay
  , entryDisplay
  , columnDisplay
  , mountainDisplay
  -- * 解析
  , fromDisplay
  -- * 山结构辅助
  , sepIsOne
  , sepDimension
  , sepAdd
  , sepSub
  , sepIncrease
  , columnVerticals
  , verticalIncrease
  , findIndexBelow
  , findIndexBelowEqual
  , parent
  , magmaIndices
  , sFunc
  , stretchDataTop
  , stretchDataList
  , subtract1
  , computeStretch
  , copyColumn
  , extend
  , infinityFs
  , expand
  ) where

import Data.List (foldl)

type Sep = [Int]

data Entry = Entry { eValue :: Int, eSep :: Sep }
  deriving (Eq, Show)

type Column = [Entry]
type Mountain = [Column]
type Vertical = [Sep]

data Position = Position { pI :: Int, pJ :: Int }
  deriving (Eq, Show)

data StretchData = StretchData
  { sdThreshold :: Sep
  , sdStretchTo :: Sep
  , sdForce :: Bool
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- 比较
-- ---------------------------------------------------------------------

numCompare :: Int -> Int -> Int
numCompare a b = compare a b

antiLexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
antiLexCompare a b cmp =
  let lenCmp = numCompare (length a) (length b)
   in if lenCmp /= 0
        then lenCmp
        else go (length a - 1)
  where
    go (-1) = 0
    go i =
      let r = cmp (a !! i) (b !! i)
       in if r /= 0 then r else go (i - 1)

lexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
lexCompare a b cmp =
  let n = min (length a) (length b)
      go i | i >= n = numCompare (length a) (length b)
           | otherwise = let r = cmp (a !! i) (b !! i) in if r /= 0 then r else go (i + 1)
   in go 0

sepCompare :: Sep -> Sep -> Int
sepCompare a b = antiLexCompare a b numCompare

verticalCompare :: Vertical -> Vertical -> Int
verticalCompare a b = lexCompare a b sepCompare

entryCompare :: Entry -> Entry -> Int
entryCompare a b =
  let r = numCompare (eValue a) (eValue b)
   in if r /= 0 then r else sepCompare (eSep a) (eSep b)

columnCompare :: Column -> Column -> Int
columnCompare a b = lexCompare a b entryCompare

mountainCompare :: Mountain -> Mountain -> Int
mountainCompare a b = lexCompare a b columnCompare

-- ---------------------------------------------------------------------
-- 显示
-- ---------------------------------------------------------------------

sepDisplay :: Sep -> String
sepDisplay s =
  let c1 = if length s > 1 then s !! 1 else 0
      c0 = if null s then 0 else head s
   in replicate c1 ';' ++ replicate c0 ','

entryDisplay :: Entry -> String
entryDisplay e = sepDisplay (eSep e) ++ show (eValue e)

columnDisplay :: Column -> String
columnDisplay c = "(" ++ concatMap entryDisplay c ++ ")"

mountainDisplay :: Mountain -> String
mountainDisplay m = concatMap columnDisplay m

-- ---------------------------------------------------------------------
-- 解析
-- ---------------------------------------------------------------------

fromDisplay :: String -> Maybe Mountain
fromDisplay s =
  if s == "Limit" then Nothing else go s 0
  where
    go str i =
      let skipSpaces j = while (< length str) (\k -> str !! k == ' ') (j + 1)
          while p f j = if p j && j < length str then while p f (f j) else j
      in Nothing  -- TODO: implement parser

-- ---------------------------------------------------------------------
-- 山结构辅助
-- ---------------------------------------------------------------------

sepIsOne :: Sep -> Bool
sepIsOne s = length s == 1 && head s == 1

sepDimension :: Sep -> Int
sepDimension s = length (takeWhile (== 0) s)

sepAdd :: Sep -> Sep -> Sep
sepAdd a b
  | null b = a
  | otherwise =
      let result = a ++ replicate (max 0 (length b - length a)) 0
          n = length b - 1
       in take n result ++ [result !! n + last b] ++ drop (n + 1) (result ++ replicate (n + 2) 0)

sepSub :: Sep -> Sep -> Sep
sepSub a b
  | length a > length b = a
  | length a < length b = []
  | otherwise =
      let n = length a
          findD d | d <= 0 = 0
                  | a !! (d - 1) == b !! (d - 1) = findD (d - 1)
                  | otherwise = d
          d = findD n
       in if d == 0 || a !! (d - 1) < b !! (d - 1)
            then []
            else take (d - 1) a ++ [a !! (d - 1) - b !! (d - 1)]

sepIncrease :: Sep -> Int -> Sep
sepIncrease a d =
  let padded = a ++ replicate (max 0 (d + 1 - length a)) 0
   in replicate d 0 ++ [padded !! d + 1] ++ drop (d + 1) padded

verticalIncrease :: Vertical -> Sep -> Vertical
verticalIncrease v s =
  let i = findI (length v)
      findI (-1) = 0
      findI k | k >= 1 && sepCompare (v !! (k - 1)) s < 0 = findI (k - 1)
              | otherwise = k
   in take i v ++ [s]

columnVerticals :: Column -> [Vertical]
columnVerticals c = go c []
  where
    go [] _ = []
    go (e:es) prev =
      let v = verticalIncrease prev (eSep e)
       in v : go es v

findIndexBelow :: [Vertical] -> Vertical -> Int
findIndexBelow verticals y =
  let loop l r =
        if l >= r then l
        else
          let m = (l + r + 1) `div` 2
              wm = if m == 0 then [] else verticals !! (m - 1)
           in if verticalCompare wm y < 0 then loop m r else loop l (m - 1)
   in loop 0 (length verticals)

findIndexBelowEqual :: [Vertical] -> Vertical -> Int
findIndexBelowEqual verticals y =
  let loop l r =
        if l >= r then l
        else
          let m = (l + r + 1) `div` 2
              wm = if m == 0 then [] else verticals !! (m - 1)
           in if verticalCompare wm y <= 0 then loop m r else loop l (m - 1)
   in loop 0 (length verticals)

parent :: Mountain -> [[Vertical]] -> Position -> Position
parent m v pos =
  let value = eValue ((m !! pI pos) !! pJ pos)
      pi = value - 1
      pj = if pi == -1 then 0 else findIndexBelow (v !! pi) ((v !! pI pos) !! pJ pos)
   in Position pi pj

magmaIndices :: Mountain -> [[Vertical]] -> Position -> [[Int]]
magmaIndices m v riRj =
  let ri = pI riRj
      rj = pJ riRj
      go i result
        | i >= length m = result
        | otherwise =
            let result' = result ++ [[]]
             if i <= ri
               then go (i + 1) result'
               else
                 let col = m !! i
                     miCol = computeMiCol i col
                  in go (i + 1) (take i result' ++ [miCol] ++ drop (i + 1) result')
      computeMiCol i col =
        [ let p = parent m v (Position i j)
              pi = pI p
              pj = pJ p
           in if pi < ri then error "break"
              else if pi == ri
                then min pj rj
                else
                  let pj' = if pj == length (m !! pi) then pj - 1 else pj
                   in (magmaIndices m v riRj !! pi) !! pj'
        | j <- [0 .. length col - 1] ]
   in go 0 []

sFunc :: Column -> Int -> Sep -> Sep
sFunc c j bound
  | j > length c = sFunc c (j - 1) bound
  | j < 0 = []
  | sepCompare (eSep (c !! j)) bound >= 0 = []
  | otherwise =
      let current = eSep (c !! j)
          previous = sFunc c (j - 1) bound
       in if sepCompare current previous < 0 then previous else current

stretchDataTop :: Mountain -> [[Vertical]] -> StretchData
stretchDataTop m v =
  let right = length m - 1
      top = length (m !! right) - 1
      p = parent m v (Position right top)
      ri = pI p
      rj = pJ p
      topRightSep = eSep ((m !! right) !! top)
   in if sepDimension topRightSep > 0
        then
          let threshold = sFunc (m !! ri) (rj - 1) topRightSep
              stretchTo = sFunc (m !! right) (top - 1) topRightSep
              (stretchTo', force) = if sepCompare stretchTo threshold <= 0
                                      then (sepIncrease stretchTo 0, True)
                                      else (stretchTo, False)
           in StretchData threshold stretchTo' force
        else
          let threshold = if null topRightSep then [] else init topRightSep
              stretchTo = threshold
              force =
                if sepIsOne topRightSep
                  then False
                  else
                    let vParent = if rj == 0 then [] else (v !! ri) !! (rj - 1)
                        vBottom = if top == 0 then [] else (v !! right) !! (top - 1)
                     in verticalCompare (verticalIncrease vParent stretchTo) vBottom > 0
           in StretchData threshold stretchTo force

stretchDataList :: Mountain -> [[Vertical]] -> [[Int]] -> [StretchData]
stretchDataList m v mi =
  let right = length m - 1
      top = length (m !! right) - 1
      p = parent m v (Position right top)
      ri = pI p
      rj = pJ p
      result = replicate (rj + 1) (StretchData [] [] False)
      loop j refJ
        | j >= rj = result
        | otherwise =
            let refJ' = until (\r -> r + 1 > top || (mi !! right) !! (r + 1) > j) (+ 1) refJ
                currentTopSep = eSep ((m !! ri) !! j)
                threshold = sFunc (m !! ri) (j - 1) currentTopSep
                stretchTo = sFunc (m !! right) (refJ' - 1) currentTopSep
             in loop (j + 1) refJ'
   in loop 0 (-1)

subtract1 :: Mountain -> [[Vertical]] -> StretchData -> Mountain
subtract1 m0 v sdTop =
  let right = length m0 - 1
      top = length (m0 !! right) - 1
      p = parent m0 v (Position right top)
      ri = pI p
      rj = pJ p
      result = take right m0 ++ [take top (m0 !! right)]
      result' = if sdForce sdTop
                  then take right result ++ [(result !! right) ++ [Entry (ri + 1) (sdStretchTo sdTop)]]
                  else result
      extraEntries = [ (m0 !! ri) !! j | j <- [rj .. length (m0 !! ri) - 1] ]
   in take right result' ++ [(result' !! right) ++ extraEntries]

computeStretch :: Sep -> StretchData -> Sep
computeStretch sep sd =
  if sepCompare sep (sdThreshold sd) <= 0
    then sep
    else sepAdd (sdStretchTo sd) (sepSub sep (sdThreshold sd))

copyColumn :: Column -> [Int] -> [Vertical] -> Column -> [Int] -> Position -> [StretchData] -> Int -> Vertical -> Column
copyColumn m0i mi0i v0i mr mir riRj sd offset stretchVMax =
  let ri = pI riRj
      rj = pJ riRj
      loop j lastMi refJ result
        | j >= length m0i = reverse result
        | otherwise =
            let entry = m0i !! j
             if j >= length mi0i
               then
                 let newEntry = if eValue entry >= ri + 1
                                   then entry { eValue = eValue entry + offset }
                                   else entry
                  in loop (j + 1) lastMi refJ (newEntry : result)
               else
                 let newVal = eValue entry + offset
                     currentMi = mi0i !! j
                     (lastMi', refJ', result') =
                       if currentMi /= lastMi
                         then
                           let liftEntries = takeWhile (\rj' -> rj' < length mir && mir !! rj' == currentMi) [refJ ..]
                               isRowLifting rj' = currentMi == rj || (rj' + 1 < length mir && mir !! (rj' + 1) == currentMi)
                               lifted = [ Entry newVal (eSep (mr !! rj')) | rj' <- liftEntries, isRowLifting rj' ]
                            in (currentMi, refJ + length liftEntries, lifted ++ result)
                         else (lastMi, refJ, result)
                     newSep = if verticalCompare (v0i !! j) stretchVMax > 0
                                then eSep entry
                                else computeStretch (eSep entry) (sd !! currentMi)
                  in loop (j + 1) lastMi' refJ' (Entry newVal newSep : result')
   in loop 0 (-1) 0 []

extend :: Mountain -> Mountain
extend m0 =
  let right = length m0 - 1
      top = length (m0 !! right) - 1
      v0 = map columnVerticals m0
      p = parent m0 v0 (Position right top)
      mi0 = magmaIndices m0 v0 p
      sd0 = stretchDataList m0 v0 mi0
      m = subtract1 m0 v0 (sd0 !! pJ p)
      v = take right v0 ++ [columnVerticals (m !! right)]
      miPartial = take right mi0
      mi = magmaIndices m v p miPartial
      offset = right - pI p
      loop i curA
        | i >= length m0 = curA
        | otherwise =
            let newCol = copyColumn (m0 !! i) (mi0 !! i) (v0 !! i) (m !! right) (mi !! right) p sd0 offset ((v0 !! right) !! top)
             in loop (i + 1) (curA ++ [newCol])
   in loop (pI p + 1) m

infinityFs :: Int -> Mountain
infinityFs index = [[], [Entry 1 [index, 1]]]

expand :: Mountain -> Int -> Bool -> Mountain
expand m index shorter
  | null m = m
  | null (last m) = init m
  | otherwise =
      let loop n curA | n > index = curA | otherwise = loop (n + 1) (extend curA)
          a1 = loop 1 m
       in if shorter
            then init a1
            else subtract1 a1 (map columnVerticals a1) (stretchDataTop a1 (map columnVerticals a1))
