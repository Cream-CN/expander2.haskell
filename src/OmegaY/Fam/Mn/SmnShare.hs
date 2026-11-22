-- | smnshare：Mountain（value + int sep）的共享运算，对齐 C++ smnshare.hpp。
module OmegaY.Fam.Mn.SmnShare
  ( Entry(..)
  , Column
  , Mountain
  , Vertical
  , Position(..)
  , kUndefSep
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
  -- * 解析（标准非 simple 语法，smnshare::from_display）
  , fromDisplay
  -- * 山结构辅助
  , columnVerticals
  , verticalIncrease
  , findIndexBelow
  , findIndexBelowEqual
  , parent
  , magmaIndices
  , fillGhost
  , clearGhost
  , subtract1
  , copyColumn
  , extend
  , expand
  ) where

import Data.List (foldl)

data Entry = Entry { eValue :: Int, eSep :: Int }
  deriving (Eq, Show)

type Column = [Entry]
type Mountain = [Column]
type Vertical = [Int]

data Position = Position { pI :: Int, pJ :: Int }
  deriving (Eq, Show)

kUndefSep :: Int
kUndefSep = 2147483647

numCompare :: Int -> Int -> Int
numCompare a b = case compare a b of { LT -> -1; EQ -> 0; GT -> 1 }

sepCompare :: Int -> Int -> Int
sepCompare a b
  | a == kUndefSep || b == kUndefSep =
      if a == b then 0 else if a == kUndefSep then 1 else -1
  | otherwise = numCompare a b

verticalCompare :: Vertical -> Vertical -> Int
verticalCompare a b = lexCompare a b sepCompare

lexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
lexCompare a b cmp =
  let n = min (length a) (length b)
      go i
        | i >= n = numCompare (length a) (length b)
        | otherwise = case cmp (a !! i) (b !! i) of
                        0 -> go (i + 1)
                        c -> c
   in go 0

entryCompare :: Entry -> Entry -> Int
entryCompare a b =
  case numCompare (eValue a) (eValue b) of
    0 -> sepCompare (eSep a) (eSep b)
    c -> c

columnCompare :: Column -> Column -> Int
columnCompare a b = lexCompare a b entryCompare

mountainCompare :: Mountain -> Mountain -> Int
mountainCompare a b = lexCompare a b columnCompare

sepDisplay :: Int -> Bool -> String
sepDisplay sep simple
  | simple && sep == 0 = ""
  | otherwise = replicate (sep + 1) ','

entryDisplay :: Entry -> Bool -> String
entryDisplay e simple =
  let d = sepDisplay (eSep e) simple
      dv = show (eValue e)
      dv' = if simple && length dv >= 2 then "(" ++ dv ++ ")" else dv
   in d ++ dv'

columnDisplay :: Column -> Bool -> String
columnDisplay c simple
  | simple && null c = "0"
  | otherwise =
      let inner = concatMap (\e -> entryDisplay e simple) c
       in if simple then inner else "(" ++ inner ++ ")"

mountainDisplay :: Mountain -> Bool -> String
mountainDisplay m simple =
  concatMap (\(i, c) -> (if i > 0 && simple then " " else "") ++ columnDisplay c simple) (zip [0..] m)

-- 解析（smnshare::from_display，非 simple 语法）

fromDisplay :: String -> Maybe Mountain
fromDisplay s0 = go s0 0
  where
    go s i
      | i >= length s = Just []
      | otherwise =
          let i1 = skipSpaces s i
           in if i1 >= length s
                then Just []
                else
                  let col = if s !! i1 == '(' then parseParenCol s i1 else parseUnparenCol s i1
                   in case col of
                        Nothing -> Nothing
                        Just (c, i2) ->
                          let i3 = skipSpaces s i2
                           in (c :) <$> go s i3
    skipSpaces s i = let n = length s in if i < n && s !! i == ' ' then skipSpaces s (i + 1) else i
    skipIndex s i =
      let n = length s
       in if i < n && s !! i == ':'
            then let i1 = i + 1
                     i2 = skipSpaces s i1
                     n2 = length s
                  in walk i2
              else i
      where walk i = let n = length s in if i < n && (s !! i >= '0' && s !! i <= '9') then walk (i + 1) else i
    parseSep s i =
      let n = length s
          walk count k = if k < n && s !! k == ',' then walk (count + 1) (k + 1) else (count, k)
          (cnt, i1) = walk 0 i
       in (if cnt == 0 then 0 else cnt - 1, i1)
    parseNumber s i =
      let n = length s
          start = i
       in if i >= n || not (isDigitC (s !! i)) then Nothing
          else
            let j = walk i
                walk k = if k < n && isDigitC (s !! k) then walk (k + 1) else k
                digits = take (j - start) (drop start s)
                v = foldl (\a d -> a * 10 + digitC d) 0 digits
             in if v > 1000000 then Nothing else Just (v, j)
    isDigitC c = c >= '0' && c <= '9'
    digitC c = fromEnum c - fromEnum '0'
    parseParenCol s i =
      let i1 = i + 1  -- skip '('
          i2 = skipSpaces s i1
       in entries s i2
      where
        entries s k =
          let n = length s
           in if k >= n || s !! k == ')' || s !! k == ':'
                then
                  let k1 = skipIndex s k
                      k2 = skipSpaces s k1
                   in if k2 >= n || s !! k2 /= ')' then Nothing
                      else Just ([], k2 + 1)
                else
                  let k3 = skipSpaces s k
                      (sep, k4) = parseSep s k3
                      k5 = skipSpaces s k4
                   in case parseNumber s k5 of
                        Nothing -> Nothing
                        Just (v, k6) ->
                          let k7 = skipSpaces s k6
                           in case entries s k7 of
                                Nothing -> Nothing
                                Just (rest, end) -> Just (Entry v sep : rest, end)
    parseUnparenCol s i =
      let n = length s
          i1 = skipSpaces s i
       in if i1 >= n then Nothing
          else
            let c = s !! i1
             in if c == '0' &&
                   (i1 + 1 >= n || s !! (i1 + 1) == ':' || s !! (i1 + 1) == ' ' ||
                    s !! (i1 + 1) == '(')
                  then let k1 = i1 + 1
                           k2 = skipIndex s k1
                        in Just ([], k2)
                  else entries s i1
      where
        entries s k =
          let n = length s
           in if k >= n || s !! k == ' ' || s !! k == '(' || s !! k == ':'
                then let k1 = skipIndex s k in Just ([], k1)
                else
                  let c = s !! k
                   in if c /= ',' then Nothing
                      else
                        let (sep, k1) = parseSep s k
                            k2 = skipSpaces s k1
                         in case parseNumber s k2 of
                              Nothing -> Nothing
                              Just (v, k3) ->
                                case entries s k3 of
                                  Nothing -> Nothing
                                  Just (rest, end) -> Just (Entry v sep : rest, end)

-- 山结构辅助

verticalIncrease :: Vertical -> Int -> Vertical
verticalIncrease v s =
  let n = length v
      go i = if i >= 1 && sepCompare (v !! (i - 1)) s < 0 then go (i - 1) else i
      i = go n
   in take i v ++ [s]

columnVerticals :: Column -> [Vertical]
columnVerticals c = go [] c
  where
    go cur [] = []
    go cur (e:es) =
      let v = verticalIncrease cur (eSep e)
       in v : go v es

findIndexBelow :: [Vertical] -> Vertical -> Int
findIndexBelow vi v =
  let l = 0
      r = length vi
      go lo hi =
        if lo >= hi then lo
        else
          let j = (lo + hi + 1) `div` 2
              vij = if j == 0 then [] else vi !! (j - 1)
           in if verticalCompare vij v < 0 then go j hi else go lo (j - 1)
   in go l r

findIndexBelowEqual :: [Vertical] -> Vertical -> Int
findIndexBelowEqual vi v =
  let go lo hi =
        if lo >= hi then lo
        else
          let j = (lo + hi + 1) `div` 2
              vij = if j == 0 then [] else vi !! (j - 1)
           in if verticalCompare vij v <= 0 then go j hi else go lo (j - 1)
   in go 0 (length vi)

parent :: Mountain -> [[Vertical]] -> Position -> Position
parent m v pos =
  let value = eValue ((m !! pI pos) !! pJ pos)
      pi = value - 1
      pj = if pi == -1 || pi >= length v
             then 0
             else findIndexBelow (v !! pi) ((v !! pI pos) !! pJ pos)
   in Position pi pj

magmaIndices :: Mountain -> [[Vertical]] -> Position -> [[Int]] -> [[Int]]
magmaIndices m v rirj result0 =
  let ri = pI rirj; rj = pJ rirj
      nCols = length m
      go i result =
        if i >= nCols then result
        else
          let result' = if i < length result then result else result ++ [[]]
              row = result' !! i
           in if i <= ri then go (i + 1) result'
              else
                let col = m !! i
                    row' = fillRow result' i col 0 row
                 in go (i + 1) (take i result' ++ [row'] ++ drop (i + 1) result')
      fillRow result' i col j row =
        if j >= length col then row
        else
          let p = parent m v (Position i j)
              pi = pI p; pj = pJ p
           in if pi < ri || pi >= length m then row
              else if pi == ri
                then fillRow result' i col (j + 1) (row ++ [if pj < rj then pj else rj])
                else
                  let pj' = if pj == length (m !! pi) then pj - 1 else pj
                   in if pj' >= length (result' !! pi) then row
                      else fillRow result' i col (j + 1) (row ++ [result' !! pi !! pj'])
   in go (length result0) result0

fillGhost :: Mountain -> Mountain
fillGhost m0 =
  let v = columnVerticals `map` m0
      nCols = length m0
      goI i m v' =
        if i >= nCols then m
        else
          let nRows = length (m !! i)
              goJ j m2 v2 =
                if j >= nRows then (m2, v2)
                else
                  let p = parent m2 v2 (Position i j)
                      pi = pI p
                   in if pi >= length m2 then goJ (j + 1) m2 v2
                      else if pJ p /= length (m2 !! pi) then goJ (j + 1) m2 v2
                      else
                        let vParent = if pJ p == 0 then [] else (v2 !! pi) !! (pJ p - 1)
                            vCur = (v2 !! i) !! j
                            sep = eSep ((m2 !! i) !! j)
                         in if verticalCompare (verticalIncrease vParent sep) vCur < 0
                              then
                                let undefSep = if length vCur >= 2 then vCur !! (length vCur - 2) else kUndefSep
                                    m' = take pi m2 ++ [(m2 !! pi) ++ [Entry 0 undefSep]] ++ drop (pi + 1) m2
                                    vv = take (length vCur - 1) vCur
                                    v' = take pi v2 ++ [(v2 !! pi) ++ [vv]] ++ drop (pi + 1) v2
                                 in goJ (j + 1) m' v'
                              else goJ (j + 1) m2 v2
              (mNext, vNext) = goJ 0 m v'  -- 用参数里的最新 verticals；外层 v 是初始值，不能用
           in goI (i + 1) mNext vNext
   in goI 0 m0 v

clearGhost :: Mountain -> Mountain
clearGhost m = map (filter (\e -> eValue e /= 0)) m

subtract1 :: Mountain -> Maybe [[Vertical]] -> Mountain
subtract1 m0 mV =
  let vOwn = case mV of
               Just v -> v
               Nothing -> columnVerticals `map` m0
      right = length m0 - 1
      top = length (m0 !! right) - 1
      topRightSep = eSep ((m0 !! right) !! top)
      p = parent m0 vOwn (Position right top)
      ri = pI p; rj = pJ p
      result0 = m0
      result1 = take right result0 ++ [take (top) (result0 !! right)]
   in if topRightSep > 0
        then
          let newSep = topRightSep - 1
              vParent = if rj == 0 then [] else (vOwn !! ri) !! (rj - 1)
              vBottom = if top == 0 then [] else (vOwn !! right) !! (top - 1)
           in if verticalCompare (verticalIncrease vParent newSep) vBottom > 0
                then
                  let result2 = take right result1 ++ [(result1 !! right) ++ [Entry (ri + 1) newSep]] ++ drop (right + 1) result1
                      tailCol = drop rj (m0 !! ri)
                   in take right result2 ++ [(result2 !! right) ++ tailCol] ++ drop (right + 1) result2
                else
                  let tailCol = drop rj (m0 !! ri)
                   in take right result1 ++ [(result1 !! right) ++ tailCol] ++ drop (right + 1) result1
        else
          let tailCol = drop rj (m0 !! ri)
           in take right result1 ++ [(result1 !! right) ++ tailCol] ++ drop (right + 1) result1

copyColumn :: Column -> [Int] -> Column -> [Int] -> Position -> Int -> Column
copyColumn m0i mi0i mr mir rirj offset = go 0 (-1) 0
  where
    ri = pI rirj; rj = pJ rirj
    go j lastMi refJ
      | j >= length m0i = []
      | j >= length mi0i =
          let e = m0i !! j
              e' = if eValue e >= ri + 1 then e { eValue = eValue e + offset } else e
           in e' : go (j + 1) lastMi refJ
      | otherwise =
          let e = m0i !! j
              newValue = eValue e + offset
              currentMi = mi0i !! j
           in if currentMi /= lastMi
                then
                  let (lifted, refJ') = lift refJ currentMi newValue
                   in lifted ++ Entry newValue (eSep e) : go (j + 1) currentMi refJ'
                else Entry newValue (eSep e) : go (j + 1) lastMi refJ
    lift refJ currentMi newValue
      | refJ >= length mir || mir !! refJ /= currentMi = ([], refJ)
      | otherwise =
          let isRowLifting = currentMi == rj ||
                                  (refJ + 1 < length mir && mir !! (refJ + 1) == currentMi)
              lifted = if isRowLifting
                         then [Entry newValue (eSep (mr !! refJ))]
                         else []
              (rest, refJ') = lift (refJ + 1) currentMi newValue
           in (lifted ++ rest, refJ')

extend :: Mountain -> Mountain
extend m0 =
  let right = length m0 - 1
      top = length (m0 !! right) - 1
      v0 = columnVerticals `map` m0
      p = parent m0 v0 (Position right top)
      mi0 = magmaIndices m0 v0 p []
      m = subtract1 m0 (Just v0)
      v = take right v0 ++ [columnVerticals (m !! right)]
      miPartial = take right mi0
      mi = magmaIndices m v p miPartial
      offset = right - pI p
      go i m'
        | i >= length m0 = m'
        | otherwise =
            let col = copyColumn (m0 !! i) (mi0 !! i) (m' !! right) (mi !! right) p offset
             in go (i + 1) (m' ++ [col])
   in go (pI p + 1) m

expand :: Mountain -> Int -> Bool -> Mountain
expand m index shorter
  | null m = m
  | null (last m) = take (length m - 1) m
  | otherwise =
      let current = fillGhost m
          iter = iterate extend current !! index
          after = if shorter then take (length iter - 1) iter else subtract1 iter Nothing
       in clearGhost after
