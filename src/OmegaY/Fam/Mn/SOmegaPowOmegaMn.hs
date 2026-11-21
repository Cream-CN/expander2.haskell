-- | Sω^ωMN：Sω2MN 的 S_default 变体，移植 s_omega_pow_omega_mn.hpp。
module OmegaY.Fam.Mn.SOmegaPowOmegaMn
  ( sOmegaPowOmegaMnNotation
  ) where

import Data.List (intercalate)
import OmegaY.Notation.Types (Notation, textAlterNotation)

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

type Sep = [Int]
type Vertical = [Sep]
data PEntry = PEntry { peVal :: Int, peSep :: Sep } deriving (Eq, Show)
type PColumn = [PEntry]
type PMountain = [PColumn]

numCompare :: Int -> Int -> Int
numCompare a b = if a == b then 0 else if a < b then -1 else 1

sepCompare :: Sep -> Sep -> Int
sepCompare a b
  | length a /= length b = numCompare (length a) (length b)
  | otherwise = go (length a)
  where
    go i | i <= 0 = 0
         | otherwise = let r = numCompare (a !! (i - 1)) (b !! (i - 1)) in if r /= 0 then r else go (i - 1)

lexCmp :: [a] -> [a] -> (a -> a -> Int) -> Int
lexCmp a b cmp = go 0
  where
    n = min (length a) (length b)
    go i | i >= n = numCompare (length a) (length b)
         | otherwise = let r = cmp (a !! i) (b !! i) in if r /= 0 then r else go (i + 1)

verticalCompare :: Vertical -> Vertical -> Int
verticalCompare a b = lexCmp a b sepCompare

entryCompare :: PEntry -> PEntry -> Int
entryCompare a b = case numCompare (peVal a) (peVal b) of
  0 -> sepCompare (peSep a) (peSep b)
  r -> r

columnCompare :: PColumn -> PColumn -> Int
columnCompare a b = lexCmp a b entryCompare

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- ---------- 显示 ----------
sepDisplay :: Sep -> String
sepDisplay s
  | length s <= 2 =
      replicate (if length s > 1 then s !! 1 else 0) ';' ++ replicate (if null s then 0 else head s) ','
  | otherwise = "[" ++ intercalate "," (map show (reverse s)) ++ "]"

entryDisplay :: PEntry -> String
entryDisplay e = sepDisplay (peSep e) ++ show (peVal e)

columnDisplay :: PColumn -> String
columnDisplay c = "(" ++ concatMap entryDisplay c ++ ")"

mountainDisplay :: PMountain -> String
mountainDisplay = concatMap columnDisplay

-- ---------- 山结构辅助 ----------
sepIsOne :: Sep -> Bool
sepIsOne s = length s == 1 && head s == 1

sepDimension :: Sep -> Int
sepDimension s = go 0
  where
    go d | d < length s && s !! d == 0 = go (d + 1)
         | otherwise = d

sepAdd :: Sep -> Sep -> Sep
sepAdd a b
  | null b = a
  | otherwise =
      let result0 = a ++ replicate (max 0 (length b - length a)) 0
          result1 = setAt (length b - 1) (result0 !! (length b - 1) + last b) result0
       in foldl (\acc d -> setAt d (b !! d) acc) result1 [0 .. length b - 2]

sepSub :: Sep -> Sep -> Sep
sepSub a b
  | length a > length b = a
  | length a < length b = []
  | otherwise =
      let d0 = go (length a)
          go d | d > 0 && a !! (d - 1) == b !! (d - 1) = go (d - 1)
               | otherwise = d
       in if d0 == 0 || a !! (d0 - 1) < b !! (d0 - 1)
            then []
            else setAt (d0 - 1) (a !! (d0 - 1) - b !! (d0 - 1)) (take d0 a)

sepIncrease :: Sep -> Int -> Sep
sepIncrease a d =
  let result0 = a ++ replicate (max 0 (d + 1 - length a)) 0
      result1 = setAt d (result0 !! d + 1) result0
   in foldl (\acc k -> setAt k 0 acc) result1 [0 .. d - 1]

verticalIncrease :: Vertical -> Sep -> Vertical
verticalIncrease v s =
  let i = length v - length (takeWhile (\x -> sepCompare x s < 0) (reverse v))
   in take i v ++ [s]

columnVerticals :: PColumn -> [Vertical]
columnVerticals c = snd (foldl step ([], []) c)
  where
    step (cur, acc) e =
      let cur' = verticalIncrease cur (peSep e)
       in (cur', acc ++ [cur'])

findIndexBelow :: [Vertical] -> Vertical -> Int
findIndexBelow vi v = go 0 (length vi)
  where
    go l r
      | l >= r = l
      | otherwise =
          let j = (l + r + 1) `div` 2
              vij = if j == 0 then [] else vi !! (j - 1)
           in if verticalCompare vij v < 0 then go j r else go l (j - 1)

parentP :: PMountain -> [[Vertical]] -> (Int, Int) -> (Int, Int)
parentP m v (i, j) =
  let value = peVal ((m !! i) !! j)
      pi' = value - 1
      pj' = if pi' == -1 then 0 else findIndexBelow (v !! pi') ((v !! i) !! j)
   in (pi', pj')

magmaIndices :: PMountain -> [[Vertical]] -> (Int, Int) -> [[Int]] -> [[Int]]
magmaIndices m v (ri, rj) result0 = goM (length result0) result0
  where
    goM i result
      | i >= length m = result
      | otherwise =
          let r1 = result ++ [[]]
              row = if i <= ri then [] else buildRow i r1 (m !! i) []
              r2 = setAt i row r1
           in goM (i + 1) r2
    buildRow i result col acc = goJ 0 acc
      where
        goJ j acc'
          | j >= length col = acc'
          | otherwise =
              let (pi', pj0) = parentP m v (i, j)
               in if pi' < ri then acc'
                  else if pi' == ri
                    then goJ (j + 1) (acc' ++ [min pj0 rj])
                    else
                      let pj1 = if pj0 == length (m !! pi') then pj0 - 1 else pj0
                       in if pj1 < 0 || pj1 >= length (result !! pi')
                            then acc'
                            else goJ (j + 1) (acc' ++ [result !! pi' !! pj1])

-- ---------- 展开核心 ----------
sDefault :: Sep -> Sep
sDefault bound =
  let d = sepDimension bound
   in if d == length bound - 1 && bound !! d == 1
        then []
        else setAt d (bound !! d - 1) bound

sFn :: PColumn -> Int -> Sep -> Sep
sFn c j bound
  | j > length c = sFn c (j - 1) bound
  | j < 0 = sDefault bound
  | sepCompare (peSep (c !! j)) bound >= 0 = sDefault bound
  | otherwise =
      let current = peSep (c !! j)
          previous = sFn c (j - 1) bound
       in if sepCompare current previous < 0 then previous else current

data StretchData = StretchData { sdThreshold :: Sep, sdStretchTo :: Sep, sdForce :: Bool } deriving (Show)

stretchDataTop :: PMountain -> [[Vertical]] -> StretchData
stretchDataTop m v =
  let right = length m - 1
      top = length (m !! right) - 1
      (ri, rj) = parentP m v (right, top)
      topRightSep = peSep ((m !! right) !! top)
      threshold = sFn (m !! ri) (rj - 1) topRightSep
      stretchTo0 = sFn (m !! right) (top - 1) topRightSep
      result0 =
        if sepIsOne topRightSep
          then (stretchTo0, False)
          else if sepDimension topRightSep > 0
            then
              let dim = sepDimension topRightSep
               in if sepCompare stretchTo0 (sepIncrease threshold (dim - 1)) < 0
                    then (sepIncrease stretchTo0 (dim - 1), True)
                    else (stretchTo0, False)
            else
              let vParent = if rj == 0 then [] else (v !! ri) !! (rj - 1)
                  vBottom = if top == 0 then [] else (v !! right) !! (top - 1)
               in (stretchTo0, verticalCompare (verticalIncrease vParent stretchTo0) vBottom > 0)
   in StretchData threshold (fst result0) (snd result0)

stretchDataList :: PMountain -> [[Vertical]] -> [[Int]] -> [StretchData]
stretchDataList m v mi =
  let right = length m - 1
      top = length (m !! right) - 1
      (ri, rj) = parentP m v (right, top)
      go j refJ acc
        | j >= rj = acc
        | otherwise =
            let refJ' = incRef refJ j
                currentTopSep = peSep ((m !! ri) !! j)
                threshold = sFn (m !! ri) (j - 1) currentTopSep
                stretchTo = sFn (m !! right) (refJ' - 1) currentTopSep
             in go (j + 1) refJ' (acc ++ [StretchData threshold stretchTo False])
      incRef refJ j
        | refJ + 1 <= top && ((mi !! right) !! (refJ + 1)) <= j = incRef (refJ + 1) j
        | otherwise = refJ
   in go 0 (-1) [] ++ [stretchDataTop m v]

subtract1 :: PMountain -> [[Vertical]] -> StretchData -> PMountain
subtract1 m0 v sdTop =
  let right = length m0 - 1
      top = length (m0 !! right) - 1
      (ri, rj) = parentP m0 v (right, top)
      popped = setAt right (init (m0 !! right)) m0
      inserted =
        if sdForce sdTop
          then setAt right ((popped !! right) ++ [PEntry (ri + 1) (sdStretchTo sdTop)]) popped
          else popped
      brCol = m0 !! ri
      tailEntries = if rj >= 0 && rj <= length brCol then drop rj brCol else []
   in setAt right ((inserted !! right) ++ tailEntries) inserted

computeStretch :: Sep -> StretchData -> Sep
computeStretch sep data_
  | sepCompare sep (sdThreshold data_) <= 0 = sep
  | otherwise = sepAdd (sdStretchTo data_) (sepSub sep (sdThreshold data_))

copyColumn
  :: PColumn -> [Int] -> [Vertical] -> PColumn -> [Int] -> (Int, Int)
  -> [StretchData] -> Int -> Vertical -> PColumn
copyColumn m0i mi0i v0i mr mir (ri, rj) sd offset stretchVMax = go 0 (-1) 0 []
  where
    go j lastMi refJ acc
      | j >= length m0i = acc
      | j >= length mi0i =
          let e = m0i !! j
              e' = if peVal e >= ri + 1 then e { peVal = peVal e + offset } else e
           in go (j + 1) lastMi refJ (acc ++ [e'])
      | otherwise =
          let e = m0i !! j
              newValue = peVal e + offset
              currentMi = mi0i !! j
              (lastMi', refJ', lifted) =
                if currentMi /= lastMi
                  then goLift currentMi refJ acc newValue
                  else (lastMi, refJ, acc)
              newSep =
                if verticalCompare (v0i !! j) stretchVMax > 0
                  then peSep e
                  else computeStretch (peSep e) (sd !! currentMi)
           in go (j + 1) lastMi' refJ' (lifted ++ [PEntry newValue newSep])
    goLift currentMi refJ0 acc0 newValue = loop refJ0 acc0
      where
        loop refJ acc
          | refJ < length mir && (mir !! refJ) == currentMi =
              let isRowLifting = currentMi == rj || (refJ + 1 < length mir && (mir !! (refJ + 1)) == currentMi)
                  acc' = if isRowLifting then acc ++ [PEntry newValue (peSep (mr !! refJ))] else acc
               in loop (refJ + 1) acc'
          | otherwise = (currentMi, refJ, acc)

extend :: PMountain -> PMountain
extend m0 =
  let right = length m0 - 1
      top = length (m0 !! right) - 1
      v0 = map columnVerticals m0
      p@(ri, _) = parentP m0 v0 (right, top)
      mi0 = magmaIndices m0 v0 p []
      sd0 = stretchDataList m0 v0 mi0
      m1 = subtract1 m0 v0 (sd0 !! snd p)
      vFirst = take right v0 ++ [columnVerticals (m1 !! right)]
      mi = magmaIndices m1 vFirst p (take right mi0)
      offset = right - ri
      mV = v0 !! right !! top
      go i mAcc
        | i >= length m0 = mAcc
        | otherwise =
            let copied = copyColumn (m0 !! i) (mi0 !! i) (v0 !! i) (m1 !! right) (mi !! right) p sd0 offset mV
             in go (i + 1) (mAcc ++ [copied])
   in go (ri + 1) m1


expandP :: PMountain -> Int -> Bool -> PMountain
expandP m index shorter
  | null m = m
  | null (last m) = init m
  | otherwise =
      let current = iterate extend m !! index
       in if shorter
            then init current
            else
              let v = map columnVerticals current
               in subtract1 current v (stretchDataTop current v)

-- ---------- 解析 ----------
isDigitC :: Char -> Bool
isDigitC c = c >= '0' && c <= '9'

fromDisplayP :: String -> Maybe PMountain
fromDisplayP s0 = go 0 []
  where
    s = s0
    n = length s
    skipSpaces i = if i < n && s !! i == ' ' then skipSpaces (i + 1) else i
    skipIndex i =
      if i < n && s !! i == ':'
        then let j = skipSpaces (i + 1) in walk j
        else i
      where
        walk k = if k < n && isDigitC (s !! k) then walk (k + 1) else k
    parseNumber i =
      let j = walk i
       in if j == i then Nothing
          else Just (foldl (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 (take (j - i) (drop i s)), j)
      where
        walk k = if k < n && isDigitC (s !! k) then walk (k + 1) else k
    normalizeSep sep = reverse (dropWhile (== 0) (reverse sep))
    parseSep i
      | i < n && s !! i == '[' =
          let loop k acc
                | k < n && s !! k /= ']' =
                    case parseNumber k of
                      Nothing -> Nothing
                      Just (v, k1) ->
                        let k2 = skipSpaces k1
                            k3 = if k2 < n && s !! k2 == ',' then skipSpaces (k2 + 1) else k2
                         in loop k3 (acc ++ [v])
                | k >= n = Nothing
                | otherwise = Just (normalizeSep (reverse acc), k + 1)
           in loop (skipSpaces (i + 1)) []
      | otherwise =
          let c1 = countSemi i 0
              c0 = countComma (i + c1) 0
           in if c0 == 0 && c1 == 0 then Nothing else Just (normalizeSep [c0, c1], i + c1 + c0)
      where
        countSemi k c1 = if k < n && s !! k == ';' then countSemi (k + 1) (c1 + 1) else c1
        countComma k c0 = if k < n && s !! k == ',' then countComma (k + 1) (c0 + 1) else c0
    parseParenColumn i0 =
      let loop i acc
            | i < n && s !! i /= ')' && s !! i /= ':' =
                let i1 = skipSpaces i
                 in case parseSep i1 of
                      Nothing -> Nothing
                      Just (sep, i2) ->
                        let i3 = skipSpaces i2
                         in case parseNumber i3 of
                              Nothing -> Nothing
                              Just (v, i4) -> loop (skipSpaces i4) (acc ++ [PEntry v sep])
            | otherwise =
                let i1 = skipIndex i
                    i2 = skipSpaces i1
                 in if i2 >= n || s !! i2 /= ')' then Nothing else Just (acc, i2 + 1)
       in loop (skipSpaces (i0 + 1)) []
    parseUnparenColumn i0 =
      let i1 = skipSpaces i0
       in if i1 >= n then Nothing
          else if s !! i1 == '0' && (i1 + 1 >= n || s !! (i1 + 1) `elem` ":,;[ (")
            then Just ([], skipIndex (i1 + 1))
            else if s !! i1 == ':' then Just ([], skipIndex i1)
            else loop i1 []
      where
        loop i acc
          | i < n && s !! i /= ' ' && s !! i /= '(' && s !! i /= ':' =
              case parseSep i of
                Nothing -> Nothing
                Just (sep, i2) ->
                  let i3 = skipSpaces i2
                   in case parseNumber i3 of
                        Nothing -> Nothing
                        Just (v, i4) -> loop i4 (acc ++ [PEntry v sep])
          | otherwise = Just (acc, skipIndex i)
    go i acc
      | i >= n = Just acc
      | otherwise =
          let i1 = skipSpaces i
           in if i1 >= n then Just acc
              else
                let parseCol = if s !! i1 == '(' then parseParenColumn i1 else parseUnparenColumn i1
                 in case parseCol of
                      Nothing -> Nothing
                      Just (col, i2) -> go (skipSpaces i2) (acc ++ [col])

-- ---------- 入口 ----------
expandImpl :: Bool -> String -> Int -> String
expandImpl longer text term0 =
  let term = max 1 term0
   in case fromDisplayP text of
          Nothing -> errText
          Just m
            | null m -> ""
            | any (\(PEntry v _) -> v < 0 || v > length m) (concat m) -> errText
            | null (last m) -> mountainDisplay (init m)
            | otherwise -> mountainDisplay (expandP m term (not longer))

sOmegaPowOmegaMnNotation :: Notation
sOmegaPowOmegaMnNotation =
  textAlterNotation "s-omega-pow-omega-mn" "Sω^ωMN" []
    (expandImpl False)
    (expandImpl True)
    ""
