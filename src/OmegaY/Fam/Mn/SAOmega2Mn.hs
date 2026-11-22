-- | SAω2MN：带 mark 的 Sω2MN 变体，移植 sa_omega2_mn.hpp。
module OmegaY.Fam.Mn.SAOmega2Mn
  ( saOmega2MnNotation
  ) where

import OmegaY.Notation.Types (Notation, textAlterNotation)

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

type Sep = [Int]
type Vertical = [Sep]
data AEntry = AEntry { aeValue :: Int, aeSep :: Sep, aeMark :: Bool } deriving (Eq, Show)
type AColumn = [AEntry]
type AMountain = [AColumn]

numCompare :: Int -> Int -> Int
numCompare a b = if a == b then 0 else if a < b then -1 else 1

boolCompare :: Bool -> Bool -> Int
boolCompare a b = (if a then 1 else 0) - (if b then 1 else 0)

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

entryCompare :: AEntry -> AEntry -> Int
entryCompare a b = case numCompare (aeValue a) (aeValue b) of
  0 -> case sepCompare (aeSep a) (aeSep b) of
    0 -> boolCompare (aeMark a) (aeMark b)
    r -> r
  r -> r

columnCompare :: AColumn -> AColumn -> Int
columnCompare a b = lexCmp a b entryCompare

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- ---------- 显示 ----------
sepDisplay :: Sep -> String
sepDisplay s =
  replicate (if length s > 1 then s !! 1 else 0) ';' ++ replicate (if null s then 0 else head s) ','

entryDisplay :: AEntry -> String
entryDisplay e = sepDisplay (aeSep e) ++ (if aeMark e then "*" else "") ++ show (aeValue e)

columnDisplay :: AColumn -> String
columnDisplay c = "(" ++ concatMap entryDisplay c ++ ")"

mountainDisplay :: AMountain -> String
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

columnVerticals :: AColumn -> [Vertical]
columnVerticals c = snd (foldl step ([], []) c)
  where
    step (cur, acc) e =
      let cur' = verticalIncrease cur (aeSep e)
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

parentP :: AMountain -> [[Vertical]] -> (Int, Int) -> (Int, Int)
parentP m v (i, j) =
  let value = aeValue ((m !! i) !! j)
      pi' = value - 1
      pj' = if pi' == -1 then 0 else findIndexBelow (v !! pi') ((v !! i) !! j)
   in (pi', pj')

magmaIndices :: AMountain -> [[Vertical]] -> (Int, Int) -> [[Int]] -> [[Int]]
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
sFn :: AColumn -> Int -> Sep
sFn c j
  | j > length c = sFn c (j - 1)
  | j < 0 = []
  | length (aeSep (c !! j)) > 1 && (aeSep (c !! j) !! 1) /= 0 = []
  | aeMark (c !! j) = aeSep (c !! j)
  | otherwise = sFn c (j - 1)

data StretchData = StretchData { sdThreshold :: Sep, sdStretchTo :: Sep, sdForce :: Bool } deriving (Show)

stretchDataTop :: AMountain -> [[Vertical]] -> Maybe StretchData
stretchDataTop m v =
  let right = length m - 1
      top = length (m !! right) - 1
      (ri, rj) = parentP m v (right, top)
      topRightSep = aeSep ((m !! right) !! top)
   in if not (null topRightSep) && head topRightSep /= 0
        then Nothing
        else
          let threshold = sFn (m !! ri) (rj - 1)
              stretchTo0 = sFn (m !! right) (top - 1)
           in if sepCompare stretchTo0 threshold <= 0
                then Just (StretchData threshold (sepIncrease stretchTo0 0) True)
                else Just (StretchData threshold stretchTo0 False)

stretchDataList :: AMountain -> [[Vertical]] -> [[Int]] -> [Maybe StretchData]
stretchDataList m v mi =
  let right = length m - 1
      top = length (m !! right) - 1
      (ri, rj) = parentP m v (right, top)
      go j refJ acc
        | j >= rj = acc
        | otherwise =
            let refJ' = incRef refJ j
                sep = aeSep ((m !! ri) !! j)
                acc' =
                  if not (null sep) && head sep /= 0
                    then acc ++ [Nothing]
                    else
                      let threshold = sFn (m !! ri) (j - 1)
                          stretchTo = sFn (m !! right) (refJ' - 1)
                       in acc ++ [Just (StretchData threshold stretchTo False)]
             in go (j + 1) refJ' acc'
      incRef refJ j
        | refJ + 1 <= top && ((mi !! right) !! (refJ + 1)) <= j = incRef (refJ + 1) j
        | otherwise = refJ
   in go 0 (-1) [] ++ [stretchDataTop m v]

subtract1 :: AMountain -> [[Vertical]] -> Maybe StretchData -> AMountain
subtract1 m0 v sdTop =
  let right = length m0 - 1
      top = length (m0 !! right) - 1
      (ri, rj) = parentP m0 v (right, top)
      topRightSep = aeSep ((m0 !! right) !! top)
      popped = setAt right (init (m0 !! right)) m0
      step2
        | sepIsOne topRightSep = popped
        | sepDimension topRightSep == 0 =
            let newSep0 = topRightSep
                newSep = if null newSep0 then [] else setAt 0 (head newSep0 - 1) newSep0
                vParent = if rj == 0 then [] else (v !! ri) !! (rj - 1)
                vBottom = if top == 0 then [] else (v !! right) !! (top - 1)
             in if verticalCompare (verticalIncrease vParent newSep) vBottom > 0
                  then setAt right ((popped !! right) ++ [AEntry (ri + 1) newSep (not (null topRightSep) && head topRightSep == 0)]) popped
                  else popped
        | otherwise =
            case sdTop of
              Just sd | sdForce sd -> setAt right ((popped !! right) ++ [AEntry (ri + 1) (sdStretchTo sd) True]) popped
              _ -> popped
      inserted = step2
      brCol = m0 !! ri
      tailEntries = if rj >= 0 && rj <= length brCol then drop rj brCol else []
   in setAt right ((inserted !! right) ++ tailEntries) inserted

computeStretch :: Sep -> Maybe StretchData -> Sep
computeStretch sep Nothing = sep
computeStretch sep (Just sd)
  | sepCompare sep (sdThreshold sd) <= 0 = sep
  | otherwise = sepAdd (sdStretchTo sd) (sepSub sep (sdThreshold sd))

copyColumn
  :: AColumn -> [Int] -> [Vertical] -> AColumn -> [Int] -> (Int, Int)
  -> [Maybe StretchData] -> Int -> Vertical -> AColumn
copyColumn m0i mi0i v0i mr mir (ri, rj) sd offset stretchVMax = go 0 (-1) 0 []
  where
    go j lastMi refJ acc
      | j >= length m0i = acc
      | j >= length mi0i =
          let e = m0i !! j
              e' = if aeValue e >= ri + 1 then e { aeValue = aeValue e + offset } else e
           in go (j + 1) lastMi refJ (acc ++ [e'])
      | otherwise =
          let e = m0i !! j
              newValue = aeValue e + offset
              currentMi = mi0i !! j
              (lastMi', refJ', lifted) =
                if currentMi /= lastMi
                  then goLift currentMi refJ acc newValue
                  else (lastMi, refJ, acc)
              newSep =
                if verticalCompare (v0i !! j) stretchVMax > 0
                  then aeSep e
                  else computeStretch (aeSep e) (sd !! currentMi)
           in go (j + 1) lastMi' refJ' (lifted ++ [AEntry newValue newSep (aeMark e)])
    goLift currentMi refJ0 acc0 newValue = loop refJ0 acc0
      where
        loop refJ acc
          | refJ < length mir && (mir !! refJ) == currentMi =
              let isRowLifting = currentMi == rj || (refJ + 1 < length mir && (mir !! (refJ + 1)) == currentMi)
                  acc' = if isRowLifting
                           then acc ++ [AEntry newValue (aeSep (mr !! refJ)) (aeMark (mr !! refJ))]
                           else acc
               in loop (refJ + 1) acc'
          | otherwise = (currentMi, refJ, acc)

extend :: AMountain -> AMountain
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

expandA :: AMountain -> Int -> Bool -> AMountain
expandA m index shorter
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

fromDisplayA :: String -> Maybe AMountain
fromDisplayA s0 = go 0 []
  where
    s = s0
    n = length s
    parseSimpleSep start =
      let c1 = countSemi start 0
          c0 = countComma (start + c1) 0
          sep = normalizeSep [c0, c1]
       in (sep, start + c1 + c0)
      where
        countSemi k c1 = if k < n && s !! k == ';' then countSemi (k + 1) (c1 + 1) else c1
        countComma k c0 = if k < n && s !! k == ',' then countComma (k + 1) (c0 + 1) else c0
        normalizeSep sep = reverse (dropWhile (== 0) (reverse sep))
    go i acc
      | i >= n = Just acc
      | s !! i /= '(' = Nothing
      | otherwise =
          let loopCol j col
                | j < n && s !! j /= ')' =
                    let (sep, j1) = parseSimpleSep j
                        mark = j1 < n && s !! j1 == '*'
                        j2 = if mark then j1 + 1 else j1
                        j3 = walkNum j2
                     in if j3 == j2 then Nothing
                        else
                          let v = foldl (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 (take (j3 - j2) (drop j2 s))
                           in loopCol j3 (col ++ [AEntry v sep mark])
                | otherwise = if j < n && s !! j == ')' then Just (col, j + 1) else Nothing
              walkNum k = if k < n && isDigitC (s !! k) then walkNum (k + 1) else k
           in case loopCol (i + 1) [] of
                Nothing -> Nothing
                Just (col, j1) -> go j1 (acc ++ [col])

-- ---------- 入口 ----------
expandImpl :: Bool -> String -> Int -> String
expandImpl longer text term0 =
  let term = max 1 term0
   in case fromDisplayA text of
          Nothing -> errText
          Just m
            | null m -> ""
            | any (\(AEntry v _ _) -> v < 0 || v > length m) (concat m) -> errText
            | null (last m) -> mountainDisplay (init m)
            | otherwise -> mountainDisplay (expandA m term (not longer))

saOmega2MnNotation :: Notation
saOmega2MnNotation =
  textAlterNotation "sa-omega2-mn" "SAω2MN" []
    (expandImpl False)
    (expandImpl True)
    ""
