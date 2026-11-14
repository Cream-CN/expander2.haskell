-- | PCF's ωMN：`p:vertical` 语法，移植 pcf_omega_mn.hpp。
module OmegaY.Fam.Mn.PcfOmegaMn
  ( pcfOmegaMnNotation
  ) where

import Data.List (intercalate)
import OmegaY.Notation.Types (Notation, textAlterNotation)

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

type Vertical = [Int]
data PEntry = PE { peP :: Int, peV :: Vertical } deriving (Show)
type PColumn = [PEntry]
type PExpr = [PColumn]

numCompare :: Int -> Int -> Int
numCompare a b = if a == b then 0 else if a < b then -1 else 1

-- TS anti_lex_compare：先比长度，再从末元素向前逐项比。
antiLexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
antiLexCompare a b cmp
  | length a /= length b = if length a > length b then 1 else -1
  | otherwise = go (length a)
  where
    go i | i <= 0 = 0
         | otherwise = let c = cmp (a !! (i - 1)) (b !! (i - 1)) in if c /= 0 then c else go (i - 1)

lexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
lexCompare a b cmp = go 0
  where
    n = min (length a) (length b)
    go i | i >= n = numCompare (length a) (length b)
         | otherwise = let c = cmp (a !! i) (b !! i) in if c /= 0 then c else go (i + 1)

verticalCompare :: Vertical -> Vertical -> Int
verticalCompare a b = antiLexCompare a b numCompare

entryCompare :: PEntry -> PEntry -> Int
entryCompare a b = case numCompare (peP a) (peP b) of
  0 -> verticalCompare (peV a) (peV b)
  c -> c

columnCompare :: PColumn -> PColumn -> Int
columnCompare a b = lexCompare a b entryCompare

verticalIncrease :: Vertical -> Int -> Vertical
verticalIncrease v s
  | length v <= s = replicate s 0 ++ [1]
  | otherwise = [ if k == s then v !! k + 1 else if k < s then 0 else v !! k
                | k <- [0 .. length v - 1] ]

topSep :: Vertical -> Int
topSep v = go 0
  where
    go i | i >= length v = -1
         | v !! i > 0 = i
         | otherwise = go (i + 1)

verticalDecrease :: Vertical -> Vertical
verticalDecrease v =
  let s = topSep v
   in if s < 0 then []
      else if v !! s == 1 && s == length v - 1 then []
      else [ if k == s then v !! k - 1 else v !! k | k <- [0 .. length v - 1] ]

verticalAdd :: Vertical -> Vertical -> Vertical
verticalAdd v a
  | length v < length a = a
  | null a = v
  | otherwise = init a ++ [v !! (length a - 1) + last a] ++ drop (length a) v

verticalSub :: Vertical -> Vertical -> Vertical
verticalSub v base
  | length v > length base = v
  | length v < length base = []
  | otherwise = go (length v - 1)
  where
    go j | j < 0 = []
         | v !! j < base !! j = []
         | v !! j > base !! j = take j v ++ [v !! j - base !! j]
         | otherwise = go (j - 1)

maxVertical :: Vertical -> Vertical -> Vertical
maxVertical a b = if verticalCompare a b < 0 then b else a

-- TS find_index_below_row：col 中最大的 m 使 col[m-1].v < v。
findIndexBelowRow :: PColumn -> Vertical -> Int
findIndexBelowRow col v = go 0 (length col)
  where
    go l r
      | l >= r = l
      | otherwise =
          let m = (l + r + 1) `div` 2
           in if verticalCompare v (peV (col !! (m - 1))) > 0 then go m r else go l (m - 1)

baseVertical :: PExpr -> Vertical -> Int -> Vertical
baseVertical expr top r = go r (verticalDecrease top)
  where
    go i bottom =
      let col = expr !! i
          j = findIndexBelowRow col top
          bottom' = maxVertical bottom (if j == 0 then [] else peV (col !! (j - 1)))
       in if j == length col then bottom' else go (peP (col !! j)) bottom'

stretch :: Vertical -> Vertical -> Vertical -> Int -> Vertical
stretch v threshold target w
  | verticalCompare v threshold <= 0 = v
  | otherwise = go w v
  where
    go k r | k <= 0 = r
           | otherwise = go (k - 1) (verticalAdd target (verticalSub r threshold))

normalizeCol :: PColumn -> PColumn
normalizeCol col = go [] [] col
  where
    go _ acc [] = reverse acc
    go current acc (e : es)
      | verticalCompare (peV e) current > 0 = go (peV e) (e : acc) es
      | otherwise = go current acc es

copyColumn :: PColumn -> Int -> Vertical -> Vertical -> Vertical -> Int -> Int -> PColumn
copyColumn col r threshold target magma offset w = normalizeCol (map one col)
  where
    stretchedMagma = stretch magma threshold target w
    one e =
      let p1 = if peP e >= r then peP e + offset * w else peP e
       in if verticalCompare (peV e) magma <= 0
            then PE p1 (stretch (peV e) threshold target w)
            else if peP e < r
              then PE p1 (peV e)
              else PE p1 (maxVertical (peV e) stretchedMagma)

magmaVerticals :: PExpr -> Int -> [Vertical]
magmaVerticals expr r = go (r + 1) (replicate (length expr) [])
  where
    -- 与 C++ 一致：result 初始全空，按列下标递增填充；读 result[e.p] 时
    -- 若该列还没处理到，读到的就是空 vertical。
    go i acc
      | i >= length expr = acc
      | otherwise = go (i + 1) (setAt i (colMagma acc (expr !! i) []) acc)
    setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs
    colMagma _ [] cur = cur
    colMagma acc (e : es) cur
      | peP e < r = cur
      | peP e == r = peV e
      | otherwise =
          let mvP = acc !! peP e
           in if verticalCompare mvP (peV e) <= 0
                then (if verticalCompare cur mvP < 0 then mvP else cur)
                else colMagma acc es (peV e)

-- TS expand(expr, index, shorter)
expandPcf :: PExpr -> Int -> Bool -> PExpr
expandPcf expr0 index shorter
  | null expr0 = expr0
  | null (last expr0) = init expr0
  | otherwise =
      let right = length expr0 - 1
          up = length (expr0 !! right) - 1
          topEntry = (expr0 !! right) !! up
          r = peP topEntry
          top = peV topEntry
          s = topSep top
          threshold = baseVertical expr0 top r
          targetCandidate = if s == 0 then threshold else verticalIncrease threshold (s - 1)
          newRight = normalizeCol
            (take up (expr0 !! right) ++ [PE r targetCandidate] ++ (expr0 !! r))
          prevVert = if up == 0 then [] else peV ((expr0 !! right) !! (up - 1))
          target = maxVertical targetCandidate prevVert
          magma = magmaVerticals expr0 r
          offset = right - r
          result0 = take right expr0 ++ [newRight]
          copies =
            [ copyColumn (result0 !! j) r threshold target (magma !! j) offset w
            | w <- [1 .. index], j <- [r + 1 .. right]
            ]
          result = result0 ++ copies
       in if shorter then init result else result

-- ---------- 显示 ----------
verticalDisplay :: Vertical -> String
verticalDisplay v =
  let parts =
        [ if j == 0 then show c
          else (if j > 1 then "ω^" ++ show j else "ω") ++ (if c > 1 then "*" ++ show c else "")
        | j <- [0 .. length v - 1], let c = v !! j, c > 0
        ]
   in if null parts then "0" else intercalate "+" (reverse parts)

entryDisplay :: PEntry -> String
entryDisplay e = show (peP e + 1) ++ ":" ++ verticalDisplay (peV e)

colDisplay :: PColumn -> String
colDisplay col = "(" ++ intercalate "," (map entryDisplay col) ++ ")"

displayExpr :: PExpr -> String
displayExpr = concatMap colDisplay

-- ---------- 解析 ----------
normalizeW :: String -> String
normalizeW = map (\c -> if c == 'ω' || c == 'w' || c == 'W' then '\DEL' else c)

skipSpaces :: String -> Int -> Int
skipSpaces s i = if i < length s && s !! i == ' ' then skipSpaces s (i + 1) else i

isDigitC :: Char -> Bool
isDigitC c = c >= '0' && c <= '9'

parseNumber :: String -> Int -> Maybe (Int, Int)
parseNumber s i =
  let j = walk i
   in if j == i then Nothing
      else
        let v = foldl (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 (take (j - i) (drop i s))
         in if v > 1000000 then Nothing else Just (v, j)
  where
    walk k = if k < length s && isDigitC (s !! k) then walk (k + 1) else k

-- TS parse_omega_term：返回 (m, n)。
parseOmegaTerm :: String -> Int -> Maybe ((Int, Int), Int)
parseOmegaTerm s i0 =
  let i1 = skipSpaces s (i0 + 1)
   in case parseM i1 of
        Nothing -> Nothing
        Just (m, i2) ->
          let i3 = skipSpaces s i2
           in if i3 < length s && s !! i3 == '*'
                then case parseNumber s (skipSpaces s (i3 + 1)) of
                       Nothing -> Nothing
                       Just (n, i4) -> Just ((m, n), i4)
                else Just ((m, 1), i3)
  where
    parseM i
      | i < length s && s !! i == '^' =
          let j = skipSpaces s (i + 1)
           in if j < length s && s !! j == '('
                then case parseNumber s (skipSpaces s (j + 1)) of
                       Nothing -> Nothing
                       Just (m, k) ->
                         let k2 = skipSpaces s k
                          in if k2 < length s && s !! k2 == ')' then Just (m, k2 + 1) else Nothing
                else case parseNumber s j of
                       Nothing -> Nothing
                       Just (m, k) -> Just (m, k)
      | otherwise = Just (1, i)

parseVerticalTerm :: String -> Int -> Maybe ((Int, Int), Int)
parseVerticalTerm s i
  | i < length s && s !! i == '\DEL' = parseOmegaTerm s i
  | otherwise = case parseNumber s i of
      Nothing -> Nothing
      Just (n, j) -> Just ((0, n), j)

parseVertical :: String -> Int -> Maybe (Vertical, Int)
parseVertical s i0 = go i0 []
  where
    go i acc =
      let j = skipSpaces s i
       in case parseVerticalTerm s j of
            Nothing -> Nothing
            Just ((m, n), k) ->
              let acc' = if n > 0 then verticalAdd acc (replicate m 0 ++ [n]) else acc
                  k2 = skipSpaces s k
               in if k2 < length s && s !! k2 == '+'
                    then go (k2 + 1) acc'
                    else Just (acc', k2)

parseEntry :: String -> Int -> Maybe (PEntry, Int)
parseEntry s i0 =
  let i1 = skipSpaces s i0
   in case parseNumber s i1 of
        Just (p, i2) | p >= 1 ->
          let i3 = skipSpaces s i2
           in if i3 < length s && s !! i3 == ':'
                then case parseVertical s (i3 + 1) of
                       Nothing -> Nothing
                       Just (v, i4) -> Just (PE (p - 1) v, i4)
                else Nothing
        _ -> Nothing

parseColumn :: String -> Int -> Maybe (PColumn, Int)
parseColumn s i0 =
  let i1 = skipSpaces s (i0 + 1)
   in if i1 < length s && s !! i1 == ')'
        then Just ([], i1 + 1)
        else go i1 []
  where
    go i acc = case parseEntry s i of
      Nothing -> Nothing
      Just (e, j) ->
        let k = skipSpaces s j
         in if k >= length s then Nothing
            else if s !! k == ',' then go (k + 1) (acc ++ [e])
            else if s !! k == ')' then Just (acc ++ [e], k + 1)
            else Nothing

fromDisplayPcf :: String -> Maybe PExpr
fromDisplayPcf raw = go (skipSpaces s 0) []
  where
    s = normalizeW raw
    go i acc
      | i >= length s = Just acc
      | s !! i /= '(' = Nothing
      | otherwise = case parseColumn s i of
          Nothing -> Nothing
          Just (col, j) -> go (skipSpaces s j) (acc ++ [col])

infinityFs :: Int -> PExpr
infinityFs index = [[], [PE 0 (replicate index 0 ++ [1])]]

trimWS :: String -> String
trimWS = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

expandImpl :: Bool -> String -> Int -> String
expandImpl longer text term0 =
  let term = max 1 term0
      raw = trimWS text
   in if raw == "Limit" || raw == "limit" || raw == "LIMIT"
        then displayExpr (infinityFs term)
        else if null raw || raw == "0" || raw == "∅"
          then ""
          else case fromDisplayPcf raw of
            Nothing -> errText
            Just e
              | null e -> ""
              | null (last e) -> displayExpr (init e)
              | otherwise -> displayExpr (expandPcf e term (not longer))

pcfOmegaMnNotation :: Notation
pcfOmegaMnNotation =
  textAlterNotation "pcf-omega-mn" "PCF's ωMN" []
    (expandImpl False)
    (expandImpl True)
    ""
