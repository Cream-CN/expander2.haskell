-- | TωMN：递归 TExpr，移植 t_omega_mn.hpp。
module OmegaY.Fam.Mn.TOmegaMn
  ( tOmegaMnNotation
  ) where

import Data.List (intercalate)
import OmegaY.Notation.Types (Notation, textAlterNotation)

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

-- 字段顺序即 C++ 的 entry_compare 顺序：value 优先，再比 sep。
data TEntry = TE { teValue :: Int, teSep :: TExpr } deriving (Eq, Ord, Show)
type TColumn = [TEntry]
type TExpr = [TColumn]
type TVertical = [TExpr]

isLimit :: TExpr -> Bool
isLimit m = not (null m) && not (null (last m))

isOne :: TExpr -> Bool
isOne m = length m == 1 && null (head m)

sepAllEmpty :: TExpr -> Bool
sepAllEmpty = all null

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- ---------- 显示 ----------
sepDisplay :: TExpr -> Bool -> String
sepDisplay sep simple
  | sepAllEmpty sep =
      let sepLen = length sep
       in if sepLen == 1 && simple then "" else replicate sepLen ','
  | otherwise =
      let d = mountainDisplay' sep simple
       in if simple then "[" ++ d ++ "]" else d

entryDisplay :: TEntry -> Bool -> String
entryDisplay e simple =
  let d = sepDisplay (teSep e) simple
      dv0 = show (teValue e)
      dv = if simple && length dv0 >= 2 then "(" ++ dv0 ++ ")" else dv0
   in d ++ dv

columnDisplay :: TColumn -> Bool -> String
columnDisplay c simple
  | simple && null c = "0"
  | otherwise =
      let result = concatMap (`entryDisplay` simple) c
       in if simple then result else "(" ++ result ++ ")"

mountainDisplay' :: TExpr -> Bool -> String
mountainDisplay' m simple = intercalate (if simple then " " else "") (map (`columnDisplay` simple) m)

-- ---------- 竖向机制 ----------
verticalCompare :: TVertical -> TVertical -> Int
verticalCompare a b = case compare a b of
  LT -> -1
  EQ -> 0
  GT -> 1

verticalIncrease :: TVertical -> TExpr -> TVertical
verticalIncrease v m =
  let dropN = length (takeWhile (< m) (reverse v))
      i = length v - dropN
   in take i v ++ [m]

findIndexBelowRow :: [TVertical] -> TVertical -> Int
findIndexBelowRow verticals y = go 0 (length working - 1)
  where
    working = ([] :: TVertical) : verticals
    go l r
      | l >= r = l
      | otherwise =
          let m = (l + r + 1) `div` 2
           in if verticalCompare (working !! m) y < 0 then go m r else go l (m - 1)

columnVerticals :: TColumn -> [TVertical]
columnVerticals column = snd (foldl step ([], []) column)
  where
    step (v, acc) e =
      let v' = verticalIncrease v (teSep e)
       in (v', acc ++ [v'])

parent :: TExpr -> [[TVertical]] -> (Int, Int) -> (Int, Int)
parent a v (i, j) =
  let value = teValue ((a !! i) !! j)
      targetI = value - 1
   in if targetI < 0
        then (-1, 0)
        else (targetI, findIndexBelowRow (v !! targetI) ((v !! i) !! j))

getReferences :: TExpr -> [TVertical] -> [Int]
getReferences a rTops = go 0 0 (replicate (length rTops) (-1))
  where
    verticals = ([] :: TVertical) : columnVerticals (last a)
    go i j ref
      | i >= length verticals || j >= length rTops = ref
      | verticalCompare (verticals !! i) (rTops !! j) < 0 = go (i + 1) j (setAt j i ref)
      | otherwise = go i (j + 1) ref

-- ---------- 展开 ----------
threshold :: TExpr -> Bool -> TVertical -> TVertical -> Int
threshold a shorter low high = go 0
  where
    go n =
      let res = expand a n shorter
       in if verticalCompare (verticalIncrease low res) (verticalIncrease high res) >= 0 then n else go (n + 1)

expand :: TExpr -> Int -> Bool -> TExpr
expand a0 index shorter =
  let rightmost = length a0 - 1
      topmost = length (a0 !! rightmost) - 1
   in if topmost == -1
        then init a0
        else
          let a1 = a0
              topRightSep = teSep ((a1 !! rightmost) !! topmost)
              v0 = map columnVerticals a1
              brij@(brI, brJ) = parent a1 v0 (rightmost, topmost)
              width = rightmost - brI
           in if isLimit topRightSep
                then
                  let vLow = if brJ - 1 < 0 then [] else (v0 !! brI) !! (brJ - 1)
                      vHigh = if topmost - 1 < 0 then [] else (v0 !! rightmost) !! (topmost - 1)
                      sep' = expand topRightSep (threshold topRightSep shorter vLow vHigh + index) shorter
                   in setAt rightmost (setAt topmost (TE (teValue ((a1 !! rightmost) !! topmost)) sep') (a1 !! rightmost)) a1
                else
                  let topLen = min (if brJ < 0 then 0 else brJ) (length (v0 !! brI))
                      topVerticals = take topLen (v0 !! brI) ++ [(v0 !! rightmost) !! topmost]
                      a2 =
                        if isOne topRightSep
                          then setAt rightmost (init (a1 !! rightmost)) a1
                          else
                            let newSep = init topRightSep
                                vParent = if brJ - 1 < 0 then [] else (v0 !! brI) !! (brJ - 1)
                                vBottom = if topmost - 1 < 0 then [] else (v0 !! rightmost) !! (topmost - 1)
                             in if verticalCompare (verticalIncrease vParent newSep) vBottom <= 0
                                  then setAt rightmost (init (a1 !! rightmost)) a1
                                  else
                                    setAt rightmost
                                      (setAt topmost (TE (teValue ((a1 !! rightmost) !! topmost)) newSep) (a1 !! rightmost))
                                      a1
                      brCol = a2 !! brI
                      tailCol = (a2 !! rightmost) ++ (if brJ >= 0 && brJ < length brCol then drop brJ brCol else [])
                      a3 = setAt rightmost tailCol a2
                      v = map columnVerticals a3
                      magmaChecksList =
                        [ if i > brI && i <= rightmost
                            then
                              [ let (wi, wj) = walk i j
                                    vWork = if wi == -1 || wj - 1 < 0 then [] else (v !! wi) !! (wj - 1)
                                    vSrc = if j == 0 then [] else (v !! i) !! (j - 1)
                                 in if wi == brI && wj <= brJ && verticalCompare vWork vSrc == 0 then wj else -1
                              | (j, _) <- zip [0 ..] (a3 !! i)
                              ]
                            else []
                        | i <- [0 .. rightmost]
                        ]
                      walk wi wj
                        | wi <= brI = (wi, wj)
                        | otherwise =
                            let wj1 = if length (a3 !! wi) <= wj then wj - 1 else wj
                             in if wj1 < 0 then (-1, 0) else walk' (parent a3 v (wi, wj1))
                      walk' p = if fst p > brI then walk (fst p) (snd p) else p
                      stepN n aCur
                        | n > index = aCur
                        | otherwise =
                            let refs = getReferences aCur topVerticals
                                refAt k = if k == -1 then -1 else refs !! k
                                targetEnd = brI + width * n + width + 1
                                aPad = if length aCur < targetEnd then aCur ++ replicate (targetEnd - length aCur) [] else aCur
                                fillDx aAcc dx
                                  | dx > width = aAcc
                                  | otherwise =
                                      let x = brI + dx
                                          sourceMagmas = magmaChecksList !! x
                                          build tc (y, entry') =
                                            let entry = entry'
                                                value = teValue entry
                                                sm = if y < length sourceMagmas then sourceMagmas !! y else -1
                                             in if sm >= 0
                                                  then
                                                    let brIndex = sm
                                                        lo = refAt (brIndex - 1) + 1
                                                        hi = refAt brIndex
                                                        addJ acc j
                                                          | j > hi = acc
                                                          | j == hi = acc ++ [TE (value + width * n) (teSep entry)]
                                                          | otherwise =
                                                              addJ (acc ++ [TE (value + width * n)
                                                                              (teSep (((aPad !! (brI + width * n)) !! j)))]) (j + 1)
                                                     in addJ tc lo
                                                  else tc ++ [TE (value + (if value > brI then width * n else 0)) (teSep entry)]
                                          targetCol = foldl build [] (zip [0 ..] (aPad !! x))
                                          aNext = setAt (x + width * n) targetCol aAcc
                                       in fillDx aNext (dx + 1)
                                aN = fillDx aPad 1
                             in stepN (n + 1) aN
                      result = stepN 1 a3
                   in if shorter then init result else result

infinityFs :: Int -> TExpr
infinityFs n
  | n <= 0 = [[]]
  | otherwise = [[], [TE 1 (infinityFs (n - 1))]]

-- ---------- 解析 ----------
skipSpaces :: String -> Int -> Int
skipSpaces s i = if i < length s && s !! i == ' ' then skipSpaces s (i + 1) else i

isDigitC :: Char -> Bool
isDigitC c = c >= '0' && c <= '9'

parseNumber :: String -> Int -> Maybe (Int, Int)
parseNumber s i0 =
  let i = skipSpaces s i0
      j = walk i
   in if j == i then Nothing
      else
        let v = foldl (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 (take (j - i) (drop i s))
         in if v > 1000000 then Nothing else Just (v, j)
  where
    walk k = if k < length s && isDigitC (s !! k) then walk (k + 1) else k

parseSep :: String -> Int -> Maybe (TExpr, Int)
parseSep s i0 =
  let i = skipSpaces s i0
   in if i < length s && s !! i == ','
        then let j = run i 0 in Just (replicate j [], i + j)
        else if i < length s && s !! i == '(' then parseExpr s i
        else Nothing
  where
    run k c = if k < length s && s !! k == ',' then run (k + 1) (c + 1) else c

parseEntry :: String -> Int -> Maybe (TEntry, Int)
parseEntry s i0 = case parseSep s i0 of
  Nothing -> Nothing
  Just (sep, i1) -> case parseNumber s i1 of
    Nothing -> Nothing
    Just (v, i2) -> Just (TE v sep, i2)

skipIndex :: String -> Int -> Int
skipIndex s i
  | i < length s && s !! i == ':' =
      let j = skipSpaces s (i + 1)
       in walk j
  | otherwise = i
  where
    walk k = if k < length s && isDigitC (s !! k) then walk (k + 1) else k

parseColumn :: String -> Int -> Maybe (TColumn, Int)
parseColumn s i0 =
  let i1 = skipSpaces s i0
   in if i1 >= length s || s !! i1 /= '(' then Nothing
      else go (skipSpaces s (i1 + 1)) []
  where
    go i acc
      | i < length s && s !! i /= ')' && s !! i /= ':' =
          case parseEntry s i of
            Nothing -> Nothing
            Just (e, j) -> go (skipSpaces s j) (acc ++ [e])
      | otherwise =
          let k = skipIndex s i
              k2 = skipSpaces s k
           in if k2 >= length s then Nothing else Just (acc, k2 + 1)

parseExpr :: String -> Int -> Maybe (TExpr, Int)
parseExpr s i0 = go (skipSpaces s i0) []
  where
    go i acc
      | i < length s && s !! i == '(' = case parseColumn s i of
          Nothing -> Nothing
          Just (col, j) -> go (skipSpaces s j) (acc ++ [col])
      | otherwise = Just (acc, i)

fromDisplayT :: String -> Maybe TExpr
fromDisplayT raw = case parseExpr raw (skipSpaces raw 0) of
  Nothing -> Nothing
  Just (e, i) -> if skipSpaces raw i == length raw then Just e else Nothing

trimWS :: String -> String
trimWS = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

expandImpl :: Bool -> String -> Int -> String
expandImpl longer text term0 =
  let term = max 1 term0
      raw = trimWS text
   in if raw == "Limit" || raw == "limit" || raw == "LIMIT"
        then mountainDisplay' (infinityFs term) False
        else if null raw || raw == "0" || raw == "∅"
          then ""
          else case fromDisplayT raw of
            Nothing -> errText
            Just m
              | null m -> ""
              | not (isLimit m) -> mountainDisplay' (init m) False
              | otherwise -> mountainDisplay' (expand m term (not longer)) False

tOmegaMnNotation :: Notation
tOmegaMnNotation =
  textAlterNotation "t-omega-mn" "TωMN" []
    (expandImpl False)
    (expandImpl True)
    ""
