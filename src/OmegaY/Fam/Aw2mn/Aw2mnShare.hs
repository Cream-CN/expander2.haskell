-- | Aω2MN 族共享核心（移植自 aw2mn_share.hpp）。
module OmegaY.Fam.Aw2mn.Aw2mnShare
  ( Entry(..)
  , Column
  , Expr
  , Sep
  , Vertical
  , VV
  , Parsed(..)
  , SubtractKind(..)
  , kParseFail
  , mountainDisplay
  , parse
  , expand
  , mountainIsLimit
  , mountainIsOne
  , validValueRange
  ) where

import Data.List (foldl')

-- 数据类型

data Entry = Entry
  { eValue :: Int
  , eSep :: Sep
  , eMark :: Bool
  } deriving (Eq, Show)

type Column = [Entry]
type Expr = [Column]
type Sep = Expr
type Vertical = [Sep]
type VV = [[Vertical]]

data Parsed = Parsed
  { pExpr :: Expr
  } deriving (Eq, Show)

data SubtractKind = V2 | V3
  deriving (Eq, Show)

kParseFail :: String
kParseFail = "解析失败，请确认你没有使用全角括号，逗号，分号"

-- 比较

numCompare :: Int -> Int -> Int
numCompare a b = case compare a b of { LT -> -1; EQ -> 0; GT -> 1 }

lexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
lexCompare a b cmp = go 0
  where
    n = min (length a) (length b)
    go i
      | i >= n = numCompare (length a) (length b)
      | otherwise =
          let r = cmp (a !! i) (b !! i)
           in if r /= 0 then r else go (i + 1)

entryCompare :: Entry -> Entry -> Int
entryCompare a b =
  let r = numCompare (eValue a) (eValue b)
   in if r /= 0 then r else mountainCompare (eSep a) (eSep b)

columnCompare :: Column -> Column -> Int
columnCompare a b = lexCompare a b entryCompare

mountainCompare :: Expr -> Expr -> Int
mountainCompare a b = lexCompare a b columnCompare

verticalCompare :: Vertical -> Vertical -> Int
verticalCompare a b = lexCompare a b mountainCompare

-- 谓词

mountainIsLimit :: Expr -> Bool
mountainIsLimit m = not (null m) && not (null (last m))

mountainIsOne :: Expr -> Bool
mountainIsOne m = length m == 1 && null (head m)

-- 显示

isOmegaSep :: Sep -> Bool
isOmegaSep sep =
  length sep >= 2 && null (sep !! 0) &&
  length (sep !! 1) == 1 &&
  let e = (sep !! 1) !! 0
   in eValue e == 1 && not (eMark e) &&
      length (eSep e) == 1 && null (eSep e !! 0) &&
      all (\k -> null (sep !! k)) [2 .. length sep - 1]

sepDisplay :: Sep -> String
sepDisplay sep =
  if all null sep
    then replicate (length sep) ','
    else if isOmegaSep sep
      then ';' : replicate (max 0 (length sep - 2)) ','
      else mountainDisplay sep

entryDisplay :: Entry -> String
entryDisplay e = sepDisplay (eSep e) ++ (if eMark e then "*" else "") ++ show (eValue e)

columnDisplay :: Column -> String
columnDisplay c = "(" ++ concatMap entryDisplay c ++ ")"

mountainDisplay :: Expr -> String
mountainDisplay m = concatMap columnDisplay m

-- 解析

omegaSep :: Sep
omegaSep = [[], [Entry 1 [[]] False]]

parse :: String -> Maybe Parsed
parse s0 = case go s0 0 of
      Just (cols, _) -> Just (Parsed cols)
      Nothing -> Nothing
  where
    go s i =
      let i1 = skipSpaces s i
       in if i1 >= length s
            then Just ([], i1)
            else do
              (col, i2) <- if s !! i1 == '(' then parseParenCol s i1 else parseUnparenCol s i1
              let i3 = skipSpaces s i2
              (rest, i4) <- go s i3
              Just (col : rest, i4)

    skipSpaces s i = let n = length s in if i < n && s !! i == ' ' then skipSpaces s (i + 1) else i

    skipIndex s i =
      let n = length s
       in if i < n && s !! i == ':'
            then let i1 = i + 1
                     i2 = skipSpaces s i1
                     n2 = length s
                  in walk i2
            else i
      where
        walk i = let n = length s in if i < n && s !! i >= '0' && s !! i <= '9' then walk (i + 1) else i

    parseSep s i =
      let n = length s
       in if i < n && s !! i == ';'
            then let i1 = i + 1
                     c0 = countCommas s i1
                  in Just (omegaSep ++ replicate c0 [], i1 + c0)
            else let c0 = countCommas s i
                  in if c0 == 0
                       then Nothing
                       else Just (replicate c0 [], i + c0)
      where
        countCommas s' i' =
          let n' = length s'
           in if i' < n' && s' !! i' == ',' then 1 + countCommas s' (i' + 1) else 0

    parseNumber s i =
      let n = length s
          start = i
       in if i >= n || not (s !! i >= '0' && s !! i <= '9')
            then Nothing
            else
              let j = walk i
                  walk k = if k < n && s !! k >= '0' && s !! k <= '9' then walk (k + 1) else k
                  digits = take (j - start) (drop start s)
                  v = foldl' (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 digits
               in Just (v, j)

    parseEntry s i = do
      let i1 = skipSpaces s i
      (sep, i2) <- parseSep s i1
      let i3 = skipSpaces s i2
          n = length s
          (mark, i4) = if i3 < n && s !! i3 == '*' then (True, i3 + 1) else (False, i3)
      (v, i5) <- parseNumber s i4
      return (Entry v sep mark, i5)

    parseParenCol s i = do
      let n = length s
      if i >= n || s !! i /= '(' then Nothing
      else do
        let i1 = i + 1
            i2 = skipSpaces s i1
        (col, i3) <- parseEntries s i2
        let i4 = skipIndex s i3
            i5 = skipSpaces s i4
        if i5 >= n || s !! i5 /= ')' then Nothing
        else Just (col, i5 + 1)

    parseEntries s i =
      let n = length s
       in if i >= n || s !! i == ')' || s !! i == ':'
            then Just ([], i)
            else do
              (e, i1) <- parseEntry s i
              let i2 = skipSpaces s i1
              (rest, i3) <- parseEntries s i2
              Just (e : rest, i3)

    parseUnparenCol s i =
      let n = length s
          i1 = skipSpaces s i
       in if i1 >= n then Nothing
          else if s !! i1 == '0' &&
                  (i1 + 1 >= n || s !! (i1 + 1) == ':' || s !! (i1 + 1) == ' ' ||
                   s !! (i1 + 1) == '(' || s !! (i1 + 1) == ',' || s !! (i1 + 1) == ';')
            then let i2 = i1 + 1
                     i3 = skipIndex s i2
                  in Just ([], i3)
          else if s !! i1 == ':'
            then let i2 = skipIndex s i1
                  in Just ([], i2)
            else do
              (col, i2) <- parseUnparenEntries s i1
              let i3 = skipIndex s i2
              Just (col, i3)

    parseUnparenEntries s i =
      let n = length s
       in if i >= n || s !! i == ' ' || s !! i == '(' || s !! i == ':'
            then Just ([], i)
            else do
              (e, i1) <- parseEntry s i
              let i2 = skipSpaces s i1
              (rest, i3) <- parseUnparenEntries s i2
              Just (e : rest, i3)

-- 山结构辅助

verticalIncrease :: Vertical -> Sep -> Vertical
verticalIncrease v m =
  let i = length v - 1
      i' = findLast i
      findLast k
        | k < 0 = -1
        | mountainCompare (v !! k) m < 0 = findLast (k - 1)
        | otherwise = k
   in take (i' + 1) v ++ [m]

columnVerticals :: Column -> [Vertical]
columnVerticals c =
  let loop [] _ = []
      loop (e:es) prev =
        let v = verticalIncrease prev (eSep e)
         in v : loop es v
   in loop c []

findIndexBelowRow :: [Vertical] -> Vertical -> Int
findIndexBelowRow verticals y =
  let loop l r =
        if l >= r then l
        else
          let m = (l + r + 1) `div` 2
              wm = if m == 0 then [] else verticals !! (m - 1)
           in if verticalCompare wm y < 0 then loop m r else loop l (m - 1)
   in loop 0 (length verticals)

parent :: Expr -> VV -> (Int, Int) -> (Int, Int)
parent a v (i, j) =
  let targetColumn = eValue ((a !! i) !! j) - 1
      targetJ = findIndexBelowRow (v !! targetColumn) ((v !! i) !! j)
   in (targetColumn, targetJ)

-- S 函数

sFunc :: Expr -> Int -> Int -> Sep
sFunc a i j
  | i < 0 || i >= length a || j < 0 || j >= length (a !! i) = []
  | otherwise =
      let e = (a !! i) !! j
       in if eMark e then eSep e else sFunc a i (j - 1)

-- 工具：更新 Expr 的指定列

updateColumn :: Expr -> Int -> Column -> Expr
updateColumn a i newCol = take i a ++ [newCol] ++ drop (i + 1) a

-- subtract1

-- 返回 (新 Expr, 追加到 rightmost 的 entries 列表)
subtract1 :: Expr -> VV -> SubtractKind -> (Expr, [Entry])
subtract1 a0 v0 kind =
  let rightmost = length a0 - 1
      topmost = length (a0 !! rightmost) - 1
      topRightValue = eValue ((a0 !! rightmost) !! topmost)
      topRightSep = eSep ((a0 !! rightmost) !! topmost)
      (brI, brJ) = parent a0 v0 (rightmost, topmost)

      -- pop_back
      aPopped = updateColumn a0 rightmost (take topmost (a0 !! rightmost))

      alpha = if brJ - 1 >= 0 then (v0 !! brI) !! (brJ - 1) else []
      workingVertical0 = if topmost - 1 >= 0 then (v0 !! rightmost) !! (topmost - 1) else []

      -- copy_entries_loop: 收集要追加的 entries，并更新 working vertical
      collectEntries :: Vertical -> (Vertical, [Entry])
      collectEntries wv
        | verticalCompare alpha wv <= 0 = (wv, [])
        | otherwise =
            let i0 = if null wv then -1 else findIndexBelowRow (v0 !! brI) wv
                loop i wv' entries =
                  if i + 1 >= brJ
                    then (wv', reverse entries)
                    else
                      let src = (a0 !! brI) !! (i + 1)
                          newEntry = Entry topRightValue (eSep src) (eMark src)
                          wv'' = verticalIncrease wv' (eSep src)
                       in loop (i + 1) wv'' (newEntry : entries)
             in loop i0 wv []
  in case kind of
       V2 ->
         if mountainIsLimit topRightSep
           then
             let brSep = sFunc a0 brI (brJ - 1)
                 j = if mountainCompare brSep topRightSep >= 0
                       then [[]]
                       else brSep ++ [[]]
                 workingVertical = workingVertical0
                 (wv', entries) = collectEntries workingVertical
              in if verticalCompare (verticalIncrease alpha j) workingVertical > 0
                   then
                     let (wv'', entries'') = collectEntries workingVertical
                         finalEntry = Entry topRightValue j True
                      in (aPopped, entries'' ++ [finalEntry])
                   else (aPopped, [])
           else if not (mountainIsOne topRightSep)
             then
               let truncated = if null topRightSep then [] else init topRightSep
                   workingVertical = workingVertical0
                   (wv', entries) = collectEntries workingVertical
                in if verticalCompare alpha wv' == 0
                     then (aPopped, entries ++ [Entry topRightValue truncated False])
                     else (aPopped, [])
             else (aPopped, [])
       V3 ->
         if mountainIsOne topRightSep
           then (aPopped, [])
           else
             let workingVertical = workingVertical0
                 (wv', entries) = collectEntries workingVertical
              in if mountainIsLimit topRightSep
                   then
                     let brSep = sFunc a0 brI (brJ - 1)
                         j0 = if mountainCompare brSep topRightSep >= 0
                                then [[]]
                                else brSep ++ [[]]
                         loop j wv =
                           if verticalCompare (verticalIncrease alpha j) (verticalIncrease wv j) == 0
                             then j
                             else loop (j ++ [[]]) wv
                         jFinal = loop j0 wv'
                      in (aPopped, entries ++ [Entry topRightValue jFinal True])
                   else
                     let truncated = if null topRightSep then [] else init topRightSep
                      in if verticalCompare (verticalIncrease alpha truncated) wv' > 0
                           then (aPopped, entries ++ [Entry topRightValue truncated False])
                           else (aPopped, [])

-- get_references

getReferences :: Expr -> [Vertical] -> (Int, [Maybe Int])
getReferences a rtops =
  let verticals = columnVerticals (last a)
      verticals' = [] : verticals  -- insert empty at beginning
      n = length rtops
      -- 对齐 C++：命中时覆盖当前 rtops[j] 槽位（j 不前进），未命中时封存并 j++。
      -- doneRev 头部 = rtops[j-1]；pending 为当前 rtops[j] 累积值。
      loop i j doneRev pending len
        | i >= length verticals' || j >= n =
            let rest = replicate (n - j - 1) Nothing
                final = reverse (pending : doneRev) ++ rest
             in (len, final)
        | verticalCompare (verticals' !! i) (rtops !! j) < 0 =
            loop (i + 1) j doneRev (Just i) (j + 1)
        | otherwise =
            loop i (j + 1) (pending : doneRev) Nothing len
   in loop 0 0 [] Nothing 0

-- extend

extend :: Expr -> Bool -> Bool -> SubtractKind -> Expr
extend a0 isSmall weak kind =
  let rightmost = length a0 - 1
      topmost = length (a0 !! rightmost) - 1
   in if topmost < 0
        then a0  -- empty rightmost column, nothing to extend
        else extend' a0 isSmall weak kind rightmost topmost

extend' :: Expr -> Bool -> Bool -> SubtractKind -> Int -> Int -> Expr
extend' a0 isSmall weak kind rightmost topmost =
  let v0 = map columnVerticals a0
      (brI, brJ) = parent a0 v0 (rightmost, topmost)

      topSeparators =
        [ eSep ((a0 !! brI) !! k) | k <- [0 .. brJ - 1] ] ++
        [ eSep ((a0 !! rightmost) !! topmost) ]

      topVerticals =
        [ (v0 !! brI) !! k | k <- [0 .. brJ - 1] ] ++
        [ (v0 !! rightmost) !! topmost ]

      width = rightmost - brI

      -- magma_checks_list
      magmaChecksList =
        let brRow = replicate (length (a0 !! brI)) (-1)
            computeRow i =
              let col = a0 !! i
               in [ computeJ i j | j <- [0 .. length col - 1] ]
            computeJ i j =
              let loop (wi, wj) =
                    if wi <= brI
                      then (wi, wj)
                      else
                        let rowLen = length (a0 !! wi)
                            wj' = if wj >= rowLen then wj - 1 else wj
                         in if wj' < 0
                              then (wi, wj')
                              else loop (parent a0 v0 (wi, wj'))
                  (workingI, workingJ) = loop (i, j)
               in if workingI /= brI
                    then -1
                    else
                      let leftV = if workingJ - 1 >= 0 then (v0 !! workingI) !! (workingJ - 1) else []
                          rightV = if j == 0 then [] else (v0 !! i) !! (j - 1)
                       in if workingJ <= brJ && verticalCompare leftV rightV == 0
                            then workingJ
                            else -1
            rows = [ if i == brI then brRow else computeRow i | i <- [0 .. rightmost] ]
         in rows

      -- subtract1
      (aAfterSub, addedEntries) = subtract1 a0 v0 kind
      a = updateColumn aAfterSub rightmost ((aAfterSub !! rightmost) ++ addedEntries)

      -- get_references
      (refsLength, refs) = getReferences a topVerticals

      refValue idx
        | idx < 0 = Just (-1)
        | idx >= length refs = Nothing
        | otherwise = refs !! idx

      -- stretch_threshold, stretch_value
      nTop = length topSeparators
      (stretchThreshold, stretchValue) =
        let loop i (thAcc, svAcc)
              | i >= nTop = (reverse thAcc, reverse svAcc)
              | otherwise =
                  let ts = topSeparators !! i
                   in if not (mountainIsLimit ts)
                        then loop (i + 1) ([[]] : thAcc, 0 : svAcc)
                        else
                          let sI = sFunc a0 brI (i - 1)
                              threshold = if mountainCompare sI ts >= 0 then [[]] else sI ++ [[]]
                              sv = computeSV i threshold
                           in loop (i + 1) (threshold : thAcc, sv : svAcc)
            computeSV i threshold =
              let hasRef = if i == 0
                             then case refValue 0 of { Just _ -> True; Nothing -> False }
                             else case (refValue i, refValue (i - 1)) of
                                    (Just _, Just _) -> True
                                    _ -> False
               in if not hasRef
                    then 0
                    else
                      let jStart = case refValue i of { Just v -> v; Nothing -> 0 }
                          cmpRef = if i == 0 then -1 else case refValue (i - 1) of { Just v -> v; Nothing -> -1 }
                          loopJ j
                            | j - 1 <= cmpRef = 0
                            | otherwise =
                                let m = if j - 1 >= 0 && j - 1 < length (a !! rightmost)
                                          then eSep ((a !! rightmost) !! (j - 1))
                                          else []
                                 in if mountainCompare m (topSeparators !! i) < 0 &&
                                       mountainCompare m threshold >= 0
                                      then length m - length threshold + 1
                                      else loopJ (j - 1)
                       in loopJ jStart
         in loop 0 ([], [])

      stretched br entrySep x y
        | br < 0 = entrySep
        | sv == 0 = entrySep
        | weak && verticalCompare ((v0 !! x) !! y) (topVerticals !! br) >= 0 = entrySep
        | mountainCompare entrySep (topSeparators !! br) >= 0 = entrySep
        | mountainCompare entrySep (stretchThreshold !! br) < 0 = entrySep
        | otherwise = entrySep ++ replicate sv []
        where sv = if br < nTop then stretchValue !! br else 0

      -- Main column-copying loop
      maxDx = if isSmall then 0 else width
      fromJust' (Just v) = v
      finalA = loopDx 0 a
      loopDx dx curA
        | dx > maxDx = curA
        | otherwise = loopDx (dx + 1) (applyDx dx curA)
      applyDx dx curA =
        let x = brI + dx
            sourceMagmas = magmaChecksList !! x
            targetIdx = x + width
            brIndex0 = if dx == 0 then refsLength - 1 else -1
            srcCol = a0 !! x
            refValue idx
              | idx < 0 = Just (-1)
              | idx >= length refs = Nothing
              | otherwise = refs !! idx
            buildCol col magmas brIdx =
              let loop y brIdx'
                    | y >= length col = []
                    | dx == 0 && y < brJ = loop (y + 1) brIdx'
                    | otherwise =
                        let entry = col !! y
                            value = eValue entry
                            magma = magmas !! y
                         in if magma /= -1
                              then
                                let brIdx'' = magma
                                 in case refValue brIdx'' of
                                      Nothing -> loop (y + 1) brIdx''
                                      Just _ ->
                                        let hasPrev = brIdx'' == 0 || case refValue (brIdx'' - 1) of { Just _ -> True; Nothing -> False }
                                         in if not hasPrev
                                              then loop (y + 1) brIdx''
                                              else
                                                let jStart = (if brIdx'' == 0 then -1 else fromJust' (refValue (brIdx'' - 1))) + 1
                                                    jEnd = fromJust' (refValue brIdx'')
                                                    entries = [ if j == jEnd
                                                                  then Entry (value + width) (stretched brIdx'' (eSep entry) x y) (eMark entry)
                                                                  else let srcEntry = (curA !! (brI + width)) !! j
                                                                        in Entry (value + width) (eSep srcEntry) (eMark srcEntry)
                                                               | j <- [jStart .. jEnd] ]
                                                in entries ++ loop (y + 1) brIdx''
                              else
                                let newValue = value + (if value > brI then width else 0)
                                 in Entry newValue (stretched brIdx' (eSep entry) x y) (eMark entry) : loop (y + 1) brIdx'
                 in loop 0 brIndex0
            newCol = buildCol srcCol sourceMagmas brIndex0
            aWithNewCol = if dx > 0 then curA ++ [[]] else curA
            existingCol = if targetIdx < length aWithNewCol then aWithNewCol !! targetIdx else []
            appendedCol = existingCol ++ newCol
            updatedCol = if targetIdx < length aWithNewCol
                           then take targetIdx aWithNewCol ++ [appendedCol] ++ drop (targetIdx + 1) aWithNewCol
                           else aWithNewCol ++ [appendedCol]
         in updatedCol
   in finalA

-- expand

expand :: Expr -> Int -> Bool -> Bool -> SubtractKind -> Expr
expand a0 index shorter weak kind =
  let loop n a
        | n > index = a
        | otherwise = loop (n + 1) (extend a False weak kind)
      a1 = loop 1 a0
   in if shorter
        then take (length a1 - 1) a1
        else extend a1 True weak kind

-- 值域校验

validValueRange :: Expr -> Bool
validValueRange e =
  let sz = length e
   in all (\col -> all (\en -> eValue en >= 1 && eValue en <= sz) col) e
