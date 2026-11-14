{-# LANGUAGE OverloadedStrings #-}
-- | mnup2 共享实现（up2dbms v1 / v1b+ / v1c）。忠实移植 mnup2share.hpp + 其依赖的
--   smnshare from_display / mountain_display 片段。
module OmegaY.Fam.Dbms.MnUp2
  ( Expr
  , Col
  , parseExpr
  , displayExpr
  , exprValid
  , infinityFs
  , dbmsExpand
  , up2mnInfinityFs
  , up2mnExpand
  , exprVert
  , parents
  , copyCol
  , setAt
  , toRelCol
  , relColCmp
  , mnComputeUp1
  , up2mnCopyCol
  ) where

import OmegaY.Fam.Dbms.Common

-- Entry = (value 0-based, sep)
type Entry = (Int, Int)
type Col = [Entry]
type Expr = [Col]
type Vertical = [Int]
type Pos = (Int, Int)

-- ---------------------------------------------------------------- smnshare 片段
-- from_display（非 simple）
smnFromDisplay :: String -> Maybe Expr
smnFromDisplay s
  | s == "Limit" = Nothing
  | otherwise = go (skipSp s)
  where
    go t =
      case t of
        [] -> Just []
        ('(' : _) -> do
          (c, t') <- parenCol t
          rest <- go (skipSp t')
          Just (c : rest)
        _ -> do
          (c, t') <- unparenCol t
          rest <- go (skipSp t')
          Just (c : rest)
    parseSep t =
      let (cs, r) = span (== ',') t
          n = length cs
      in (if n == 0 then 0 else n - 1, r)
    parseNum' t =
      let (ds, r) = span (`elem` ("0123456789" :: String)) t
      in if null ds then Nothing else Just (read ds, r)
    parenCol t0 = do
      s1 <- expect '(' t0
      let s2 = skipSp s1
      (col, s3) <- entries s2
      let s4 = skipIdx (skipSp s3)
      s5 <- expect ')' s4
      Just (col, s5)
    entries t =
      case t of
        (')' : _) -> Just ([], t)
        (':' : _) -> Just ([], t)
        [] -> Just ([], t)
        _ -> do
          let (sep, t1) = parseSep (skipSp t)
          (v, t2) <- parseNum' t1
          more [(v, sep)] (skipSp t2)
    more acc t =
      case t of
        (')' : _) -> Just (reverse acc, t)
        (':' : _) -> Just (reverse acc, t)
        [] -> Just (reverse acc, t)
        _ -> do
          let (sep, t1) = parseSep (skipSp t)
          (v, t2) <- parseNum' t1
          more ((v, sep) : acc) (skipSp t2)
    unparenCol t0 =
      let s1 = skipSp t0
       in case s1 of
            ('0' : rest) ->
              if null rest || head rest == ':' || head rest == ' ' || head rest == '('
                then Just ([], skipIdx rest)
                else goUnparen [] s1
            _ -> goUnparen [] s1
    goUnparen acc t =
      case t of
        [] -> Just (reverse acc, t)
        (' ' : _) -> Just (reverse acc, skipIdx t)
        ('(' : _) -> Just (reverse acc, skipIdx t)
        (':' : _) -> Just (reverse acc, skipIdx t)
        (',' : _) -> do
          let (sep, t1) = parseSep t
          (v, t2) <- parseNum' t1
          goUnparen ((v, sep) : acc) t2
        _ -> Nothing

-- mountain_display(m, false)
displayExpr :: Expr -> String
displayExpr = concatMap colD
  where
    colD c = "(" ++ concatMap entryD c ++ ")"
    entryD (v, sep) = replicate (sep + 1) ',' ++ show (v + 1)

-- mnup2::from_display: smn from_display then value-=1
parseExpr :: String -> Maybe Expr
parseExpr s = do
  m <- smnFromDisplay s
  Just [[(v - 1, sep) | (v, sep) <- col] | col <- m]

exprValid :: Expr -> Bool
exprValid e =
  let n = length e
   in all (\col -> all (\(v, _) -> v >= 0 && v < n) col) e

-- ---------------------------------------------------------------- 比较
entryCmp :: Entry -> Entry -> Int
entryCmp (v1, s1) (v2, s2) =
  case cmpI v1 v2 of
    0 -> cmpI s1 s2
    c -> c

colCmp :: Col -> Col -> Int
colCmp a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = entryCmp (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

-- vertical_compare（anti-lex）：从尾部对齐比较
vertCmp :: Vertical -> Vertical -> Int
vertCmp a b =
  let n = min (length a) (length b)
      ai = [length a - 1 - k | k <- [0 .. n - 1]]
      bi = [length b - 1 - k | k <- [0 .. n - 1]]
   in case [ c
           | (x, y) <- zip ai bi
           , let c = cmpI (a !! x) (b !! y)
           , c /= 0
           ] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

-- vertical_increase
vertInc :: Vertical -> Int -> Vertical
vertInc v sep
  | length v <= sep = replicate sep 0 ++ [1]
  | otherwise =
      let r = [if k < sep then 0 else (v !! k) | k <- [0 .. length v - 1]]
          idx = sep
          rv = r !! idx + 1
       in [if k == idx then rv else r !! k | k <- [0 .. length v - 1]]

colVert :: Col -> [Vertical]
colVert c = go [] c
  where
    go cur [] = []
    go cur ((_, sep) : rest) =
      let v = vertInc cur sep
       in v : go v rest

exprVert :: Expr -> [[Vertical]]
exprVert = map colVert

findBelowRow :: [Vertical] -> Vertical -> Int
findBelowRow vv v =
  let n = length vv
   in if n == 0
        then 0
        else if vertCmp v (vv !! (n - 1)) > 0
          then n
          else binSearch 0 n
  where
    binSearch l r =
      if l >= r
        then l
        else
          let mid = (l + r + 1) `div` 2
              w = if mid == 0 then [] else vv !! (mid - 1)
           in if vertCmp v w > 0 then binSearch mid r else binSearch l (mid - 1)

parents :: Expr -> [[Vertical]] -> [[Pos]]
parents expr vv =
  [ [ let (v, _) = expr !! i !! j
       in (v, findBelowRow (vv !! v) (vv !! i !! j))
    | j <- [0 .. length (expr !! i) - 1]
    ]
  | i <- [0 .. length expr - 1]
  ]

-- to_rel_column
type RelEntry = (Bool, Int, Int) -- flag, value, sep

relEntry :: Int -> Entry -> RelEntry
relEntry r (v, sep) = (v >= r, (if v >= r then v - r else v), sep)

relEntryCmp :: RelEntry -> RelEntry -> Int
relEntryCmp (f1, v1, s1) (f2, v2, s2) =
  case cmpBool f1 f2 of
    0 -> case cmpI v1 v2 of
           0 -> cmpI s1 s2
           c -> c
    c -> c

cmpBool :: Bool -> Bool -> Int
cmpBool a b = if a == b then 0 else if a then 1 else -1

relColCmp :: [RelEntry] -> [RelEntry] -> Int
relColCmp a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = relEntryCmp (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

toRelCol :: Col -> Int -> [RelEntry]
toRelCol c r = map (relEntry r) c

-- ---------------------------------------------------------------- compute_up
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- v1 的 finite：直接 fill(true, Ri)
v1ComputeUp1 :: Expr -> Pos -> [Int]
v1ComputeUp1 expr (ri, _) =
  [if k < ri then 0 else 1 | k <- [0 .. length expr - 1]]

-- up2mn_compute_up_1mn（v1b+ / v1c finite 分支）
mnComputeUp1 :: Expr -> [[Pos]] -> Pos -> [Int]
mnComputeUp1 expr p (ri, rj) = build
  where
    n = length expr
    right = n - 1
    initial = [if k < ri then 0 else if k == ri then 1 else (-1) | k <- [0 .. n - 1]]
    build
      | rj == 0 = [if k < ri then 0 else 1 | k <- [0 .. n - 1]]
      | otherwise = foldl applyIdx initial [ri + 1 .. n - 1]
    applyIdx rs i =
      let col = expr !! i
       in if length col <= rj
            then setAt i 0 rs
            else
              let pi = fst ((p !! i) !! rj)
               in if pi /= ri
                    then setAt i (rs !! pi) rs
                    else
                      case firstVal col ri of
                        Nothing -> setAt i 0 rs
                        Just j ->
                          if j > 0
                            then
                              let pj = walkJ i j
                               in if rs !! pj == 0
                                    then setAt i 0 rs
                                    else upCheck rs i j
                            else upCheck rs i j
    upCheck rs i j =
      let yStart = walkY right j
       in if yStart <= i
            then setAt i (if i == yStart then 1 else 0) rs
            else
              let x0 = drop j (expr !! i)
                  y0 = drop j (expr !! yStart)
                  c0 = colCmp x0 y0
               in if c0 /= 0
                    then setAt i (if c0 > 0 then 1 else 0) rs
                    else
                      let (dec, rs2) = loopUp i yStart 1 rs
                       in if dec then rs2 else setAt i 1 rs2
    firstVal col target =
      case [k | k <- [0 .. length col - 1], fst (col !! k) == target] of
        (k : _) -> Just k
        [] -> Nothing
    walkJ start j =
      let n = length expr
       in goGuard (n + 1) start
      where
        goGuard 0 cur = cur
        goGuard guard cur =
          let pi = fst ((p !! cur) !! (j - 1))
           in if pi == ri then cur else goGuard (guard - 1) pi
    walkY start j =
      let n = length expr
       in goGuard (n + 1) start
      where
        goGuard 0 cur = cur
        goGuard guard cur =
          let pi = fst ((p !! cur) !! j)
           in if pi == ri then cur else goGuard (guard - 1) pi
    loopUp xStart yStart k rs =
      if yStart + k >= length expr
        then (False, rs)
        else
          let xk = toRelCol (expr !! (xStart + k)) xStart
              yk = toRelCol (expr !! (yStart + k)) yStart
              c = relColCmp xk yk
           in if c /= 0
                then (True, setAt xStart (if c > 0 then 1 else 0) rs)
                else loopUp xStart yStart (k + 1) rs

-- up2dbms_compute_up_2mn（!is_finite 分支）
dbmsComputeUp2 :: Expr -> [[Pos]] -> Pos -> Bool -> [Int]
dbmsComputeUp2 expr p (ri, rj) v1c = build
  where
    n = length expr
    right = n - 1
    initial = [if k < ri then 0 else if k == ri then 1 else (-1) | k <- [0 .. n - 1]]
    build = foldl applyIdx initial [ri + 1 .. n - 1]
    applyIdx rs i =
      let col = expr !! i
       in if length col <= rj + 1
            then setAt i 0 rs
            else if length col >= rj + 3
              then setAt i (rs !! (fst ((p !! i) !! (rj + 1)))) rs
              else
                let isFinite = snd (last col) == 0
                 in if isFinite
                      then setAt i (rs !! (fst ((p !! i) !! (rj + 1)))) rs
                      else
                        let pi = fst ((p !! i) !! (rj + 1))
                         in if pi /= ri
                              then setAt i (rs !! pi) rs
                              else
                                let yStart = walkY right
                                 in if yStart <= i
                                      then setAt i (if i == yStart then 1 else 0) rs
                                      else
                                        let (dec, rs2) = loopUp i yStart 1 rs
                                         in if dec then rs2 else setAt i 1 rs2
    walkY start =
      let n = length expr
       in goGuard (n + 1) start
      where
        goGuard 0 cur = cur
        goGuard guard cur =
          let stop =
                if v1c
                  then length (expr !! cur) /= rj + 2 || fst ((p !! cur) !! rj) /= ri
                  else length (expr !! cur) /= rj + 2
           in if not stop then cur else goGuard (guard - 1) (fst ((p !! cur) !! (if v1c then rj else rj + 1)))
    loopUp xStart yStart k rs =
      if yStart + k >= length expr
        then (False, rs)
        else
          let xk = toRelCol (expr !! (xStart + k)) xStart
              yk = toRelCol (expr !! (yStart + k)) yStart
              c = relColCmp xk yk
           in if c /= 0
                then (True, setAt xStart (if c > 0 then 1 else 0) rs)
                else loopUp xStart yStart (k + 1) rs

-- copy_column
copyCol :: Col -> Pos -> Int -> Int -> Bool -> Bool -> Col
copyCol col (ri, rj) offset yOffset up v1flag =
  let result = zipWith entryAt [0 ..] col
      entryAt j (v, sep) =
        let nv =
              if v > ri
                then v + offset
                else if v < ri
                  then v
                  else if up || (v1flag && j <= rj)
                    then v + offset
                    else v
         in (nv, sep)
   in if up && yOffset > 0 && rj < length result
        then
          let fillVal = fst (result !! rj)
              head = take rj result
              tail = drop rj result
              head' = head ++ [ (fillVal, 0) | _ <- [1 .. yOffset] ]
           in head' ++ tail
        else result

-- infinity_fs
infinityFs :: Int -> Expr
infinityFs index =
  [] : [ [ (i - 1, 0) | _ <- [1 .. i] ] ++ [ (i - 1, 1) ] | i <- [1 .. index] ]

-- dbmsExpand
dbmsExpand :: Expr -> Int -> Bool -> Bool -> Bool -> Expr
dbmsExpand expr index shorter v1flag v1c
  | null expr = expr
  | null (last expr) = init expr
  | otherwise = result
  where
    n = length expr
    right = n - 1
    rc = expr !! right
    top = length rc - 1
    vv = exprVert expr
    p = parents expr vv
    rp = (p !! right) !! top
    (ri, rj) = rp
    isFinite = snd (rc !! top) == 0
    upList =
      if isFinite
        then
          if v1flag
            then v1ComputeUp1 expr rp
            else mnComputeUp1 expr p rp
        else dbmsComputeUp2 expr p rp v1c
    result0 = init expr
    newCol0 = init rc
    newCol = newCol0 ++ drop rj (expr !! ri)
    result1 = result0 ++ [newCol]
    yOffset = top - rj
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [ri + 1 .. right]
    addOne w rs i =
      let off = (right - ri) * w
       in rs ++ [copyCol (rs !! i) rp off (yOffset * w) (upList !! i /= 0) v1flag]
    result
      | shorter = init result2
      | otherwise = result2

-- =====================================================================
-- up2mn 专用（v1b / v1b-plus）
-- =====================================================================

up2mnInfinityFs :: Int -> Bool -> Expr
up2mnInfinityFs index plus =
  [[]] ++ [ oneCol i | i <- [1 .. index] ]
  where
    oneCol i =
      if plus
        then [(i - 1, 1)]
        else [ (i - 1, 0) | _ <- [1 .. i - 1] ] ++ [(0, 1)]

up2mnCopyCol :: Col -> Pos -> Int -> Int -> Bool -> Col
up2mnCopyCol col (ri, rj) offset yOffset up =
  let result = [ (if v > ri then v + offset else if v < ri then v else if up then v + offset else v, sep)
               | (v, sep) <- col ]
   in if up && yOffset > 0 && rj < length result
        then
          let fillVal = fst (result !! rj)
              head = take rj result
              tail = drop rj result
              head' = head ++ [ (fillVal, 0) | _ <- [1 .. yOffset] ]
           in head' ++ tail
        else result

up2mnComputeUp2 :: Expr -> [[Pos]] -> Pos -> Bool -> [Int]
up2mnComputeUp2 expr p (ri, rj) plus = build
  where
    n = length expr
    right = n - 1
    initial = [if k < ri then 0 else if k == ri then 1 else (-1) | k <- [0 .. n - 1]]
    build = foldl applyIdx initial [ri + 1 .. n - 1]
    applyIdx rs i =
      let col = expr !! i
       in if length col <= rj
            then setAt i 0 rs
            else if length col >= rj + 2
              then setAt i (rs !! (fst ((p !! i) !! rj))) rs
              else
                let isFinite = snd (last col) == 0
                 in if isFinite
                      then setAt i 0 rs
                      else
                        let pi = fst ((p !! i) !! rj)
                         in if pi /= ri
                              then setAt i (if plus then rs !! pi else 0) rs
                              else
                                let yStart = walkY right
                                 in if yStart <= i
                                      then setAt i (if i == yStart then 1 else 0) rs
                                      else
                                        let (dec, rs2) = loopUp i yStart 1 rs
                                         in if dec then rs2 else setAt i 1 rs2
    walkY start =
      let n = length expr
       in goGuard (n + 1) start
      where
        goGuard 0 cur = cur
        goGuard guard cur =
          if length (expr !! cur) /= rj + 1
            then goGuard (guard - 1) (fst ((p !! cur) !! rj))
            else cur
    loopUp xStart yStart k rs =
      if yStart + k >= length expr
        then (False, rs)
        else
          let xk = toRelCol (expr !! (xStart + k)) xStart
              yk = toRelCol (expr !! (yStart + k)) yStart
              c = relColCmp xk yk
           in if c /= 0
                then (True, setAt xStart (if c > 0 then 1 else 0) rs)
                else loopUp xStart yStart (k + 1) rs

up2mnExpand :: Expr -> Int -> Bool -> Bool -> Expr
up2mnExpand expr index shorter plus
  | null expr = expr
  | null (last expr) = init expr
  | otherwise = result
  where
    n = length expr
    right = n - 1
    rc = expr !! right
    top = length rc - 1
    vv = exprVert expr
    p = parents expr vv
    rp = (p !! right) !! top
    (ri, rj) = rp
    isFinite = snd (rc !! top) == 0
    upList =
      if isFinite
        then mnComputeUp1 expr p rp
        else up2mnComputeUp2 expr p rp plus
    result0 = init expr
    newCol0 = init rc
    newCol1 = if not isFinite && top == rj then newCol0 ++ [(ri, 0)] else newCol0
    newCol = newCol1 ++ drop rj (expr !! ri)
    result1 = result0 ++ [newCol]
    yOffset = if isFinite then 0 else (if top - rj > 1 then top - rj else 1)
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [ri + 1 .. right]
    addOne w rs i =
      let off = (right - ri) * w
       in rs ++ [up2mnCopyCol (rs !! i) rp off (yOffset * w) (upList !! i /= 0)]
    result
      | shorter = init result2
      | otherwise = result2
