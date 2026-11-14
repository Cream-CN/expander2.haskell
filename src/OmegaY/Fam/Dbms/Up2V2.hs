{-# LANGUAGE OverloadedStrings #-}
-- | UP2DBMS v2（id: up2dbms-v2）。忠实移植 up2dbms_v2.hpp（TS 风格 Height h0/h1）。
module OmegaY.Fam.Dbms.Up2V2 (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data H = H !Int !Int deriving (Show) -- h0, h1
data E = E !Int !H deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v

cmpH :: H -> H -> Int
cmpH (H a0 a1) (H b0 b1) =
  case cmpI a1 b1 of
    0 -> cmpI a0 b0
    c -> c

cmpE :: E -> E -> Int
cmpE (E v1 h1) (E v2 h2) =
  case cmpI v1 v2 of
    0 -> cmpH h1 h2
    c -> c

colCmp :: Col -> Col -> Int
colCmp a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = cmpE (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

-- display
display :: Expr -> String
display = concatMap colD
  where
    colD [] = "(0)"
    colD cs = "(" ++ joinE cs ++ ")"
    joinE = foldr (\e acc -> entryD e ++ (if null acc then "" else "," ++ acc)) ""
    entryD (E v (H h0 h1)) = show (v + 1) ++ "^(" ++ show h1 ++ "," ++ show (h0 + 1) ++ ")"

-- parse
parseH :: String -> Maybe (H, String)
parseH t = do
  s1 <- expect '(' (skipSp t)
  (h1, s2) <- parseNum s1
  s3 <- expect ',' s2
  (h0p, s4) <- parseNum s3
  let h0 = h0p - 1
  s5 <- expect ')' (skipSp s4)
  Just (H h0 h1, s5)

entryP :: String -> Maybe (E, String)
entryP t = do
  (v, t1) <- parseNum t
  let v' = v - 1
  let t2 = skipSp t1
  case t2 of
    ('^' : r) -> do
      (h, t3) <- parseH r
      Just (E v' h, t3)
    ('(' : _) -> do
      (h, t3) <- parseH t2
      Just (E v' h, t3)
    _ -> Just (E v' (H 0 (-1)), t2)

data Parsed = Parsed Bool Expr

parse :: String -> Maybe Parsed
parse text =
  let s1 = skipSp text
   in case s1 of
        ('L' : 'i' : 'm' : 'i' : 't' : rest) ->
          if null (skipSp rest) then Just (Parsed True []) else Nothing
        _ ->
          if s1 == "limit" || s1 == "LIMIT"
            then Just (Parsed True [])
            else do
              (cs, s2) <- cols [] s1
              if null (skipSp s2) then Just (Parsed False cs) else Nothing
  where
    cols acc t =
      case skipSp t of
        ('(' : _) -> do
          (c, s') <- parseCol entryP t
          let c' = dropTrailing c eVal
          cols (c' : acc) s'
        rest -> Just (reverse acc, rest)

colHeight :: Col -> Maybe H
colHeight [] = Nothing
colHeight col = Just (let E _ h = last col in h)

height1 :: Col -> Int
height1 col = case colHeight col of
  Just (H _ h1) -> h1
  Nothing -> 0

filterGt :: Col -> H -> Col
filterGt col h0 = [e | e@(E _ h) <- col, cmpH h h0 > 0]

mergeCol :: Col -> Col -> Col
mergeCol c1 c2 = case colHeight c1 of
  Just h -> c1 ++ filterGt c2 h
  Nothing -> c2

-- copy_value / copy_height
copyVal :: Int -> Int -> Int -> Bool -> Int
copyVal v r off up = if v > r || (v == r && up) then v + off else v

copyH :: H -> Int -> Int -> Bool -> H
copyH (H h0 h1) r off up = H (copyVal h0 r off up) h1

copyE :: E -> Int -> Int -> Bool -> E
copyE (E v h) r off up = E (copyVal v r off up) (copyH h r off up)

hh1 :: H -> Int
hh1 (H _ x) = x

-- TS copy_column with BMS upgrade branch
copyCol :: Col -> Int -> Int -> Bool -> Col
copyCol col r off up = go 0 col
  where
    go _ [] = []
    go j (e@(E v h) : rest) =
      let prevH = if j == 0 then Nothing else Just (let E _ ph = col !! (j - 1) in ph)
       in if v == r && not up
            then
              if hh1 h == 0 || (case prevH of Just ph -> hh1 h == hh1 ph; Nothing -> False)
                then E (r + off) (copyH h r off False) : go (j + 1) rest
                else
                  let finite = H r (maybe 0 hh1 prevH)
                   in E (r + off) finite : copyE e r off False : go (j + 1) rest
            else copyE e r off up : go (j + 1) rest

-- compute_new_height
computeNewHeight :: H -> Expr -> Int -> Maybe H
computeNewHeight h@(H v h1) expr r =
  let colRh = expr !! v
      hits = [ k | k <- [0 .. length colRh - 1], cmpH h (let E _ hh = colRh !! k in hh) <= 0 ]
      hj = case hits of (k : _) -> Just k; [] -> Nothing
   in case hj of
        Nothing ->
          if height1 colRh < h1
            then Just (H r (h1 - 1))
            else colHeight colRh
        Just k ->
          let newVal = let E x _ = colRh !! k in x
              base = H newVal h1
              newH = if k > 0 then let E _ lower = colRh !! (k - 1) in if cmpH lower base > 0 then lower else base else base
           in Just newH

heightAfter :: Col -> Int -> Maybe H
heightAfter col r = go (reverse col)
  where
    go [] = Nothing
    go (E v h : rest) = if v >= r then Just h else go rest

parentAt :: Col -> Maybe H -> Int
parentAt col mh = case mh of
  Nothing -> if null col then -1 else let E v _ = head col in v
  Just h -> go col
    where
      go [] = -1
      go (E v hh : rest) = if cmpH hh h >= 0 then v else go rest

discardAfter :: Col -> Int -> Col
discardAfter col r = [e | e@(E v _) <- col, v <= r]

-- RelEntry
data RelE = RelE !Bool !Int !Bool !Int !Int deriving (Show)

toRelCol :: Col -> Int -> [RelE]
toRelCol col r =
  [ let f0 = v >= r
        f1 = h0 >= r
     in RelE f0 (if f0 then v - r else v) f1 (if f1 then h0 - r else h0) h1
  | E v (H h0 h1) <- col
  ]

cmpRelVal :: Bool -> Int -> Bool -> Int -> Int
cmpRelVal fa va fb vb =
  if fa /= fb then (if fa then 1 else -1) else cmpI va vb

cmpRelE :: RelE -> RelE -> Int
cmpRelE (RelE f0a v0a f1a v1a h1a) (RelE f0b v0b f1b v1b h1b) =
  case cmpRelVal f0a v0a f0b v0b of
    0 -> case cmpRelVal f1a v1a f1b v1b of
           0 -> cmpI h1a h1b
           c -> c
    c -> c

relColCmp :: [RelE] -> [RelE] -> Int
relColCmp a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = cmpRelE (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- compute_up
computeUp :: Expr -> Int -> H -> [Bool]
computeUp expr r hh@(H _ h1) = build
  where
    h = hh
    n = length expr
    right = n - 1
    initial = [k < r | k <- [0 .. n - 1]]
    build
      | h1 == 0 = [k <= r | k <- [0 .. n - 1]]
      | otherwise = foldl applyIdx (setAt r True initial) [r + 1 .. right]
    applyIdx rs i =
      let col = expr !! i
          hRoot = heightAfter col r
       in case hRoot of
            Nothing -> setAt i False rs
            Just hr ->
              if hh1 hr < hh1 h
                then setAt i False rs
                else
                  let hProj = heightAfter col (r + 1)
                      threshold = H (r + 1) (maybe 0 (\hp -> min (hh1 hp) (hh1 h - 1)) hProj)
                   in case hProj of
                        Just hp | cmpH hp threshold >= 0 ->
                          let pa = parentAt col (Just threshold)
                           in setAt i (if pa >= 0 then rs !! pa else False) rs
                        _ ->
                          let pProj = parentAt col hProj
                           in if pProj < 0 || not (rs !! pProj)
                                then setAt i False rs
                                else
                                  let yStart = walkY right threshold
                                   in if not yStart_ok
                                        then setAt i False rs
                                        else if yStart <= i
                                          then setAt i (i == yStart) rs
                                          else
                                            let x0 = discardAfter (expr !! i) r
                                                y0 = discardAfter (expr !! yStart) r
                                                c0 = colCmp x0 y0
                                             in if c0 /= 0
                                                  then setAt i (c0 > 0) rs
                                                  else
                                                    let (dec, rs2) = loopUp i yStart 1 rs
                                                     in if dec then rs2 else setAt i True rs
      where
        walkY start threshold =
          let n = length expr
           in goGuard (n + 1) start
          where
            goGuard 0 cur = cur
            goGuard guard cur =
              let next = parentAt (expr !! cur) (Just threshold)
               in if next == r then cur else goGuard (guard - 1) next
        -- walkY_ok: we treat as true (valid input)
        yStart_ok = True
        loopUp xStart yStart k rs =
          if yStart + k >= length expr
            then (False, rs)
            else
              let xk = toRelCol (expr !! (xStart + k)) xStart
                  yk = toRelCol (expr !! (yStart + k)) yStart
                  c = relColCmp xk yk
               in if c /= 0
                    then (True, setAt xStart (c > 0) rs)
                    else loopUp xStart yStart (k + 1) rs

infinityFs :: Int -> Expr
infinityFs index =
  [] : [ [E i (H i (i + 1))] | i <- [0 .. index - 1] ]

expand :: Expr -> Int -> Bool -> Expr
expand expr index shorter
  | null expr = expr
  | null (last expr) = init expr
  | otherwise = result
  where
    n = length expr
    right = n - 1
    rc = expr !! right
    E r h = last rc
    newH = computeNewHeight h expr r
    result0 = init expr
    newCol0 = init rc
    newCol1 = case newH of
      Just nh -> mergeCol newCol0 [E r nh]
      Nothing -> newCol0
    newCol = mergeCol newCol1 (expr !! r)
    result1 = result0 ++ [newCol]
    up = computeUp expr r h
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [r + 1 .. right]
    addOne w rs i =
      let off = (right - r) * w
       in rs ++ [copyCol (rs !! i) r off (up !! i)]
    result
      | shorter = init result2
      | otherwise = result2

exprValid :: Expr -> Bool
exprValid expr =
  let n = length expr
   in all (\col -> all (\(E v (H h0 _)) -> v >= 0 && v < n && h0 >= 0 && h0 < n) col) expr

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = if term < 1 then 1 else term
   in case parse text of
        Nothing -> errParse
        Just (Parsed inf expr) ->
          if inf
            then display (infinityFs term')
            else
              if null expr
                then ""
                else
                  if not (exprValid expr)
                    then errParse
                    else
                      if null (last expr)
                        then display (init expr)
                        else display (expand expr term' (not longer))

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandAlter :: String -> Int -> String
expandAlter text term = expandTextImpl text term True
