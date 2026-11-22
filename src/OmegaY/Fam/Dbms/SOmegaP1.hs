{-# LANGUAGE OverloadedStrings #-}
-- | Sω+1DBMS（id: s-omega-p1-dbms）。忠实移植 s_omega_p1_dbms.hpp。
module OmegaY.Fam.Dbms.SOmegaP1 (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data HP = HP !Bool !Int deriving (Show) -- isW, num
data HE = HE !Int !HP deriving (Show) -- v, p
type Height = [HE]
data E = E !Int !Height deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v

cmpHP :: HP -> HP -> Int
cmpHP (HP w1 n1) (HP w2 n2)
  | w1 && w2 = 0
  | w1 = 1
  | w2 = -1
  | otherwise = cmpI n1 n2

cmpHE :: HE -> HE -> Int
cmpHE (HE v1 p1) (HE v2 p2) =
  case cmpI v1 v2 of
    0 -> cmpHP p1 p2
    c -> c

cmpH :: Height -> Height -> Int
cmpH a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = cmpHE (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> negate (cmpI (length a) (length b))

colHeight :: Col -> Height
colHeight [] = []
colHeight col = let E _ h = last col in h

filterGt :: Col -> Height -> Col
filterGt col h0 = [e | e@(E _ h) <- col, cmpH h h0 > 0]

mergeCol :: Col -> Col -> Col
mergeCol c1 c2 =
  if null c1 then c1 ++ c2 else c1 ++ filterGt c2 (colHeight c1)

copyVal :: Int -> Int -> Int -> Int
copyVal v r off = if v >= r then v + off else v

copyHP :: HP -> Int -> Int -> HP
copyHP (HP w n) r off = if w then HP w n else HP w (copyVal n r off)

copyHE :: HE -> Int -> Int -> HE
copyHE (HE v p) r off = HE (copyVal v r off) (copyHP p r off)

copyH :: Height -> Int -> Int -> Height
copyH h r off = map (\he -> copyHE he r off) h

copyCol :: Col -> Int -> Int -> Col
copyCol col r off = [E (copyVal v r off) (copyH h r off) | E v h <- col]

heightFillDec :: Height -> HP -> Int -> Height
heightFillDec h p v =
  let popped = popV h
   in popped ++ [HE v p]
  where
    popV xs = if not (null xs) && hv (last xs) >= v then popV (init xs) else xs
    hv (HE x _) = x

descend :: Expr -> Height -> Int -> Maybe Height
descend expr h v =
  let hits = [ k | k <- [0 .. length h - 1], hv (h !! k) > v ]
   in case hits of
        [] -> Just h
        (j : _) ->
          if j == 0
            then
              let rh = hv (h !! 0)
                  out = colHeight (expr !! rh)
               in if null out then Nothing else descend expr out v
            else
              let pBound = hp (h !! (j - 1))
               in case findLowerHP expr h pBound v of
                    Nothing -> Nothing
                    Just newP -> Just (heightFillDec h newP v)
  where
    hv (HE x _) = x
    hp (HE _ x) = x

findMaxBelow :: Expr -> Int -> Height -> Int -> Maybe E
findMaxBelow expr i h r
  | r /= i =
      case findMaxBelow expr r h r of
        Nothing -> Nothing
        Just (E _ h1) -> findMaxBelow expr i h1 (hvE h1)
  | otherwise =
      let col = expr !! i
          hits = [ k | k <- [0 .. length col - 1], cmpH (hhE (col !! k)) h > 0 ]
       in case hits of
            [] -> if null col then Nothing else Just (last col)
            (j : _) ->
              let lower = if j == 0 then [] else hhE (col !! (j - 1))
                  haveLower = j /= 0
                  newVal = eVal (col !! j)
                  upper = descend expr h newVal
               in case upper of
                    Just u | not haveLower || cmpH u lower > 0 -> Just (col !! j)
                    _ -> if j == 0 then Nothing else Just (col !! (j - 1))
  where
    hvE h = hv (last h)
    hv (HE x _) = x
    hhE (E _ h) = h

findLowerHP :: Expr -> Height -> HP -> Int -> Maybe HP
findLowerHP expr h (HP w n) r
  | w = Just (HP False r)
  | otherwise =
      case findMaxBelow expr n h r of
        Nothing -> Nothing
        Just bound ->
          let r1 = eVal bound
              h1 = hhE bound
              last = last' h1
              v1 = hv last
              p1 = hp last
           in if hpIsW p1
                then Nothing
                else if v1 == pNum p1 then Just (HP False v1)
                else findLowerHP expr h1 (HP False v1) r1
  where
    last' xs = xs !! (length xs - 1)
    hv (HE x _) = x
    hp (HE _ x) = x
    hpIsW (HP w _) = w
    pNum (HP _ n) = n
    hhE (E _ h) = h

computeNewH :: Height -> Expr -> Int -> Maybe Height
computeNewH h expr r =
  let last = last' h
      v = hv last
      p = hp last
   in case findLowerHP expr h p r of
        Just newP -> Just (heightFillDec h newP r)
        Nothing -> findMaxBelow expr v h r >>= \(E _ nh) -> Just nh
  where
    last' xs = xs !! (length xs - 1)
    hv (HE x _) = x
    hp (HE _ x) = x

-- parse
parseHPos :: String -> Maybe (HP, String)
parseHPos t =
  let s1 = skipSp t
   in case s1 of
        ('ω' : r) -> Just (HP True 0, r)
        ('w' : r) -> Just (HP True 0, r)
        _ -> do
          (n, r) <- parseNum s1
          Just (HP False (n - 1), r)

parseItem :: String -> Maybe (HE, String)
parseItem t = do
  (n, t1) <- parseNum t
  let v = n - 1
  let t2 = skipSp t1
  s3 <- expect '@' t2
  (p, s4) <- parseHPos s3
  Just (HE v p, s4)

parseH :: String -> Maybe (Height, String)
parseH t = do
  s1 <- expect '(' (skipSp t)
  let s2 = skipSp s1
  (extra, s3) <- case s2 of
    ('(' : r) -> Just (True, skipSp r)
    _ -> Just (False, s2)
  (items, s4) <-
    if null s3 || head s3 == ')'
      then Just ([], s3)
      else do
        (e, s3') <- parseItem s3
        itemsLoop [e] (skipSp s3')
  s5 <- expect ')' s4
  s6 <- if extra
          then expect ')' (skipSp s5)
          else Just s5
  Just (items, s6)
  where
    itemsLoop acc s =
      case s of
        (',' : r) -> do
          (e, r') <- parseItem (skipSp r)
          itemsLoop (acc ++ [e]) (skipSp r')
        _ -> Just (acc, s)

entryP :: String -> Maybe (E, String)
entryP t = do
  (v, t1) <- parseNum t
  let v' = v - 1
  let t2 = skipSp t1
  case t2 of
    ('^' : r) -> do
      (h, t3) <- parseH r
      Just (E v' h, t3)
    _ -> Just (E v' [], t2)

parse :: String -> Maybe Expr
parse s =
      let s1 = skipSp s
       in case s1 of
            ('(' : _) -> do
              (cs, s2) <- cols [] s1
              let s3 = skipSp s2
              if null s3 then Just (cs) else Nothing
            _ -> if null s1 then Just ([]) else Nothing
  where
    cols acc t =
      case skipSp t of
        ('(' : _) -> do
          (c, s') <- parseCol entryP t
          let c' = dropTrailing c eVal
          cols (c' : acc) s'
        rest -> Just (reverse acc, rest)

display :: Expr -> String
display = concatMap colD
  where
    colD [] = "(0)"
    colD cs = "(" ++ joinE cs ++ ")"
    joinE = foldr (\e acc -> entryD e ++ (if null acc then "" else "," ++ acc)) ""
    entryD (E v h) =
      show (v + 1) ++ "^(" ++ concat [ itemD k (h !! k) | k <- [0 .. length h - 1] ] ++ ")"
    itemD k (HE v p) =
      (if k > 0 then "," else "") ++ show (v + 1) ++ "@" ++
      (if hpIsW p then "ω" else show (pNum p + 1))
      where hpIsW (HP w _) = w; pNum (HP _ n) = n

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
    newH = computeNewH h expr r
    result0 = init expr
    newCol0 = init rc
    newCol1 = case newH of
      Just nh -> mergeCol newCol0 [E r nh]
      Nothing -> newCol0
    newCol = mergeCol newCol1 (expr !! r)
    result1 = result0 ++ [newCol]
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [r + 1 .. right]
    addOne w rs i = rs ++ [copyCol (rs !! i) r ((right - r) * w)]
    result
      | shorter = init result2
      | otherwise = result2

exprValid :: Expr -> Bool
exprValid expr =
  let n = length expr
      okE (E v h) =
        v >= 0 && v < n && not (null h) &&
        all (\(HE hv (HP w pn)) -> hv >= 0 && hv < n && (w || (pn >= 0 && pn < n))) h
   in all (\col -> all okE col) expr

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = if term < 1 then 1 else term
   in case parse text of
        Nothing -> errParse
        Just expr ->
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
