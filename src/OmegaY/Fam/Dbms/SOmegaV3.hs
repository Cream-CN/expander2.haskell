{-# LANGUAGE OverloadedStrings #-}
-- | SωDBMS v3（id: s-omega-dbms-v3）。忠实移植 s_omega_dbms_v3.hpp。
module OmegaY.Fam.Dbms.SOmegaV3 (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data HE = HE !Int !Int deriving (Show) -- v (0-based), p
type Height = [HE]
data E = E !Int !Height deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v


cmpHE :: HE -> HE -> Int
cmpHE (HE v1 p1) (HE v2 p2) =
  case cmpI v1 v2 of
    0 -> cmpI p1 p2
    c -> c

-- shorter is greater (longer_last=false)
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

copyHE :: HE -> Int -> Int -> HE
copyHE (HE v p) r off = HE (copyVal v r off) p

copyH :: Height -> Int -> Int -> Height
copyH h r off = map (\he -> copyHE he r off) h

copyCol :: Col -> Int -> Int -> Col
copyCol col r off = [E (copyVal v r off) (copyH h r off) | E v h <- col]

heightFillDec :: Height -> Int -> Int -> Height
heightFillDec h p v =
  let popped = popP h
      popped2 = if not (null popped) && hv (last popped) == v then init popped else popped
   in popped2 ++ [HE v p]
  where
    popP xs = if not (null xs) && hp (last xs) <= p then popP (init xs) else xs
    hp (HE _ x) = x
    hv (HE x _) = x

computeNewH :: Height -> Expr -> Int -> Maybe Height
computeNewH h expr r =
  let last = last' h
      v = hv last
      p = hp last
   in if p > 0
        then Just (heightFillDec h (p - 1) r)
        else if length h == 1
          then
            let out = colHeight (expr !! v)
             in if null out then Nothing else Just out
          else
            let pBound = hp (h !! (length h - 2))
                colRh = expr !! v
                hits = [ k | k <- [0 .. length colRh - 1], cmpH h (hhE (colRh !! k)) <= 0 ]
                hj = case hits of (k : _) -> k; [] -> length colRh
             in if hj == length colRh
                  then
                    let out = colHeight colRh
                     in if null out then Nothing else Just out
                  else
                    let newVal = eVal (colRh !! hj)
                        newH = heightFillDec h (pBound - 1) newVal
                        lower = if hj > 0 then hhE (colRh !! (hj - 1)) else []
                        result = if hj > 0 && cmpH lower newH > 0 then lower else newH
                     in Just result
  where
    last' xs = xs !! (length xs - 1)
    hp (HE _ x) = x
    hv (HE x _) = x
    hhE (E _ h) = h

-- parse
parseItem :: String -> Maybe (HE, String)
parseItem t = do
  (n, t1) <- parseNum t
  let v = n - 1
  let t2 = skipSp t1
  s3 <- expect '@' t2
  (p, s4) <- parseNum s3
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
    ('(' : _) -> do
      (h, t3) <- parseH t2
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
      show (v + 1) ++ "^(" ++ concat [ (if k > 0 then "," else "") ++ show (hv (h !! k) + 1) ++ "@" ++ show (hp (h !! k)) | k <- [0 .. length h - 1] ] ++ ")"
      where hv (HE x _) = x; hp (HE _ x) = x


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
        v >= 0 && v < n && not (null h) && all (\(HE hv _) -> hv >= 0 && hv < n) h
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
