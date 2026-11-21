{-# LANGUAGE OverloadedStrings #-}
-- | SωDBMS v2（id: s-omega-dbms-v2）。忠实移植 s_omega_dbms_v2.hpp。
module OmegaY.Fam.Dbms.SOmegaV2 (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data HEntry = HEntry !Bool !Int !Bool !Int deriving (Show) -- pDef, p, vDef, v
type Height = [HEntry]
data E = E !Int !Height deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v


cmpUndef :: Bool -> Int -> Bool -> Int -> Int
cmpUndef aDef a bDef b
  | aDef && bDef = cmpI a b
  | aDef = -1
  | bDef = 1
  | otherwise = 0

cmpHE :: HEntry -> HEntry -> Int
cmpHE (HEntry pd p vd v) (HEntry pd2 p2 vd2 v2) =
  case cmpUndef pd p pd2 p2 of
    0 -> cmpUndef vd v vd2 v2
    c -> c

cmpH :: Height -> Height -> Int
cmpH a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = cmpHE (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

colHeight :: Col -> Height
colHeight [] = []
colHeight col = let E _ h = last col in h

filterGt :: Col -> Height -> Col
filterGt col h0 = [e | e@(E _ h) <- col, cmpH h h0 > 0]

mergeCol :: Col -> Col -> Col
mergeCol c1 c2 = c1 ++ filterGt c2 (colHeight c1)

copyVal :: Int -> Int -> Int -> Int
copyVal v r off = if v >= r then v + off else v

copyHE :: HEntry -> Int -> Int -> HEntry
copyHE (HEntry pd p vd v) r off = HEntry pd p vd (if vd then copyVal v r off else 0)

copyH :: Height -> Int -> Int -> Height
copyH h r off = map (\he -> copyHE he r off) h

copyCol :: Col -> Int -> Int -> Col
copyCol col r off = [E (copyVal v r off) (copyH h r off) | E v h <- col]

heightFill :: Height -> Int -> Bool -> Int -> Height
heightFill h p vDef v =
  let popped = popWhile h
      last = last' popped
   in if not (lastVDef last == vDef && (not vDef || lastV last == v))
        then popped ++ [HEntry True p vDef v]
        else popped
  where
    lastV (HEntry _ _ _ x) = x
    lastVDef (HEntry _ _ x _) = x
    last' xs = xs !! (length xs - 1)
    popWhile xs =
      if length xs > 1 && pDef (last' xs) && pVal (last' xs) <= p
        then popWhile (init xs)
        else xs
    pDef (HEntry x _ _ _) = x
    pVal (HEntry _ x _ _) = x

computeNewH :: Height -> Expr -> Int -> Height
computeNewH h expr r =
  let last = last' h
   in if not (vDef last)
        then
          let h1 = heightFill h (p last) True r
           in if p last > 0 then heightFill h1 (p last - 1) False 0 else h1
        else if not (pDef last)
          then colHeight (expr !! v last)
          else
            let colRh = expr !! v last
                hits = [ k | k <- [0 .. length colRh - 1], cmpH h (heightE (colRh !! k)) <= 0 ]
                hj = case hits of (k : _) -> k; [] -> length colRh
             in if hj == length colRh
                  then colHeight colRh
                  else
                    let newVal = eVal (colRh !! hj)
                        h1 = heightFill h (p last) True newVal
                        h2 = if p last > 0 then heightFill h1 (p last - 1) False 0 else h1
                        lower = if hj > 0 then heightE (colRh !! (hj - 1)) else []
                     in if hj > 0 && cmpH lower h2 > 0 then lower else h2
  where
    last' xs = xs !! (length xs - 1)
    pDef (HEntry x _ _ _) = x
    p (HEntry _ x _ _) = x
    vDef (HEntry _ _ x _) = x
    v (HEntry _ _ _ x) = x
    heightE (E _ h) = h

-- parse
parseHVal :: String -> Maybe (Bool, Int, String)
parseHVal t =
  let s1 = skipSp t
   in case s1 of
        ('ω' : r) -> Just (True, 0, r)
        ('w' : r) -> Just (True, 0, r)
        _ -> do
          (n, r) <- parseNum s1
          Just (False, n - 1, r)

parseItem :: Bool -> String -> Maybe (HEntry, String)
parseItem first t = do
  (undef, v, t1) <- parseHVal t
  let t2 = skipSp t1
  case t2 of
    ('@' : r) -> do
      (p, r') <- parseNum r
      Just (HEntry True p (not undef) v, r')
    _ ->
      if not first
        then Nothing
        else Just (HEntry False 0 (not undef) v, t2)

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
        (e, s3') <- parseItem True s3
        itemsLoop [e] (skipSp s3')
  s5 <- expect ')' s4
  s6 <- if extra
          then do
            s7 <- expect ')' (skipSp s5)
            Just s7
          else Just s5
  Just (items, s6)
  where
    itemsLoop acc s =
      case s of
        (',' : r) -> do
          (e, r') <- parseItem False (skipSp r)
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
    entryD (E v h) = show (v + 1) ++ "^(" ++ concat (zipWith itemD [0 ..] h) ++ ")"
    itemD k he =
      (if k > 0 then "," else "") ++
      (if vDef he then show (vVal he + 1) else "ω") ++
      (if pDef he then "@" ++ show (pVal he) else "")
      where
        pDef (HEntry x _ _ _) = x
        pVal (HEntry _ x _ _) = x
        vDef (HEntry _ _ x _) = x
        vVal (HEntry _ _ _ x) = x


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
    newCol1 = mergeCol newCol0 [E r newH]
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
        all (\(HEntry pd _ vd v) ->
               (not vd || (v >= 0 && v < n)) && (vd || pd)) h
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
