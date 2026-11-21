{-# LANGUAGE OverloadedStrings #-}
-- | SωDBMS v1（id: s-omega-dbms）。忠实移植 s_omega_dbms_v1.hpp。
module OmegaY.Fam.Dbms.SOmegaV1 (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

type Height = [Int]
data E = E !Int !Height deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v


cmpH :: Height -> Height -> Int
cmpH a b =
  case cmpI (length a) (length b) of
    0 -> go (length a - 1)
    c -> c
  where
    go (-1) = 0
    go k = case cmpI (a !! k) (b !! k) of
      0 -> go (k - 1)
      c -> c

topSep :: Height -> Int
topSep h = case h of
  [] -> 0
  (rh : _) -> length (takeWhile (== rh) h)

findBelowH :: Col -> Height -> Int
findBelowH col h = bin 0 (length col)
  where
    bin lo hi =
      if lo >= hi
        then lo
        else
          let m = (lo + hi + 1) `div` 2
           in if cmpH h (let E _ ch = col !! (m - 1) in ch) > 0
                then bin m hi
                else bin lo (m - 1)

lexEq :: Height -> Height -> Int -> Bool
lexEq a b from = all (\k -> a !! k == b !! k) [from .. length a - 1]

colHeight :: Col -> Height
colHeight [] = []
colHeight col = let E _ h = last col in h

filterGt :: Col -> Height -> Col
filterGt col h0 = [e | e@(E _ h) <- col, cmpH h h0 > 0]

mergeCol :: Col -> Col -> Col
mergeCol c1 c2 =
  let h = colHeight c1
   in c1 ++ filterGt c2 h

copyVal :: Int -> Int -> Int -> Int
copyVal v r off = if v >= r then v + off else v

copyH :: Height -> Int -> Int -> Height
copyH h r off = map (\s -> copyVal s r off) h

copyCol :: Col -> Int -> Int -> Col
copyCol col r off = [E (copyVal v r off) (copyH h r off) | E v h <- col]

computeNewH :: Height -> Col -> Int -> Height
computeNewH h colRh r =
  let s = topSep h
      newH0 = h
      hj = findBelowH colRh h
      setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs
      setPrefix nh = [if k < s - 1 then r else nh !! k | k <- [0 .. length nh - 1]]
      newH1 =
        if hj == length colRh
          then
            let hCand = colHeight colRh
             in if null colRh
                  then if s > 0 then init newH0 else newH0
                  else if length hCand < s
                    then init newH0
                    else setAt (s - 1) (hCand !! (s - 1)) newH0
          else
            let haveBase = hj /= 0
                baseRef = if haveBase then let E _ bh = colRh !! (hj - 1) in bh else []
                eq = haveBase && length baseRef == length h && lexEq baseRef h s
                nv = if eq then baseRef !! (s - 1) else let E bv _ = colRh !! hj in bv
             in setAt (s - 1) nv newH0
   in setPrefix newH1

-- parse
parseH :: String -> Maybe (Height, String)
parseH t = do
  s1 <- expect '(' (skipSp t)
  let s2 = skipSp s1
  (displayed, s3) <-
    if null s2 || head s2 == ')'
      then Just ([], s2)
      else do
        (n, s2') <- parseNum s2
        rest s2' [n]
  s4 <- expect ')' (skipSp s3)
  let internal = reverse (map (\x -> x - 1) displayed)
  Just (internal, s4)
  where
    rest t acc =
      case skipSp t of
        (',' : r) -> do
          (n, r') <- parseNum r
          rest (skipSp r') (acc ++ [n])
        _ -> Just (acc, t)

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
      show (v + 1) ++ "^(" ++
      concat [ (if i > 0 then "," else "") ++ show (h !! (length h - 1 - i) + 1) | i <- [0 .. length h - 1] ]
      ++ ")"


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
    rh = head h
    newH = computeNewH h (expr !! rh) r
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
   in all (\col -> all (\(E v h) -> v >= 0 && v < n && not (null h) && head h >= 0 && head h < n) col) expr

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
