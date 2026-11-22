{-# LANGUAGE OverloadedStrings #-}
-- | S2DBMS（id: s2dbms）。忠实移植 s2dbms.hpp。
module OmegaY.Fam.Dbms.S2Dbms (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data H = H !Int !Int deriving (Show) -- h0, h1
data E = E !Int !H deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v

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
    entryD (E v (H h0 h1)) = show (v + 1) ++ "^(" ++ show h1 ++ "," ++ show (h0 + 1) ++ ")"

cmpH :: H -> H -> Int
cmpH (H a0 a1) (H b0 b1) =
  case cmpI a1 b1 of
    0 -> cmpI a0 b0
    c -> c

colHeight :: Col -> Maybe H
colHeight [] = Nothing
colHeight col = Just (let E _ h = last col in h)

filterGt :: Col -> H -> Col
filterGt col h0 = [e | e@(E _ h) <- col, cmpH h h0 > 0]

mergeCol :: Col -> Col -> Col
mergeCol c1 c2 = case colHeight c1 of
  Just h -> c1 ++ filterGt c2 h
  Nothing -> c1 ++ c2

copyVal :: Int -> Int -> Int -> Int
copyVal v r off = if v >= r then v + off else v

copyH :: H -> Int -> Int -> H
copyH (H h0 h1) r off = H (copyVal h0 r off) h1

copyCol :: Col -> Int -> Int -> Col
copyCol col r off = [E (copyVal v r off) (copyH h r off) | E v h <- col]

height1 :: Col -> Int
height1 col = case colHeight col of
  Just (H _ h1) -> h1
  Nothing -> 0

computeNewHeight :: H -> Expr -> Int -> Maybe H
computeNewHeight h@(H v h1) expr r =
  let colRh = expr !! v
      hits = [ k | k <- [0 .. length colRh - 1], cmpH h (let E _ hh = colRh !! k in hh) <= 0 ]
      hj = case hits of (k : _) -> k; [] -> length colRh
   in if hj == length colRh
        then
          if height1 colRh < h1
            then Just (H r (h1 - 1))
            else colHeight colRh
        else
          let newVal = let E x _ = colRh !! hj in x
              base = H newVal h1
              newH = if hj > 0 then let E _ lower = colRh !! (hj - 1) in if cmpH lower base > 0 then lower else base else base
           in Just newH

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
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [r + 1 .. right]
    addOne w rs i = rs ++ [copyCol (rs !! i) r ((right - r) * w)]
    result
      | shorter = init result2
      | otherwise = result2

anyOutOfRange :: Expr -> Bool
anyOutOfRange expr =
  let n = length expr
   in any (\col -> any (\(E _ (H h0 _)) -> h0 < 0 || h0 >= n) col
              || any (\(E v _) -> v < 0 || v >= n) col) expr

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = if term < 1 then 1 else term
   in case parse text of
        Nothing -> errParse
        Just expr ->
              if null expr
                then ""
                else
                  if anyOutOfRange expr
                    then errParse
                    else
                      if null (last expr)
                        then display (init expr)
                        else display (expand expr term' (not longer))

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandAlter :: String -> Int -> String
expandAlter text term = expandTextImpl text term True
