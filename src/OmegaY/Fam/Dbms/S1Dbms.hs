{-# LANGUAGE OverloadedStrings #-}
-- | S1DBMS（id: s1dbms）。忠实移植 s1dbms.hpp。
module OmegaY.Fam.Dbms.S1Dbms (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data E = E !Int !Int deriving (Show)
type Col = [E]
type Expr = [Col]

eVal :: E -> Int
eVal (E v _) = v

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
    entryP t = do
      (v, t1) <- parseNum t
      let v' = v - 1
      let t2 = skipSp t1
      case t2 of
        ('^' : r) -> do
          (y, t3) <- parseNum r
          Just (E v' (y - 1), t3)
        _ -> Just (E v' (-1), t2)

display :: Expr -> String
display = concatMap colD
  where
    colD [] = "(0)"
    colD cs = "(" ++ joinE cs ++ ")"
    joinE = foldr (\e acc -> entryD e ++ (if null acc then "" else "," ++ acc)) ""
    entryD (E v h) = show (v + 1) ++ "^" ++ show (h + 1)

colHeight :: Col -> Int
colHeight [] = -1
colHeight col = let E _ h = last col in h

filterGt :: Col -> Int -> Col
filterGt col h0 = [e | e@(E _ h) <- col, h > h0]

mergeCol :: Col -> Col -> Col
mergeCol c1 c2 = c1 ++ filterGt c2 (colHeight c1)

copyVal :: Int -> Int -> Int -> Int
copyVal v r off = if v >= r then v + off else v

copyCol :: Col -> Int -> Int -> Col
copyCol col r off = [E (copyVal v r off) (copyVal h r off) | E v h <- col]

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
    newH = colHeight (expr !! h)
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

anyOutOfRange :: Expr -> Bool
anyOutOfRange expr =
  let n = length expr
   in any (\col -> any (\(E v _) -> v < 0 || v >= n) col) expr
        || ( not (null expr)
               && not (null (last expr))
               && let E _ th = last (last expr) in th < 0 || th >= n
           )

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
