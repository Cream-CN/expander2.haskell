{-# LANGUAGE OverloadedStrings #-}
-- | UP1DBMS（id: up1dbms）。忠实移植 up1dbms.hpp。
module OmegaY.Fam.Dbms.Up1Dbms (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common

data E = E !Int !Int deriving (Show) -- v (0-based), h (0-based; -1 省略)

type Col = [E]
type Expr = [Col]


-- parse :: String -> Maybe (Bool, Expr)  -- Just (inf, expr)
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

eVal :: E -> Int
eVal (E v _) = v

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

copyVal :: Int -> Int -> Int -> Bool -> Int
copyVal v r off up = if v > r || (v == r && up) then v + off else v

copyEntry :: E -> Int -> Int -> Bool -> E
copyEntry (E v h) r off up = E (copyVal v r off up) (copyVal h r off up)

copyCol :: Col -> Int -> Int -> Bool -> Col
copyCol col r off up = map (\e -> copyEntry e r off up) col

heightAfter :: Col -> Int -> Int
heightAfter col r = go (reverse col)
  where
    go [] = -1
    go (E v h : rest) = if v >= r then h else go rest

parentAt :: Col -> Int -> Int
parentAt col h = go col
  where
    go [] = -1
    go (E v hh : rest) = if hh >= h then v else go rest

discardAfter :: Col -> Int -> Col
discardAfter col r = [e | e@(E v _) <- col, v <= r]

cmpE :: E -> E -> Int
cmpE (E v1 h1) (E v2 h2) = case cmpI v1 v2 of
  0 -> cmpI h1 h2
  c -> c

cmpCol :: Col -> Col -> Int
cmpCol a b =
  let n = min (length a) (length b)
   in case [c | k <- [0 .. n - 1], let c = cmpE (a !! k) (b !! k), c /= 0] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

computeUp :: Expr -> Int -> Int -> [Int]
computeUp expr r h = build
  where
    n = length expr
    right = n - 1
    build = foldl applyIdx initialResult [r + 1 .. right]
    initialResult = [if k < r then 0 else if k == r then 1 else (-1) | k <- [0 .. n - 1]]
    applyIdx rs i =
      let col = expr !! i
          hRoot = heightAfter col r
       in if hRoot < h
            then setAt i 0 rs
            else
              let hProj = heightAfter col (r + 1)
               in if hProj >= h
                    then
                      let pa = parentAt col h
                       in setAt i (if pa < 0 then 0 else rs !! pa) rs
                    else
                      let rs1 = if hProj >= 0
                                 then
                                   let pProj = walkUp i (parentAt (expr !! i) hProj) hProj r
                                   in if pProj < 0
                                        then setAt i 0 rs
                                        else if rs !! pProj == 0
                                          then setAt i 0 rs
                                          else rs
                                 else rs
                          yStart = walkUp right (parentAt (expr !! right) (hProj + 1)) (hProj + 1) r
                       in if yStart < 0
                            then setAt i 0 rs1
                            else if yStart <= i
                              then setAt i (if i == yStart then 1 else 0) rs1
                              else
                                let x0 = discardAfter (expr !! i) r
                                    y0 = discardAfter (expr !! yStart) r
                                    c0 = cmpCol x0 y0
                                 in if c0 /= 0
                                      then setAt i (if c0 > 0 then 1 else 0) rs1
                                      else
                                        let (dec, rs2) = decidedLoop i yStart 1 rs1
                                         in if dec then rs2 else setAt i 1 rs2
    walkUp start nextAt hh r0 =
      let n = length expr
       in goGuard (n + 1) start nextAt
      where
        goGuard 0 cur _ = cur
        goGuard guard cur next
          | next == r0 = cur
          | next < 0 = next
          | otherwise = goGuard (guard - 1) next (parentAt (expr !! next) hh)
    decidedLoop xStart yStart k rs =
      if yStart + k >= length expr
        then (False, rs)
        else
          let xk = relCol (expr !! (xStart + k)) xStart
              yk = relCol (expr !! (yStart + k)) yStart
              c = relCmp xk yk
           in if c /= 0
                then (True, setAt (xStart) (if c > 0 then 1 else 0) rs)
                else decidedLoop xStart yStart (k + 1) rs

-- 相对列条目：(flag_v, nv, flag_h, nh)
relCol :: Col -> Int -> [(Bool, Int, Bool, Int)]
relCol col base =
  [ (v >= base, (if v >= base then v - base else v), h >= base, (if h >= base then h - base else h))
  | E v h <- col
  ]

relCmp :: [(Bool, Int, Bool, Int)] -> [(Bool, Int, Bool, Int)] -> Int
relCmp a b =
  let n = min (length a) (length b)
   in case [ c
           | k <- [0 .. n - 1]
           , let (fa, nva, fha, nha) = a !! k
                 (fb, nvb, fhb, nhb) = b !! k
           , c <- [cmpQuad fa nva fha nha fb nvb fhb nhb]
           , c /= 0
           ] of
        (c : _) -> c
        [] -> cmpI (length a) (length b)

cmpQuad :: Bool -> Int -> Bool -> Int -> Bool -> Int -> Bool -> Int -> Int
cmpQuad fa nva fha nha fb nvb fhb nhb =
  let c0 = cmpBool fa fb
   in if c0 /= 0
        then c0
        else
          let c1 = cmpI nva nvb
           in if c1 /= 0
                then c1
                else
                  let c2 = cmpBool fha fhb
                   in if c2 /= 0 then c2 else cmpI nha nhb

cmpBool :: Bool -> Bool -> Int
cmpBool a b = if a == b then 0 else if a then 1 else -1

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs


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
    up = computeUp expr r h
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [r + 1 .. right]
    addOne w rs i =
      let off = (right - r) * w
          col = rs !! i
       in rs ++ [copyCol col r off (up !! i /= 0)]
    result
      | shorter = init result2
      | otherwise = result2

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

anyOutOfRange :: Expr -> Bool
anyOutOfRange expr =
  let n = length expr
   in any (\col -> any (\(E v _) -> v < 0 || v >= n) col) expr
        || ( not (null expr)
               && not (null (last expr))
               && let E _ th = last (last expr) in th < 0 || th >= n
           )

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandAlter :: String -> Int -> String
expandAlter text term = expandTextImpl text term True
