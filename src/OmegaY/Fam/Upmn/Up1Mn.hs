{-# LANGUAGE OverloadedStrings #-}
-- | up1mn（UP1MN）。移植 up1mn.hpp。
-- Expr = [[Int]]（0-based value，无 sep）。
module OmegaY.Fam.Upmn.Up1Mn (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common
import OmegaY.Fam.Dbms.MnUp2 (parseExpr, displayExpr, Expr, Col, setAt)

errP = errParse

type IExpr = [[Int]]

parseIntExpr :: String -> Maybe IExpr
parseIntExpr text = do
  e <- parseExpr text
  if all (\col -> all (\(_, sep) -> sep == 0) col) e
    then Just [ [v | (v, _) <- col] | col <- e ]
    else Nothing

displayIntExpr :: IExpr -> String
displayIntExpr e = displayExpr [ [(v, 0) | v <- col] | col <- e ]

computeUp :: IExpr -> Int -> Int -> [Int]
computeUp expr r b = result
  where
    n = length expr
    right = n - 1
    initial = [ if k < r then 0 else if k == r then 1 else (-1) | k <- [0 .. n - 1] ]
    result = foldl applyI initial [r + 1 .. n - 1]
    applyI rs i =
      let col = expr !! i
       in if length col <= b
            then setAt i 0 rs
            else
              let cb = col !! b
               in if cb /= r
                    then setAt i (rs !! cb) rs
                    else
                      case [k | k <- [0 .. length col - 1], col !! k == r] of
                        [] -> setAt i 0 rs
                        (j : _) ->
                          if j > 0
                            then
                              let p = walkJ i j
                               in if rs !! p == 0
                                    then setAt i 0 rs
                                    else continueUp rs i j
                            else continueUp rs i j
    continueUp rs i j =
      let yStart = walkY right j
       in if yStart <= i
            then setAt i (if i == yStart then 1 else 0) rs
            else
              let x0 = drop j (expr !! i)
                  y0 = drop j (expr !! yStart)
                  c0 = cmpIntVec x0 y0
               in if c0 /= 0
                    then setAt i (if c0 > 0 then 1 else 0) rs
                    else
                      let (dec, rs2) = loopUp i yStart 1 rs
                       in if dec then rs2 else setAt i 1 rs2
    walkJ start j = goGuard (n + 1) start
      where
        goGuard 0 cur = cur
        goGuard guard cur =
          let pi = (expr !! cur) !! (j - 1)
           in if pi == r then cur else goGuard (guard - 1) pi
    walkY start j = goGuard (n + 1) start
      where
        goGuard 0 cur = cur
        goGuard guard cur =
          let pi = (expr !! cur) !! j
           in if pi == r then cur else goGuard (guard - 1) pi
    loopUp xStart yStart k rs =
      if yStart + k >= n
        then (False, rs)
        else
          let xk = map (\v -> if v >= xStart then v - xStart else v) (expr !! (xStart + k))
              yk = map (\v -> if v >= yStart then v - yStart else v) (expr !! (yStart + k))
              c = cmpIntVec xk yk
           in if c /= 0
                then (True, setAt xStart (if c > 0 then 1 else 0) rs)
                else loopUp xStart yStart (k + 1) rs
    cmpIntVec a b =
      let n0 = min (length a) (length b)
          go k
            | k >= n0 = compare (length a) (length b)
            | otherwise =
                let c = compare (a !! k) (b !! k)
                 in if c /= EQ then c else go (k + 1)
       in case go 0 of
            LT -> -1
            EQ -> 0
            GT -> 1

copyColumn :: [Int] -> Int -> Int -> Bool -> [Int]
copyColumn col r offset up =
  [ if v > r then v + offset else if v < r then v else if up then v + offset else v
  | v <- col ]

expand :: IExpr -> Int -> Bool -> IExpr
expand expr index shorter
  | null expr = expr
  | null (last expr) = init expr
  | otherwise = result
  where
    n = length expr
    right = n - 1
    rc = expr !! right
    r = last rc
    upList = computeUp expr r (length rc - 1)
    result0 = init expr
    newCol = init rc ++ drop (length rc - 1) (expr !! r)
    result1 = result0 ++ [newCol]
    result2 = foldl addCopy result1 [1 .. index]
    addCopy rs w = foldl (addOne w) rs [r + 1 .. right]
    addOne w rs i = rs ++ [copyColumn (rs !! i) r ((right - r) * w) (upList !! i /= 0)]
    result
      | shorter = init result2
      | otherwise = result2

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = if term < 1 then 1 else term
   in case parseIntExpr text of
            Nothing -> errP
            Just e ->
              if null e then ""
              else if any (\col -> any (\v -> v < 0 || v >= length e) col) e
                then errP
                else if null (last e)
                  then displayIntExpr (init e)
                  else displayIntExpr (expand e term' (not longer))

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandAlter :: String -> Int -> String
expandAlter text term = expandTextImpl text term True
