{-# LANGUAGE OverloadedStrings #-}
-- | up2mn-v1a（UP2MN v1A）。移植 up2mn_v1a.hpp。
-- 使用 0-based Expr（与 MnUp2 一致），infinity_value 对应 1-based [[],[(1,1)],[(2,1)]]
-- = 0-based [[],[(0,1)],[(1,1)]]。
module OmegaY.Fam.Upmn.Up2MnV1a (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common
import OmegaY.Fam.Dbms.MnUp2
import OmegaY.Fam.Upmn.SmnShare (smnExpand)

errP = errParse

infinityValue :: Expr
infinityValue = [[], [(0, 1)], [(1, 1)]]

hasInfinite :: Col -> Bool
hasInfinite col = not (null col) && snd (last col) > 0

finiteHeight :: Col -> Int
finiteHeight col = if hasInfinite col then length col - 1 else length col

colCompare :: Col -> Col -> Int
colCompare a b = go 0
  where
    n0 = min (length a) (length b)
    ord2Int LT = -1
    ord2Int EQ = 0
    ord2Int GT = 1
    go k
      | k >= n0 = ord2Int (compare (length a) (length b))
      | otherwise =
          let (va, sa) = a !! k
              (vb, sb) = b !! k
           in if va /= vb
                then ord2Int (compare va vb)
                else if sa /= sb
                  then ord2Int (compare sa sb)
                  else go (k + 1)

mountainCompare :: Expr -> Expr -> Int
mountainCompare a b = go 0
  where
    n0 = min (length a) (length b)
    ord2Int LT = -1
    ord2Int EQ = 0
    ord2Int GT = 1
    go k
      | k >= n0 = ord2Int (compare (length a) (length b))
      | otherwise =
          let c = colCompare (a !! k) (b !! k)
           in if c /= 0 then c else go (k + 1)

isInfinity :: Expr -> Bool
isInfinity m = mountainCompare m infinityValue == 0

-- subtract_1 for v1a infinite branch (0-based: value -= 1)
subtract1 :: Expr -> Expr
subtract1 = map (map (\(v, sep) -> (v - 1, sep)))

v1aInfinityFs :: Int -> Expr
v1aInfinityFs index = smnExpand infinityValue index False

copyColumnV1a :: Col -> Int -> Int -> Int -> Bool -> Int -> Col
copyColumnV1a col ri rj offset up yOffset =
  let result0 =
        if null col && rj == 0 && up
          then [ (ri + 1 + offset, 0) | _ <- [1 .. yOffset] ]
          else []
      loop j acc =
        if j >= length col
          then reverse acc
          else
            let e = col !! j
                (v, sep) = e
                acc1 =
                  if j == rj && up
                    then [ (v + offset, 0) | _ <- [1 .. yOffset] ] ++ acc
                    else acc
                nv =
                  if v > ri + 1 || (v == ri + 1 && (up || j < rj))
                    then v + offset
                    else v
             in loop (j + 1) ((nv, sep) : acc1)
   in loop 0 result0

expandInfinite :: Expr -> Int -> Bool -> Expr
expandInfinite m index shorter = result
  where
    n = length m
    right = n - 1
    rc = m !! right
    top = length rc - 1
    vv = exprVert m
    p = parents m vv
    rp = (p !! right) !! top
    (ri, rj) = rp
    offset = right - ri
    yOffset = if top == rj then 1 else top - rj
    upArr0 = [0 | _ <- [0 .. n - 1]]
    upArr1 = foldl applyUp upArr0 [ri + 1 .. right - 1]
    upArr2 = setAt right 1 upArr1
    applyUp arr i =
      let col = m !! i
       in if not (hasInfinite col)
            then
              if finiteHeight col <= rj + 1
                then arr
                else
                  let pi = fst ((p !! i) !! rj)
                   in setAt i (arr !! pi) arr
            else
              if finiteHeight col < rj
                then arr
                else
                  if finiteHeight col > rj
                    then
                      let pi = fst ((p !! i) !! rj)
                       in setAt i (arr !! pi) arr
                    else
                      let thresholdCol = [ (i, 0) | _ <- [0 .. rj] ] ++ [(ri, 1)]
                          xEnd = findXEnd (i + 1)
                          findXEnd xe
                            | xe >= n = xe
                            | colCompare (m !! xe) thresholdCol >= 0 = findXEnd (xe + 1)
                            | otherwise = xe
                          yEnd = n
                          yStart = findYStart right
                          findYStart ys
                            | finiteHeight (m !! ys) <= rj = ys
                            | otherwise = findYStart (fst ((p !! ys) !! rj))
                          upI = findUp 1 (-1)
                          findUp k ui
                            | k + i >= xEnd || k + yStart >= yEnd =
                                if ui < 0 then (if xEnd - i >= yEnd - yStart then 1 else 0) else ui
                            | otherwise =
                                let xCol = [ (if v >= i then v + yStart - i else v, sep) | (v, sep) <- m !! (i + k) ]
                                    yCol = m !! (yStart + k)
                                    c = colCompare xCol yCol
                                 in if c /= 0
                                      then (if c > 0 then 1 else 0)
                                      else findUp (k + 1) ui
                       in setAt i upI arr
    base = subtract1 m
    result1 = foldl addCopy base [1 .. index]
    addCopy rs w = foldl (addOne w) rs [ri + 1 .. right]
    addOne w rs i =
      let off = offset * w
       in rs ++ [copyColumnV1a (rs !! i) ri rj off (upArr2 !! i /= 0) (yOffset * w)]
    result
      | shorter = init result1
      | otherwise = result1

expand :: Expr -> Int -> Bool -> Expr
expand m index shorter
  | isInfinity m = v1aInfinityFs index
  | null m = m
  | null (last m) = init m
  | not (hasInfinite (last m)) = smnExpand m index shorter
  | otherwise = expandInfinite m index shorter

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = if term < 1 then 1 else term
      isInf = text == "Limit" || text == "limit" || text == "LIMIT"
   in if isInf
        then displayExpr (expand infinityValue term' (not longer))
        else
          case parseExpr text of
            Nothing -> errP
            Just e ->
              if null e then ""
              else if any (\col -> any (\(v, _) -> v < 0 || v >= length e) col) e
                then errP
                else displayExpr (expand e term' (not longer))

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandAlter :: String -> Int -> String
expandAlter text term = expandTextImpl text term True
