-- | RPD / ARD / ARD2（11242 家族）：忠实移植 C++ @notation/graph11242.hpp@。
module OmegaY.Fam.G11242.Graph
  ( rpdNotation
  , ardNotation
  , ard2Notation
  , expandRpd
  , expandArd
  , expandArd2
  ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (maximumBy, sortBy)
import qualified Data.Map.Strict as M
import OmegaY.Fam.G11242.State hiding (tick)
import OmegaY.Notation.Types (Notation, textNotation)

data Edge = Edge
  { eK :: Int
  , eP :: Int
  , eQ :: Int
  }
  deriving (Eq, Show)

data Kind = RPD | ARD | ARD2
  deriving (Eq)

type Graph = [[Edge]]
type Budget = (Int, Int) -- (work, copied)

kWidth, kCells :: Int
kWidth = 8192
kCells = 250000

failM :: String -> M s a
failM msg = M $ \_ -> Left msg

tick :: M Budget ()
tick = do
  (w, c) <- get
  if w + 1 > kCells * 4
    then failM "计算预算不足"
    else put (w + 1, c)

moveTick :: M Budget ()
moveTick = do
  tick
  (w, c) <- get
  if c + 1 > kCells
    then failM "计算预算不足"
    else put (w, c + 1)

pairCompare :: Edge -> Edge -> Ordering
pairCompare a b
  | eK a /= eK b = compare (eK a) (eK b)
  | otherwise = compare (eQ a) (eQ b)

edgeCompare :: Edge -> Edge -> Ordering
edgeCompare a b
  | eP a /= eP b = compare (eP a) (eP b)
  | otherwise = pairCompare a b

sortDescEdge :: [Edge] -> [Edge]
sortDescEdge = sortBy (\a b -> case edgeCompare a b of
  LT -> GT
  GT -> LT
  EQ -> EQ)

-- | 按父列 p 取 pairCompare 最大者，再按 p 降序做“天际线”剪枝。
skyline :: [Edge] -> [Edge]
skyline col =
  let best = M.fromListWith pickMax [(eP e, e) | e <- col]
      byP = sortBy (\a b -> compare (eP b) (eP a)) (M.elems best)
   in go byP Nothing
 where
  pickMax a b = if pairCompare a b >= EQ then a else b
  go [] _ = []
  go (e : es) Nothing = e : go es (Just e)
  go (e : es) (Just high)
    | pairCompare e high == GT = e : go es (Just e)
    | otherwise = go es (Just high)

checkEdge :: Kind -> Int -> Edge -> M Budget ()
checkEdge kind j e = do
  tick
  if eK e < 0 || eP e < 0 || eQ e < 0 then failM "需要非负整数" else pure ()
  let p = eP e
      k = eK e
      q = eQ e
      bad
        | p >= j = True
        | kind == ARD2 = k > j || q > j
        | otherwise = q > p || (kind == ARD && k > p)
  if bad then failM "父列、层标或根标超出本记号允许的范围" else pure ()

normalize :: Kind -> [[Edge]] -> M Budget [[Edge]]
normalize kind cols0 = do
  if length cols0 > kWidth then failM "列数超出预算" else pure ()
  if kind == RPD then rpd cols0 0 0 else other cols0 0 0
 where
  rpd [] _ _ = pure []
  rpd (col : rest) j cells = do
    (rec, cells') <- rpdCol col j cells M.empty
    (rec :) <$> rpd rest (j + 1) cells'
   where
    rpdCol [] _ cells acc = pure (sortDescEdge (M.elems acc), cells)
    rpdCol (e : es) j cells acc = do
      checkEdge kind j e
      if eQ e >= kCells then failM "计算预算不足" else pure ()
      let qs = [0 .. eQ e]
      (acc', cells') <- foldQ acc cells qs e
      rpdCol es j cells' acc'
    foldQ acc cells [] _ = pure (acc, cells)
    foldQ acc cells (q : qs) e = do
      tick
      let acc' = M.insert (eK e, eP e, q) (Edge (eK e) (eP e) q) acc
      if cells + 1 > kCells then failM "计算预算不足" else pure ()
      foldQ acc' (cells + 1) qs e

  other [] _ _ = pure []
  other (col : rest) j cells = do
    mapM_ (checkEdge kind j) col
    let sky = skyline col
        cells' = cells + length sky
    if cells' > kCells then failM "计算预算不足" else (sky :) <$> other rest (j + 1) cells'

predecessor :: Kind -> Edge -> Int -> [Edge]
predecessor kind e child
  | eQ e /= 0 = [Edge (eK e) (eP e) (eQ e - 1)]
  | eK e /= 0 = [Edge (eK e - 1) (eP e) (if kind == ARD2 then child else eP e)]
  | otherwise = []

seed :: Kind -> Int -> M Budget Graph
seed kind index = do
  let cap = if kind == RPD then 4096 else kWidth
  if index < 0 || index > cap then failM "输入过大" else pure ()
  if kind == RPD
    then pure [[], [Edge k 0 0 | k <- [index, index - 1 .. 0]]]
    else
      let v j = j - (if kind == ARD then 1 else 0)
       in pure [ if j == 0 then [] else [Edge (v j) (j - 1) (v j)] | j <- [0 .. index - 1] ]

fs :: Kind -> Graph -> Int -> M Budget Graph
fs kind graph n
  | n < 0 = failM "需要非负整数"
  | x < 0 = pure []
  | n == 0 || null (graph !! x) = pure (take x graph)
  | otherwise = do
      let last_ = graph !! x
          control
            | kind == RPD =
                maximumBy
                  (\a b -> case pairCompare a b of
                    EQ -> compare (eP a) (eP b)
                    c -> c)
                  last_
            | otherwise = last last_
          c = eP control
          width = x - c
      if x + n * width > kWidth then failM "展开列数超出预算" else go 0 (take x graph) width c control last_
 where
  x = length graph - 1

  go v result width c control last_
    | v >= n = normalize kind result
    | otherwise = do
        tick
        let phi block i = if i < c then i else i + block * width
            moveE block e = do
              moveTick
              if kind == RPD
                then pure (Edge (eK e) (phi block (eP e)) (phi block (eQ e)))
                else pure (Edge (phi block (eK e)) (phi block (eP e)) (phi block (eQ e)))
        seam0 <-
          if kind == RPD
            then do
              let cutRoot = phi v (eQ control)
                  goSeam acc [] = pure acc
                  goSeam acc (e : es) = do
                    shifted <- moveE v e
                    let mx
                          | eK e < eK control = eQ shifted
                          | eK e == eK control = min (eQ shifted) (cutRoot - 1)
                          | otherwise = -1
                    goSeam (if mx >= 0 then acc ++ [Edge (eK shifted) (eP shifted) mx] else acc) es
              goSeam [] last_
            else do
              front <- mapM (moveE v) (init last_)
              shiftedControl <- moveE v control
              pure (front ++ predecessor kind shiftedControl (x + v * width))
        fromC <- mapM (moveE (v + 1)) (graph !! c)
        restCols <- mapM (\j -> mapM (moveE (v + 1)) (graph !! j)) [c + 1 .. x - 1]
        go (v + 1) (result ++ [seam0 ++ fromC] ++ restCols) width c control last_

plain :: Graph -> String
plain g
  | null g = "0"
  | otherwise = concat [renderCol col | col <- g]
 where
  renderCol col = "[" ++ joinEdges col ++ "]"
  joinEdges [] = ""
  joinEdges [e] = renderEdge e
  joinEdges (e : es) = renderEdge e ++ concat ["," ++ renderEdge x | x <- es]
  renderEdge e = "(" ++ show (eK e) ++ "," ++ show (eP e) ++ "," ++ show (eQ e) ++ ")"

parse :: Kind -> String -> M Budget Graph
parse kind raw = case routeOr of
  Just g -> g
  Nothing -> ordinary
 where
  s = filter (not . isSpace) raw

  routeOr = case s of
    (ch : rest) | toLower ch == 'a' ->
      let (v, next, any) = readDigitsAt 0 rest
       in if any then Just (route v (drop next rest)) else Nothing
    _ -> Nothing

  route v rest0 = do
    g0 <- seed kind v
    go 128 g0 rest0
   where
    go 0 g rest
      | null rest = pure g
      | otherwise = failM "表达式后有未识别内容"
    go steps g rest = case rest of
      ('[' : r1) ->
        let (v, next, _) = readDigitsAt 0 r1
         in if next < length r1 && r1 !! next == ']'
              then do
                put (0, 0)
                g' <- fs kind g v
                go (steps - 1) g' (drop (next + 1) r1)
              else failM "路由展开格式错误"
      _ -> if null rest then pure g else failM "表达式后有未识别内容"

  ordinary
    | null s || s == "∅" = pure []
    | all isDigit s =
        let n = foldl (\a ch -> a * 10 + fromEnum ch - fromEnum '0') 0 s
         in if n > kWidth then failM "输入过大" else do
              put (0, 0)
              normalize kind (replicate n [])
    | otherwise = columns 0 []

  n = length s

  columns i out
    | i >= n =
        if null out then failM "请输入 [(层,父列,根)] 列表" else do
          put (0, 0)
          normalize kind out
    | s !! i /= '[' = failM "请输入 [(层,父列,根)] 列表"
    | otherwise = do
        (col, j) <- parseCol (i + 1) []
        if j >= n || s !! j /= ']'
          then failM "请输入 [(层,父列,根)] 列表"
          else columns (j + 1) (out ++ [col])

  parseCol j acc
    | j < n && s !! j == '(' = do
        let (a, p1, any1) = readDigitsAt (j + 1) s
        if not any1 || p1 >= n || s !! p1 /= ',' then failM "请输入 [(层,父列,根)] 列表" else do
          let (b, p2, any2) = readDigitsAt (p1 + 1) s
          if not any2 || p2 >= n || s !! p2 /= ',' then failM "请输入 [(层,父列,根)] 列表" else do
            let (c, p3, any3) = readDigitsAt (p2 + 1) s
            if not any3 then failM "请输入 [(层,父列,根)] 列表" else do
              let p4 = if p3 < n && s !! p3 == ')' then p3 + 1 else p3
                  acc' = acc ++ [Edge a b c]
              if p4 < n && s !! p4 == ',' then parseCol (p4 + 1) acc' else pure (acc', p4)
    | j < n && s !! j == ',' = parseCol (j + 1) acc
    | otherwise = pure (acc, j)

-- | 从整串第 i 位起读十进制数字；返回 (值, 结束位置, 是否读到数字)。
readDigitsAt :: Int -> String -> (Int, Int, Bool)
readDigitsAt i s =
  let (ds, _) = span isDigit (drop i s)
   in (foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0 ds, i + length ds, not (null ds))

expand :: Kind -> String -> Int -> String
expand kind text term =
  case runM (parse kind text >>= \g -> put (0, 0) >> fs kind g term) (0, 0) of
    Left e -> e
    Right g -> plain g

expandRpd, expandArd, expandArd2 :: String -> Int -> String
expandRpd = expand RPD
expandArd = expand ARD
expandArd2 = expand ARD2

rpdNotation, ardNotation, ard2Notation :: Notation
rpdNotation = textNotation "rpd" "RPD" ["11242", "文本"] expandRpd ""
ardNotation = textNotation "ard" "ARD" ["11242", "文本"] expandArd ""
ard2Notation = textNotation "ard2" "ARD2" ["11242", "文本"] expandArd2 ""
