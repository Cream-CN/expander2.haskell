-- | e0MN 族共享核心：序数(Ordinal) / 表达式(Expr) 的解析、显示与基本列机制。
--   逐行对齐 C++ @Header/notation/mnordshare.hpp@。
module OmegaY.Fam.Mn.MnOrdShare
  ( Term(..)
  , Ordinal
  , Entry(..)
  , Column
  , Expr
  -- * 构造与判定
  , ordNat
  , ordOne
  , isZero
  , isOne
  , isPositive
  , omegaTower
  , isLimitExpr
  , predecessor
  , towerIndex
  -- * 序数比较
  , ordCompare
  , ordEq
  , ordIsSuccessor
  -- * 序数运算
  , ordFS
  , shiftOrdNumbers
  -- * 解析与显示
  , parseOrd
  , ordToPlain
  , parseExpr
  , colToPlain
  , exprToPlain
  -- * 基本列机制
  , down
  , rfl
  , fs
  , shortExpansion
  , fsShort
  , entryCompare
  , colCompare
  , exprCompare
  ) where

import Data.Char (isDigit, toLower)
import Data.List (foldl')

-- 数据类型

data Term = Term { tExp :: Ordinal, tCoeff :: Int }
  deriving (Eq, Show)

type Ordinal = [Term]

data Entry = Entry { eA :: Int, eX :: Ordinal }
  deriving (Eq, Show)

type Column = [Entry]
type Expr = [Column]

-- 基础构造与判定

ordNat :: Int -> Ordinal
ordNat n | n <= 0 = []
         | otherwise = [Term [] n]

ordOne :: Ordinal
ordOne = ordNat 1

isZero :: Ordinal -> Bool
isZero = null

isOne :: Ordinal -> Bool
isOne [Term [] 1] = True
isOne _ = False

isPositive :: Ordinal -> Bool
isPositive = not . null

omegaTower :: Int -> Ordinal
omegaTower n | n < 0 = []
             | otherwise = go n
  where go 0 = ordOne
        go k = [Term (go (k - 1)) 1]

isLimitExpr :: Expr -> Bool
isLimitExpr e = not (null e) && not (null (last e))

predecessor :: Expr -> Expr
predecessor [] = []
predecessor e = init e

towerIndex :: Ordinal -> Int
towerIndex a
  | isOne a = 0
  | length a /= 1 || tCoeff (head a) /= 1 || isZero (tExp (head a)) = -1
  | otherwise = case towerIndex (tExp (head a)) of
      k | k < 0 -> -1
        | otherwise -> k + 1

-- 序数比较

numCompare :: Int -> Int -> Int
numCompare a b = case compare a b of { LT -> -1; EQ -> 0; GT -> 1 }

ordCompare :: Ordinal -> Ordinal -> Int
ordCompare a b = go 0
  where
    n = min (length a) (length b)
    go i
      | i >= n = numCompare (length a) (length b)
      | otherwise =
          let c = ordCompare (tExp (a !! i)) (tExp (b !! i))
           in if c /= 0
                then c
                else case numCompare (tCoeff (a !! i)) (tCoeff (b !! i)) of
                       0 -> go (i + 1)
                       c' -> c'

ordEq :: Ordinal -> Ordinal -> Bool
ordEq a b = ordCompare a b == 0

ordIsSuccessor :: Ordinal -> Bool
ordIsSuccessor a = isPositive a && isZero (tExp (last a))

-- 序数运算

ordFS :: Ordinal -> Int -> Ordinal
ordFS src0 m
  | null src0 = []
  | otherwise =
      let lastT = last src0
       in if isZero (tExp lastT)
            then if tCoeff lastT == 1
                   then init src0
                   else init src0 ++ [lastT { tCoeff = tCoeff lastT - 1 }]
            else
              let exp = tExp lastT
                  coeff = tCoeff lastT
                  a0 = init src0
                  a1 = if coeff > 1 then a0 ++ [Term exp (coeff - 1)] else a0
                  nextExp = ordFS exp m
               in if ordIsSuccessor exp
                    then (if m /= 0 then a1 ++ [Term nextExp m] else a1)
                    else a1 ++ [Term nextExp 1]

shiftOrdNumbers :: Ordinal -> Int -> Int -> Bool -> Ordinal
shiftOrdNumbers a cutoff d strong = map go a
  where
    go t
      | not strong && isZero (tExp t) =
          t { tCoeff = if tCoeff t > cutoff then tCoeff t + d else tCoeff t }
      | strong =
          t { tExp = shiftOrdNumbers (tExp t) cutoff d strong
            , tCoeff = if tCoeff t >= cutoff then tCoeff t + d else tCoeff t
            }
      | otherwise =
          t { tExp = if isOne (tExp t) then tExp t else shiftOrdNumbers (tExp t) cutoff d strong
            , tCoeff = if tCoeff t /= 1 && tCoeff t > cutoff then tCoeff t + d else tCoeff t
            }

-- 解析与显示

-- | ω 的哨兵字符（与 C++ '\x7F' 对应）。
kW :: Char
kW = '\x7F'

normalizeOmega :: String -> String
normalizeOmega = go
  where
    go [] = []
    -- Haskell getContents 已把 UTF-8 解码为 Unicode Char；ω (U+03C9) 直接替换。
    go (c:rest)
      | c == 'ω' = kW : go rest
      | c == 'w' = kW : go rest
      | c == 'W' = kW : go rest
      | (c == 'o' || c == 'O'),
        (c2:c3:c4:c5:_) <- rest,
        map toLower [c,c2,c3,c4,c5] == "omega" = kW : go (drop 4 rest)
      | otherwise = c : go rest

-- ω^^n 捷径：仅全串匹配。
towerShortcut :: String -> Maybe Ordinal
towerShortcut s =
  case s of
    (c1:c2:c3:rest) | c1 == kW && c2 == '^' && c3 == '^' ->
      if null rest || not (all isDigit rest)
        then Nothing
        else
          let n = foldl' (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 rest
           in if n > 1000000 then Nothing else Just (omegaTower n)
    _ -> Nothing

numberAt :: String -> Int -> Maybe (Int, Int)
numberAt s i0 =
  let n = length s
   in if i0 >= n || not (isDigit (s !! i0))
        then Nothing
        else
          let j = walk i0
              walk k = if k < n && isDigit (s !! k) then walk (k + 1) else k
              digits = take (j - i0) (drop i0 s)
              v = foldl' (\a d -> a * 10 + (fromEnum d - fromEnum '0')) 0 digits
           in Just (v, j)

peTerm :: String -> Int -> Maybe (Term, Int)
peTerm s i0
  | i0 >= length s = Nothing
  | otherwise =
      let c = s !! i0
       in if isDigit c
            then case numberAt s i0 of
                   Nothing -> Nothing
                   Just (v, i1) ->
                     if v <= 0 then Nothing else Just (Term [] v, i1)
            else if c /= kW
              then Nothing
              else
                let i1 = i0 + 1
                 in case peExponentAfterW s i1 of
                      Nothing -> Nothing
                      Just (exp, i2) ->
                        case peCoeff s i2 of
                          Nothing -> Nothing
                          Just (coeff, i3) -> Just (Term exp coeff, i3)

peExponentAfterW :: String -> Int -> Maybe (Ordinal, Int)
peExponentAfterW s i0 =
  let n = length s
   in if i0 < n && s !! i0 == '^'
        then peExponentAt s (i0 + 1)
        else Just (ordOne, i0)

peCoeff :: String -> Int -> Maybe (Int, Int)
peCoeff s i0 =
  let n = length s
   in if i0 < n && isDigit (s !! i0)
        then case numberAt s i0 of
               Nothing -> Nothing
               Just (v, i1) -> if v <= 0 then Nothing else Just (v, i1)
        else Just (1, i0)

peExponentAt :: String -> Int -> Maybe (Ordinal, Int)
peExponentAt s i0
  | i0 >= length s = Nothing
  | otherwise = case s !! i0 of
      c | c == '{' || c == '(' ->
          let close = if c == '{' then '}' else ')'
              i1 = i0 + 1
           in if i1 < length s && s !! i1 == close
                then Just ([], i1 + 1)
                else case peExprAt s i1 close of
                       Nothing -> Nothing
                       Just (e, i2) ->
                         if i2 < length s && s !! i2 == close
                           then Just (e, i2 + 1)
                           else Nothing
            | isDigit c ->
                case numberAt s i0 of
                  Nothing -> Nothing
                  Just (v, i1) -> Just (ordNat v, i1)
            | c == kW ->
                case peTerm s i0 of
                  Nothing -> Nothing
                  Just (t, i1) -> Just ([t], i1)
            | otherwise -> Nothing

peExprAt :: String -> Int -> Char -> Maybe (Ordinal, Int)
peExprAt s i0 end =
  let n = length s
   in if i0 < n && s !! i0 == '0' &&
         (i0 + 1 == n || (end /= '\0' && i0 + 1 < n && s !! (i0 + 1) == end))
        then Just ([], i0 + 1)
        else go i0 []
  where
    go i acc =
      let n = length s
       in if i >= n || (end /= '\0' && s !! i == end)
            then Just (reverse acc, i)
            else case peTerm s i of
                   Nothing -> Nothing
                   Just (t, i1) ->
                     let acc' = t : acc
                      in if i1 < length s && s !! i1 == '+'
                           then let i2 = i1 + 1
                                 in if i2 >= length s || (end /= '\0' && s !! i2 == end)
                                      then Nothing
                                      else go i2 acc'
                           else Just (reverse acc', i1)

parseOrd :: String -> Bool -> Maybe Ordinal
parseOrd text allowZero =
  let s0 = normalizeOmega text
      s = filter (`notElem` " \t\r\n") s0
   in if null s
        then Nothing
        else if s == "0"
          then if allowZero then Just [] else Nothing
          else case towerShortcut s of
                 Just r -> Just r
                 Nothing ->
                   case peExprAt s 0 '\0' of
                     Just (out, i)
                       | i == length s, allowZero || not (null out) -> Just out
                     _ -> Nothing

ordToPlain :: Ordinal -> String
ordToPlain a
  | null a = "0"
  | otherwise = concat (zipWith (\i t -> (if i > 0 then "+" else "") <> termStr t) [0..] a)
  where
    termStr t
      | isZero (tExp t) = show (tCoeff t)
      | otherwise =
          let e = "ω" <> (if isOne (tExp t) then "" else "^{" <> ordToPlain (tExp t) <> "}")
           in e <> (if tCoeff t /= 1 then show (tCoeff t) else "")

-- e0MN 表达式比较

entryCompare :: Entry -> Entry -> Int
entryCompare a b =
  case numCompare (eA a) (eA b) of
    0 -> ordCompare (eX a) (eX b)
    c -> c

colCompare :: Column -> Column -> Int
colCompare a b = lexCompare a b entryCompare

lexCompare :: [a] -> [a] -> (a -> a -> Int) -> Int
lexCompare a b cmp =
  let n = min (length a) (length b)
      go i
        | i >= n = numCompare (length a) (length b)
        | otherwise = case cmp (a !! i) (b !! i) of
                        0 -> go (i + 1)
                        c -> c
   in go 0

exprCompare :: Expr -> Expr -> Int
exprCompare a b = lexCompare a b colCompare

-- 表达式解析与显示

colToPlain :: Column -> String
colToPlain c
  | null c = "()"
  | otherwise = "(" <> concat (zipWith (\i e -> (if i > 0 then ";" else "") <> show (eA e) <> ":" <> ordToPlain (eX e)) [0..] c) <> ")"

exprToPlain :: Expr -> String
exprToPlain e = concatMap colToPlain e

splitTop :: String -> Char -> [String]
splitTop s sep = go 0 0 0 0
  where
    n = length s
    go p b start i
      | i >= n = [drop start s]
      | otherwise =
          let ch = s !! i
           in case ch of
                '(' -> go (p + 1) b start (i + 1)
                ')' -> go (p - 1) b start (i + 1)
                '{' -> go p (b + 1) start (i + 1)
                '}' -> go p (b - 1) start (i + 1)
                _ | p == 0 && b == 0 && ch == sep ->
                      take (i - start) (drop start s) : go p b (i + 1) (i + 1)
                  | otherwise -> go p b start (i + 1)

topColon :: String -> Int
topColon s = go 0 0 0
  where
    n = length s
    go p b i
      | i >= n = -1
      | otherwise =
          let ch = s !! i
           in case ch of
                '(' -> go (p + 1) b (i + 1)
                ')' -> go (p - 1) b (i + 1)
                '{' -> go p (b + 1) (i + 1)
                '}' -> go p (b - 1) (i + 1)
                _ | p == 0 && b == 0 && ch == ':' -> i
                  | otherwise -> go p b (i + 1)

trimWS :: String -> String
trimWS = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

parseColumn :: String -> Maybe Column
parseColumn s0 =
  let body = trimWS s0
   in if null body then Just []
      else go (splitTop body ';')
  where
    go [] = Just []
    go (piece:rest) =
      let ci = topColon piece
       in if ci < 0 then Nothing
          else
            let aText = trimWS (take ci piece)
                xText = trimWS (drop (ci + 1) piece)
             in if null aText || any (not . isDigit) aText
                  then Nothing
                  else
                    let a = foldl' (\x d -> x * 10 + (fromEnum d - fromEnum '0')) 0 aText
                     in if a > 1000000 || a <= 0
                          then Nothing
                          else case parseOrd xText False of
                                 Nothing -> Nothing
                                 Just x -> (Entry a x :) <$> go rest

parseExpr :: String -> Maybe Expr
parseExpr text =
  let raw = trimWS text
   in if null raw || raw == "0" || raw == "∅"
        then Just []
        else go raw
  where
    go raw = scan raw 0 []
      where
        nRaw = length raw
        scan s i acc
          | i >= nRaw = Just (reverse acc)
          | otherwise =
              let i1 = skipWS s i
               in if i1 >= nRaw
                    then Just (reverse acc)
                    else if s !! i1 /= '('
                      then Nothing
                      else
                        case findClose s i1 of
                          Nothing -> Nothing
                          Just endPos ->
                            let body = take (endPos - i1 - 1) (drop (i1 + 1) s)
                             in case parseColumn body of
                                  Nothing -> Nothing
                                  Just col -> scan s (endPos + 1) (col : acc)
    skipWS :: String -> Int -> Int
    skipWS s i = let n = length s in if i < n && s !! i `elem` " \t" then skipWS s (i + 1) else i
    findClose :: String -> Int -> Maybe Int
    findClose s i = goF 0 0 i
      where
        n = length s
        goF p b j
          | j >= n = Nothing
          | otherwise =
              let ch = s !! j
               in case ch of
                    '(' -> goF (p + 1) b (j + 1)
                    ')' -> if p == 1 && b == 0 then Just j else goF (p - 1) b (j + 1)
                    '{' -> goF p (b + 1) (j + 1)
                    '}' -> goF p (b - 1) (j + 1)
                    _ -> goF p b (j + 1)

-- 基本列机制

down :: Expr -> Bool -> Expr
down src0 strong
  | not (isLimitExpr src0) = predecessor src0
  | otherwise =
      let e = src0
          l = length e
          lastCol = last e
          n = length lastCol
          an = eA (last lastCol)
          xn = eX (last lastCol)
          d = l - an
       in if d <= 0
            then src0
            else
              let sourceCol = e !! (an - 1)
                  s = countWhile sourceCol xn
                  newLastInit = init lastCol
                  xnL = ordFS xn (l - 1)
                  prevX = if n > 1 then eX (lastCol !! (n - 2)) else []
                  omit = isOne xn ||
                         (n > 1 &&
                           (if strong
                              then ordEq xnL prevX
                              else ordCompare xnL prevX <= 0))
                  newLastMid = if not omit && isPositive xnL
                                 then newLastInit ++ [Entry an xnL]
                                 else newLastInit
                  newLast = newLastMid ++ drop s sourceCol
                  e' = init e ++ [newLast]
               in e'
  where
    countWhile col xn = go 0 col
      where
        go i c
          | i < length col && ordCompare (eX (col !! i)) xn < 0 = go (i + 1) (drop 1 c)
          | otherwise = i

processRflCol :: Column -> Int -> Int -> Bool -> Column
processRflCol c cutoff d strong =
  map (\z -> Entry (if eA z >= cutoff then eA z + d else eA z)
                   (shiftOrdNumbers (eX z) cutoff d strong)) c

rfl :: Expr -> Bool -> Expr
rfl src0 strong
  | not (isLimitExpr src0) = predecessor src0
  | otherwise =
      let l = length src0
          an = eA (last (last src0))
          d = l - an
       in if d <= 0
            then src0
            else
              let out = down src0 strong
                  cols = [src0 !! (col - 1) | col <- [an + 1 .. l]]
                  newCols = map (\c -> processRflCol c an d strong) cols
               in out ++ newCols

fs :: Expr -> Int -> Bool -> Bool -> Expr
fs src0 m shorter strong
  | m <= 0 || not (isLimitExpr src0) = predecessor src0
  | otherwise =
      let r0 = iterate (\x -> rfl x strong) src0 !! m
       in if shorter
            then predecessor r0
            else
              let l = length r0
                  r1 = rfl r0 strong
               in if length r1 > l then take l r1 else r1

shortExpansion :: Expr -> Bool -> Expr
shortExpansion src0 strong
  | not (isLimitExpr src0) = predecessor src0
  | otherwise =
      let l = length src0
          lastCol = last src0
          n = length lastCol
          an = eA (last lastCol)
          xn = eX (last lastCol)
          prevX = if n > 1 then eX (lastCol !! (n - 2)) else []
          xnL = ordFS xn (l - 1)
          omit = isOne xn ||
                 (n > 1 &&
                   (if strong
                      then ordEq xnL prevX
                      else ordCompare xnL prevX <= 0))
       in if omit
            then down src0 strong
            else
              let xnS0 = ordFS xn 0
               in findS 0 xnS0 prevX src0 strong n an xn

findS :: Int -> Ordinal -> Ordinal -> Expr -> Bool -> Int -> Int -> Ordinal -> Expr
findS s xnS prevX src0 strong n an xn
  | ordCompare xnS prevX <= 0 =
      let s' = s + 1
       in if s' > 1000000
            then src0
            else findS s' (ordFS xn s') prevX src0 strong n an xn
  | otherwise =
      let l = length src0
          e = src0
          lastCol = last e
          newLast = init lastCol ++ [Entry an xnS]
       in init e ++ [newLast]

fsShort :: Expr -> Int -> Bool -> Expr
fsShort src0 m strong
  | m <= 0 || not (isLimitExpr src0) = predecessor src0
  | otherwise =
      let shortEx = shortExpansion src0 strong
       in if m == 1
            then shortEx
            else
              let first = fs src0 1 True strong
                  m' = if exprCompare shortEx first == 0 then m else m - 1
               in fs src0 m' True strong
