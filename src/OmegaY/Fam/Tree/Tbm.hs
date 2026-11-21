-- | TBM（超限巴斯丘矩阵）树形矩阵记号。忠实移植 @Header/notation/tbm.hpp@。
--
--   数据层级：Expr = Column[]，Column = Entry[]，Entry = (value, height:Expr)。
--   本模块同时导出 TUPMS 需要复用的全部原语（比较链、竖直序列、parents、
--   列运算、解析、显示）。
module OmegaY.Fam.Tree.Tbm
  ( Expr (..)
  , Entry (..)
  , Column
  , oneExpr
  , makeOmega
  , isOne
  , cmpInt
  , exprCmp
  , entryCmp
  , columnCmp
  , verticalCmp
  , indexAfter
  , verticalParent
  , verticalAdd
  , columnVerticals
  , parents
  , toVertical
  , columnSub
  , ascensionThreshold
  , columnAdd
  , columnTruncate
  , columnMul
  , copyColumn
  , isLimit
  , tbmFs
  , display
  , parseExprText
  , kParseError
  , expandText
  ) where

import Data.Char (isDigit)

-- ---------------------------------------------------------------------------
-- 数据
-- ---------------------------------------------------------------------------

data Expr = Expr { cols :: [Column] } deriving (Eq)
type Column = [Entry]
data Entry = Entry { eValue :: Int, eHeight :: Expr } deriving (Eq)

-- | 高度为 1：单列且该列为空。
oneExpr :: Expr
oneExpr = Expr [[]]

makeOmega :: Expr
makeOmega = Expr [[], [Entry 1 oneExpr]]

isOne :: Expr -> Bool
isOne (Expr [[]]) = True
isOne _ = False


kParseError :: String
kParseError = "解析失败，请确认你没有使用全角括号，逗号"

-- ---------------------------------------------------------------------------
-- 比较
-- ---------------------------------------------------------------------------

cmpInt :: Int -> Int -> Int
cmpInt a b = case compare a b of
  LT -> -1
  EQ -> 0
  GT -> 1

entryCmp :: Entry -> Entry -> Int
entryCmp a b =
  case cmpInt (eValue a) (eValue b) of
    0 -> exprCmp (eHeight a) (eHeight b)
    c -> c

columnCmp :: Column -> Column -> Int
columnCmp a b = go 0
 where
  n = min (length a) (length b)
  go i
    | i >= n = cmpInt (length a) (length b)
    | otherwise = case entryCmp (a !! i) (b !! i) of
      0 -> go (i + 1)
      c -> c

exprCmp :: Expr -> Expr -> Int
exprCmp a b = go 0
 where
  ca = cols a
  cb = cols b
  n = min (length ca) (length cb)
  go i
    | i >= n = cmpInt (length ca) (length cb)
    | otherwise = case columnCmp (ca !! i) (cb !! i) of
      0 -> go (i + 1)
      c -> c

verticalCmp :: [Expr] -> [Expr] -> Int
verticalCmp a b = go 0
 where
  n = min (length a) (length b)
  go i
    | i >= n = cmpInt (length a) (length b)
    | otherwise = case exprCmp (a !! i) (b !! i) of
      0 -> go (i + 1)
      c -> c

-- ---------------------------------------------------------------------------
-- 竖直序列
-- ---------------------------------------------------------------------------

-- | 在 V 中找到第一个 > pos 的下标；都不大于则返回 V.size。
indexAfter :: [[Expr]] -> [Expr] -> Int
indexAfter v pos = go 0
 where
  go k
    | k >= length v = length v
    | verticalCmp (v !! k) pos > 0 = k
    | otherwise = go (k + 1)

-- | 返回第一个 Vi[k] >= v 对应的 Pi[k]。
verticalParent :: [Expr] -> [(Int, Int)] -> [[Expr]] -> Maybe (Int, Int)
verticalParent v pi vi = go 0
 where
  go k
    | k >= length vi = Nothing
    | verticalCmp (vi !! k) v >= 0 = Just (pi !! k)
    | otherwise = go (k + 1)

verticalAdd :: [Expr] -> [Expr] -> [Expr]
verticalAdd v1 v2
  | null v1 = v2
  | null v2 = v1
  | otherwise = take i v1 ++ v2
 where
  first2 = head v2
  i = dropTrailing (length v1)
  dropTrailing k
    | k > 0 && exprCmp (v1 !! (k - 1)) first2 < 0 = dropTrailing (k - 1)
    | otherwise = k

columnVerticals :: Column -> [[Expr]]
columnVerticals col = reverse (snd (foldl step ([], []) col))
 where
  step (acc, out) en =
    let acc' = verticalAdd acc [eHeight en]
    in (acc', acc' : out)

-- ---------------------------------------------------------------------------
-- parents
-- ---------------------------------------------------------------------------

-- | 对每一列逐层求父项链。某行找不到父项时该列更高行不再生成（与 C++ 一致）。
parents :: Expr -> [[[Expr]]] -> [[(Int, Int)]]
parents (Expr mcols) v = allRows
 where
  n = length mcols
  allRows = [buildCol i | i <- [0 .. n - 1]]
  buildCol i = goEntries 0 []
   where
    col = mcols !! i
    goEntries j pi
      | j >= length col = pi
      | otherwise =
        let value = eValue (col !! j)
            pos = if j /= 0 then v !! i !! (j - 1) else []
            p0 = if j == 0 then i - 1 else fst (last pi)
        in case walk p0 j pos value of
             Nothing -> pi
             Just pair -> goEntries (j + 1) (pi ++ [pair])
    walk p j pos value
      | p < 0 = Nothing
      | otherwise =
        let pcol = mcols !! p
        in if null pcol
             then if 0 < value
                    then Just (p, 0)
                    else Nothing
             else
               let jp = if j == 0 then 0 else indexAfter (v !! p) pos
               in if jp >= length pcol
                    then Just (p, jp)
                    else
                      let valueP = eValue (pcol !! jp)
                      in if valueP < value
                           then Just (p, jp)
                           else case nextP of
                                  Nothing -> Nothing
                                  Just (np0, _) -> walk np0 j pos value
     where
      nextP =
        if j == 0
          then Just (p - 1, 0)
          else verticalParent pos (allRows !! p) (v !! p)

-- | 把高度表达式按空列切成若干竖直段。
toVertical :: Expr -> [Expr]
toVertical (Expr mcols) = build 0 1
 where
  n = length mcols
  build prev i
    | i == n = [Expr (drop prev mcols)]
    | null (mcols !! i) = Expr (take (i - prev) (drop prev mcols)) : build i (i + 1)
    | otherwise = build prev (i + 1)

columnSub :: Column -> Column -> Column
columnSub a b = go 0
 where
  va = columnVerticals a
  vb = columnVerticals b
  go j
    | j >= length a = []
    | otherwise =
      let pos = if j /= 0 then va !! (j - 1) else []
          jr = indexAfter vb pos
          delta = eValue (a !! j)
                  - (if jr < length vb then eValue (b !! jr) else 0)
      in if delta <= 0
           then []
           else Entry delta (eHeight (a !! j)) : go (j + 1)

-- | TBM 版提升阈值（直接扫描）。
ascensionThreshold :: [[[Expr]]] -> [[(Int, Int)]] -> Int -> [Expr] -> [[Expr]]
ascensionThreshold v p r b = result
 where
  ncols = length v
  result = [build i | i <- [0 .. ncols - 1]]
  build i
    | i < r = []
    | i == r = b
    | otherwise = go 0
   where
    go j
      | j >= length (v !! i) = last (v !! i)
      | otherwise =
        let pos = if j /= 0 then v !! i !! (j - 1) else []
            colP = fst (p !! i !! j)
        in if colP < r
             then pos
             else if verticalCmp pos (result !! colP) >= 0
               then pos
               else if verticalCmp (v !! i !! j) (result !! colP) >= 0
                 then result !! colP
                 else go (j + 1)

columnAdd :: Column -> Column -> Column
columnAdd a b = go a b
 where
  go [] bs = bs
  go as [] = as
  go as'@(ea : as) bs'@(eb : bs) =
    let c = exprCmp (eHeight ea) (eHeight eb)
        h = if c < 0 then eHeight ea else eHeight eb
    in Entry (eValue ea + eValue eb) h :
         (if c < 0
            then go as bs'
            else if c > 0
              then go as' bs
              else go as bs)

columnTruncate :: Column -> [Expr] -> Column
columnTruncate col b = go col b
 where
  go [] _ = []
  go _ [] = []
  go (ea : as) (vh : vs) =
    let c = exprCmp (eHeight ea) vh
        h = if c < 0 then eHeight ea else vh
    in Entry (eValue ea) h :
         (if c < 0
            then go as (vh : vs)
            else if c > 0
              then go (ea : as) vs
              else go as vs)

columnMul :: Column -> Int -> Column
columnMul col w = map (\e -> e { eValue = eValue e * w }) col

copyColumn :: Column -> Column -> [Expr] -> Int -> Column
copyColumn colI offset ai w =
  columnAdd colI (columnMul (columnTruncate offset ai) w)

-- ---------------------------------------------------------------------------
-- 展开
-- ---------------------------------------------------------------------------


isLimit :: Expr -> Bool
isLimit e
  | otherwise = case cols e of
    [] -> False
    cs -> not (null (last cs))

expandSuccessor :: Expr -> Int -> Expr
expandSuccessor m@(Expr mcols) index = Expr resultCols
 where
  v = map columnVerticals mcols
  p = parents m v
  n = length mcols - 1
  lastCol = mcols !! n
  r = fst (p !! n !! (length lastCol - 1))
  before = take n mcols
  b = if length lastCol > 1
        then v !! n !! (length lastCol - 2)
        else []
  offset = columnSub lastCol (mcols !! r)
  a = ascensionThreshold v p r b
  added =
    [ copyColumn (mcols !! i) offset (a !! i) w
    | w <- [1 .. index]
    , i <- [r .. n - 1] ]
  resultCols = before ++ added

expandLimit :: Expr -> Int -> Expr
expandLimit m@(Expr mcols) index = Expr (take right mcols ++ [newEntries])
 where
  right = length mcols - 1
  col = mcols !! right
  lastIdx = length col - 1
  v = eValue (col !! lastIdx)
  newH = tbmFs (eHeight (col !! lastIdx)) index
  segs = toVertical newH
  newEntries = take lastIdx col ++ map (\seg -> Entry v seg) segs

expand :: Expr -> Int -> Expr
expand m@(Expr mcols) index = case mcols of
  [] -> m
  _ ->
    let n = length mcols - 1
        lastCol = mcols !! n
    in if null lastCol
         then Expr (take n mcols)
         else
           let lastHeight = eHeight (lastCol !! (length lastCol - 1))
           in if isOne lastHeight
                then expandSuccessor m index
                else expandLimit m index

tbmFs :: Expr -> Int -> Expr
tbmFs m index
  | not (isLimit m) = expand m 0
  | otherwise = expand m index

-- ---------------------------------------------------------------------------
-- 显示
-- ---------------------------------------------------------------------------

display :: Expr -> String
display e = concatMap columnDisplay (cols e)
 where
  columnDisplay c = "(" ++ intercalateComma (map entryDisplay c) ++ ")"

intercalateComma :: [String] -> String
intercalateComma [] = ""
intercalateComma [x] = x
intercalateComma (x : xs) = x ++ "," ++ intercalateComma xs

entryDisplay :: Entry -> String
entryDisplay e =
  case heightDisplay (eHeight e) of
    Nothing -> show (eValue e)
    Just sd -> show (eValue e) ++ "^" ++ sd

-- | 返回 Just 上标文本；Nothing 表示高度为 1（无上标）。
heightDisplay :: Expr -> Maybe String
heightDisplay s
  | length (cols s) == 1 = Nothing
  | exprCmp s makeOmega == 0 = Just "\969"
  | otherwise = Just (display s)

-- ---------------------------------------------------------------------------
-- 解析（失败返回 Nothing；expandText 统一坍缩为 kParseError）
-- ---------------------------------------------------------------------------

type P a = String -> Maybe (a, String)

pSp :: P ()
pSp s = Just ((), dropWhile (== ' ') s)

pDigits :: P String
pDigits s =
  let (ds, rest) = span isDigit s
  in if null ds then Nothing else Just (ds, rest)

pNumber :: P Int
pNumber s0 = do
  ((), s1) <- pSp s0
  (ds, s2) <- pDigits s1
  Just (foldl (\a d -> a * 10 + (fromEnum d - 48)) 0 ds, s2)

pChar :: Char -> P ()
pChar c (x : xs) | x == c = Just ((), xs)
pChar _ _ = Nothing

pExpr :: P Expr
pExpr s0 = do
  ((), s1) <- pSp s0
  go s1
 where
  go s = do
    ((), s') <- pSp s
    case s' of
      ('(' : _) -> do
        (col, s'') <- pColumn s'
        ((), s''') <- pSp s''
        (Expr cols, rest) <- go s'''
        Just (Expr (col : cols), rest)
      _ -> Just (Expr [], s)

pColumn :: P Column
pColumn s0 = do
  ((), s1) <- pSp s0
  ((), s2) <- pChar '(' s1
  ((), s3) <- pSp s2
  case s3 of
    (')' : s4) -> Just ([], s4)
    _ -> do
      (e, s4) <- pEntry s3
      ((), s5) <- pSp s4
      (entries, s6) <- entriesAfter s5
      ((), s7) <- pSp s6
      ((), s8) <- pChar ')' s7
      Just (e : entries, s8)
 where
  entriesAfter s = do
    ((), s') <- pSp s
    case s' of
      (',' : rest) -> do
        ((), s'') <- pSp rest
        case s'' of
          (')' : _) -> Just ([], s'')
          _ -> do
            (e, s3) <- pEntry s''
            ((), s4) <- pSp s3
            (es, s5) <- entriesAfter s4
            Just (e : es, s5)
      _ -> Just ([], s)

pMatchOmega :: P ()
pMatchOmega ('\969' : s) = Just ((), s)
pMatchOmega ('w' : s) = Just ((), s)
pMatchOmega _ = Nothing

pHeight :: P Expr
pHeight s0 = do
  ((), s1) <- pSp s0
  case pMatchOmega s1 of
    Just (_, s2) -> Just (makeOmega, s2)
    Nothing -> pExpr s1

pEntry :: P Entry
pEntry s0 = do
  (v, s1) <- pNumber s0
  ((), s2) <- pSp s1
  case s2 of
    ('^' : rest) -> do
      (h, s3) <- pHeight rest
      Just (Entry v h, s3)
    _ -> Just (Entry v oneExpr, s2)

pParse :: P Expr
pParse s0 = do
  ((), s1) <- pSp s0
  do
      (e, s2) <- pExpr s1
      ((), s3) <- pSp s2
      if null s3
        then Just (e, s3)
        else Nothing

parseExprText :: String -> Maybe Expr
parseExprText s = case pParse s of
  Just (e, "") -> Just e
  _ -> Nothing

-- | 对外：解析 + FS + 显示；任何失败返回统一错误串。
expandText :: String -> Int -> String
expandText text term =
  case pParse text of
    Just (e, "") -> display (tbmFs e term)
    _ -> kParseError
