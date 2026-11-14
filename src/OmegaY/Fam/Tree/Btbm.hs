-- | BTBM / BTBM-weak / BBM 分层树形矩阵记号。忠实移植
--   @Header/notation/btbm.hpp@、@btbw.hpp@、@bbm.hpp@。
--
--   内部 value 为 0-based，显示 +1；空列显示 (0)。末元素 height 即下一层。
module OmegaY.Fam.Tree.Btbm
  ( btbmExpandText
  , btbwExpandText
  , bbmExpandText
  ) where

import Data.Char (isDigit)

-- ---------------------------------------------------------------------------
-- 数据
-- ---------------------------------------------------------------------------

data Expr = Expr
  { eInf :: Bool
  , eCols :: [Column]
  } deriving (Eq)

type Column = [Entry]

data Entry = Entry
  { enValue :: Int
  , enHeight :: Expr
  } deriving (Eq)

data HExpr = HExpr { hCols :: [HColumn] } deriving (Eq)
type HColumn = [HEntry]
data HEntry = HEntry
  { heMark :: Bool
  , heValue :: Int
  , heHeight :: HExpr
  } deriving (Eq)

type Vertical = [HExpr]

emptyHeight :: Expr
emptyHeight = Expr False []

makeInf :: Expr
makeInf = Expr True []

isInfinity :: Expr -> Bool
isInfinity = eInf

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

booleanCmp :: Bool -> Bool -> Int
booleanCmp a b = (if a then 1 else 0) - (if b then 1 else 0)

-- ---------------------------------------------------------------------------
-- 极限基本列
-- ---------------------------------------------------------------------------

infinityFs :: Int -> Expr
infinityFs index
  | index == 0 = Expr False [[]]
  | otherwise = Expr False [[], tower]
 where
  tower = loop (index - 1) [Entry (index - 1) emptyHeight]
  loop i col
    | i <= 0 = col
    | otherwise =
      let next = [Entry (i - 1) (Expr False [col])]
      in loop (i - 1) next

-- ---------------------------------------------------------------------------
-- to_height / from_height
-- ---------------------------------------------------------------------------

toHeight :: Expr -> Int -> HExpr
toHeight (Expr inf cols) r = HExpr (map hcol cols)
 where
  hcol col = map hentry col
  hentry en =
    let p = enValue en
        mark = p >= r
    in HEntry mark (if mark then p - r else p) (toHeight (enHeight en) r)

fromHeight :: HExpr -> Int -> Expr
fromHeight (HExpr cols) r = Expr False (map fcol cols)
 where
  fcol col = map fentry col
  fentry he =
    Entry (heValue he + (if heMark he then r else 0)) (fromHeight (heHeight he) r)

heightEntryCmp :: HEntry -> HEntry -> Int
heightEntryCmp a b =
  case booleanCmp (heMark a) (heMark b) of
    0 -> case cmpInt (heValue a) (heValue b) of
      0 -> heightCmp (heHeight a) (heHeight b)
      c -> c
    c -> c

heightCmp :: HExpr -> HExpr -> Int
heightCmp a b = goCols 0
 where
  ca = hCols a
  cb = hCols b
  n = min (length ca) (length cb)
  goCols i
    | i >= n = cmpInt (length ca) (length cb)
    | otherwise = goEntries i (ca !! i) (cb !! i) 0
  goEntries i ra rb j
    | j >= min (length ra) (length rb) =
      case cmpInt (length ra) (length rb) of
        0 -> goCols (i + 1)
        c -> c
    | otherwise = case heightEntryCmp (ra !! j) (rb !! j) of
      0 -> goEntries i ra rb (j + 1)
      c -> c

verticalCmp :: Vertical -> Vertical -> Int
verticalCmp a b = go 0
 where
  n = min (length a) (length b)
  go i
    | i >= n = cmpInt (length a) (length b)
    | otherwise = case heightCmp (a !! i) (b !! i) of
      0 -> go (i + 1)
      c -> c

-- ---------------------------------------------------------------------------
-- 层导航
-- ---------------------------------------------------------------------------

hasNextLayer :: Expr -> Bool
hasNextLayer e = case eCols e of
  [] -> False
  cs -> case last cs of
    [] -> False
    _ -> True

nextLayer :: Expr -> Expr
nextLayer e = enHeight (last (last (eCols e)))

skipLayers :: Expr -> Int -> Expr
skipLayers e l = foldl (\cur _ -> nextLayer cur) e [1 .. l]

tailLayer :: Expr -> Int
tailLayer e
  | not (hasNextLayer e) = -1
  | otherwise = 1 + tailLayer (nextLayer e)

tailCol :: Expr -> Int -> Int
tailCol eIn tLayer = go eIn 0 0
 where
  go cur left i
    | i >= tLayer = left + length (eCols cur) - 1
    | otherwise = go (nextLayer cur) (left + length (eCols cur)) (i + 1)

root :: Expr -> Int -> Int
root eIn tLayer = enValue (last (last (eCols (skipLayers eIn tLayer))))

rootLayer :: Expr -> Int -> (Int, Int)
rootLayer eIn r = go eIn 0 0
 where
  go cur left layer
    | r < left + length (eCols cur) = (layer, r - left)
    | otherwise = go (nextLayer cur) (left + length (eCols cur)) (layer + 1)

-- ---------------------------------------------------------------------------
-- 整体替换提升
-- ---------------------------------------------------------------------------

ascendReplace :: Expr -> Int -> Int -> Maybe Int -> Expr -> Expr
ascendReplace (Expr _ cols) r diff tl newTail =
  Expr False (concatMap buildCol [0 .. length cols - 1])
 where
  lastCol = length cols - 1
  buildCol i =
    if tl == Just 0 && i == lastCol
      then eCols newTail
      else [map (buildEntry i) (zip [0 ..] (cols !! i))]
  buildEntry i (j, en) =
    let col = cols !! i
        isLayerTail = tl /= Nothing && i == lastCol && j == length col - 1
        newTl = if isLayerTail then fmap (subtract 1) tl else Nothing
        v = enValue en
        nv = if v >= r then v + diff else v
    in Entry nv (ascendReplace (enHeight en) r diff newTl newTail)

-- ---------------------------------------------------------------------------
-- special（BTBM 版）
-- ---------------------------------------------------------------------------

isSpecial :: Expr -> Int -> Bool
isSpecial eIn tLayer
  | tLayer == 0 = False
  | otherwise =
    let (cur, left) = walk eIn 0 0
    in case eCols cur of
      [] -> False
      cs -> case last cs of
        [en] -> enHeight en == emptyHeight && enValue en == left - 1
        _ -> False
 where
  walk cur left i
    | i >= tLayer = (cur, left)
    | otherwise = walk (nextLayer cur) (left + length (eCols cur)) (i + 1)

expandSpecial :: Expr -> Int -> Int -> Expr
expandSpecial (Expr _ cols) tLayer index = Expr False (init cols ++ [resultCol])
 where
  col = last cols
  prefix = take (length col - 1) col
  entry = last col
  added
    | tLayer > 1 =
      [Entry (enValue entry) (expandSpecial (enHeight entry) (tLayer - 1) index)]
    | otherwise =
      let h = Expr False (init (eCols (enHeight entry)))
          base = Entry (enValue entry) h
      in (if null prefix then [base] else []) ++ replicate index base
  resultCol = prefix ++ added

-- ---------------------------------------------------------------------------
-- 追加起点
-- ---------------------------------------------------------------------------

rootAppendingStart :: Column -> Int -> Column -> Int -> Int
rootAppendingStart colRoot r colTail t = go 0 0
 where
  heightsRoot = map (\en -> toHeight (enHeight en) r) colRoot
  heightsTail =
    [ toHeight (enHeight (colTail !! k)) t | k <- [0 .. length colTail - 2] ]
  go ir it
    | ir == length heightsRoot || it == length heightsTail = ir
    | otherwise =
      let c = heightCmp (heightsRoot !! ir) (heightsTail !! it)
      in go (if c <= 0 then ir + 1 else ir) (if c >= 0 then it + 1 else it)

-- ---------------------------------------------------------------------------
-- 竖直序列（weak 用）
-- ---------------------------------------------------------------------------

verticalIncrease :: Vertical -> HExpr -> Vertical
verticalIncrease vert h =
  reverse (dropWhile (\x -> heightCmp x h < 0) (reverse vert)) ++ [h]

verticalDiff :: Vertical -> Vertical -> Vertical
verticalDiff v1 v2 = drop i v1
 where
  n = min (length v1) (length v2)
  i = length (takeWhile (\k -> heightCmp (v1 !! k) (v2 !! k) == 0) [0 .. n - 1])

-- ---------------------------------------------------------------------------
-- plain 比较
-- ---------------------------------------------------------------------------

entryPlainCmp :: Entry -> Entry -> Int
entryPlainCmp a b =
  case cmpInt (enValue a) (enValue b) of
    0 -> plainCmp (enHeight a) (enHeight b)
    c -> c

plainCmp :: Expr -> Expr -> Int
plainCmp a b
  | eInf a || eInf b = booleanCmp (eInf a) (eInf b)
  | otherwise = goCols 0
 where
  ca = eCols a
  cb = eCols b
  n = min (length ca) (length cb)
  goCols i
    | i >= n = cmpInt (length ca) (length cb)
    | otherwise = goEntries i (ca !! i) (cb !! i) 0
  goEntries i ra rb j
    | j >= min (length ra) (length rb) =
      case cmpInt (length ra) (length rb) of
        0 -> goCols (i + 1)
        c -> c
    | otherwise = case entryPlainCmp (ra !! j) (rb !! j) of
      0 -> goEntries i ra rb (j + 1)
      c -> c

-- ---------------------------------------------------------------------------
-- 核心 FS（参数化 special）
-- ---------------------------------------------------------------------------

isLimit :: Expr -> Bool
isLimit e =
  isInfinity e || (not (null (eCols e)) && not (null (last (eCols e))))

fsCoreWith :: (Expr -> Int -> Bool) -> (Expr -> Int -> Int -> Expr) -> Expr -> Int -> Expr
fsCoreWith isSpecialF expandSpecialF e index
  | isInfinity e = infinityFs index
  | null (eCols e) = e
  | otherwise =
    let tLayer = tailLayer e
    in if tLayer < 0
         then Expr False (init (eCols e))
         else if isSpecialF e tLayer
           then expandSpecialF e tLayer index
           else standardCase e tLayer index

standardCase :: Expr -> Int -> Int -> Expr
standardCase e tLayer index =
  ascendReplace e 0 0 (Just tLayer) newTail
 where
  t = tailCol e tLayer
  r = root e tLayer
  (rLayer, ri) = rootLayer e r
  exprRoot = skipLayers e rLayer
  colRoot = eCols exprRoot !! ri
  exprTail = skipLayers exprRoot (tLayer - rLayer)
  colTail = last (eCols exprTail)
  appending = rootAppendingStart colRoot r colTail t

  newTail = buildTail newTail0 index
  newTail0 = Expr False []

  buildTail nt j
    | j < 1 = nt
    | otherwise =
      let nt' =
            if ri /= length (eCols exprRoot) - 1
              then
                let tail1 =
                      Expr False (drop (ri + 1) (eCols exprRoot))
                    replaced1 =
                      ascendReplace tail1 r (j * (t - r))
                        (Just (tLayer - rLayer)) nt
                    tailPrefix = take (length colTail - 1) colTail
                    prefixExpr = Expr False [tailPrefix]
                    newCol0 =
                      ascendReplace prefixExpr r ((j - 1) * (t - r)) Nothing
                        (Expr False [])
                    newCol =
                      head (eCols newCol0)
                      ++ [ Entry (enValue (colRoot !! k))
                           (ascendReplace (enHeight (colRoot !! k)) r
                              (j * (t - r)) Nothing (Expr False []))
                         | k <- [appending .. length colRoot - 1] ]
                    combined = Expr False [newCol]
                    combined' = combined { eCols = eCols combined ++ eCols replaced1 }
                in combined'
              else
                let tailPrefix = take (length colTail - 1) colTail
                    prefixExpr = Expr False [tailPrefix]
                    newCol0 =
                      ascendReplace prefixExpr r ((j - 1) * (t - r)) Nothing
                        (Expr False [])
                    newCol =
                      head (eCols newCol0)
                      ++ [ Entry (enValue (colRoot !! k))
                           (ascendReplace (enHeight (colRoot !! k)) r
                              (j * (t - r))
                              (if k == length colRoot - 1
                                 then Just (tLayer - rLayer - 1)
                                 else Nothing)
                              nt)
                         | k <- [appending .. length colRoot - 1] ]
                in Expr False [newCol]
      in buildTail nt' (j - 1)

fs :: (Expr -> Int -> Bool) -> (Expr -> Int -> Int -> Expr) -> Expr -> Int -> Expr
fs isSF exSF e index
  | isInfinity e = infinityFs index
  | not (isLimit e) = fsCoreWith isSF exSF e 0
  | otherwise = fsCoreWith isSF exSF e index

-- ---------------------------------------------------------------------------
-- weak：w_ascend_replace + w_fs
-- ---------------------------------------------------------------------------

wAscendReplace :: Expr -> Int -> Int -> Int -> Maybe Int -> Expr -> Vertical -> Expr
wAscendReplace (Expr _ cols) startIndex r diff tl newTail critical =
  Expr False (concatMap buildCol [0 .. length cols - 1])
 where
  lastCol = length cols - 1
  buildCol i =
    if tl == Just 0 && i == lastCol
      then eCols newTail
      else
        let col = cols !! i
            colIndex = startIndex + i
            (resultCol, _, _) =
              foldl (step i colIndex) ([], False, False) (zip [0 ..] col)
        in [resultCol]
  step i colIndex (resCol, aboveCrit, ignoreCheck) (j, en) =
    let col = cols !! i
        isLayerTail = tl /= Nothing && i == lastCol && j == length col - 1
    in if not aboveCrit
         then
           let height = toHeight (enHeight en) colIndex
               newVert = verticalIncrease [] height
               cmp = verticalCmp newVert critical
               plainV = if enValue en >= r then enValue en + diff else enValue en
           in if cmp > 0
                then
                  let critRemaining = verticalDiff critical []
                      extra =
                        [ Entry plainV (fromHeight h (startIndex + diff))
                        | h <- critRemaining ]
                      resCol1 = resCol ++ extra
                  in tailBlock i colIndex resCol1 True ignoreCheck isLayerTail en
                else
                  let resCol1 =
                        resCol ++ [Entry plainV (fromHeight height (startIndex + diff))]
                      aboveCrit' = cmp == 0
                  in if not isLayerTail
                       then (resCol1, aboveCrit', ignoreCheck)
                       else tailBlock i colIndex resCol1 aboveCrit' ignoreCheck isLayerTail en
         else tailBlock i colIndex resCol aboveCrit ignoreCheck isLayerTail en
  tailBlock i colIndex resCol aboveCrit ignoreCheck isLayerTail en =
    let newTl = if isLayerTail then fmap (subtract 1) tl else Nothing
        newEntryV = if enValue en >= r then enValue en + diff else enValue en
        newEntry =
          Entry newEntryV
            (wAscendReplace (enHeight en) (colIndex + 1) r diff newTl newTail
               critical)
        popped =
          if ignoreCheck
            then resCol
            else popLoop resCol newEntry
    in (popped ++ [newEntry], aboveCrit, True)
   where
    popLoop rc ne =
      if not (null rc)
         && enValue (last rc) == enValue ne
         && plainCmp (enHeight (last rc)) (enHeight ne) < 0
        then popLoop (init rc) ne
        else rc

wFs :: Expr -> Int -> Expr
wFs e index
  | isInfinity e = infinityFs index
  | null (eCols e) = e
  | otherwise =
    let tLayer = tailLayer e
    in if tLayer < 0
         then Expr False (init (eCols e))
         else if isSpecial e tLayer
           then expandSpecial e tLayer index
           else weakCase e tLayer index

weakCase :: Expr -> Int -> Int -> Expr
weakCase e tLayer index =
  wAscendReplace e 0 0 0 (Just tLayer) newTail []
 where
  t = tailCol e tLayer
  r = root e tLayer
  (rLayer, ri) = rootLayer e r
  exprRoot = skipLayers e rLayer
  colRoot = eCols exprRoot !! ri
  exprTail = skipLayers exprRoot (tLayer - rLayer)
  colTail = last (eCols exprTail)
  appending = rootAppendingStart colRoot r colTail t
  criticalHeights =
    [ toHeight (enHeight (colTail !! k)) t | k <- [0 .. length colTail - 2] ]
  criticalVert = foldl verticalIncrease [] criticalHeights

  newTail = buildTail newTail0 index
  newTail0 = Expr False []

  buildTail nt j
    | j < 1 = nt
    | otherwise =
      let nt' =
            if ri /= length (eCols exprRoot) - 1
              then
                let tail1 = Expr False (drop (ri + 1) (eCols exprRoot))
                    replaced1 =
                      wAscendReplace tail1 (r + 1) r (j * (t - r))
                        (Just (tLayer - rLayer)) nt criticalVert
                    newCol0 =
                      [ Entry (enValue (colTail !! k) + (j - 1) * (t - r))
                        (fromHeight (criticalHeights !! k) (r + j * (t - r)))
                      | k <- [0 .. length colTail - 2] ]
                    newCol =
                      newCol0
                      ++ [ Entry (enValue (colRoot !! k))
                           (wAscendReplace (enHeight (colRoot !! k)) (r + 1) r
                              (j * (t - r)) Nothing (Expr False []) criticalVert)
                         | k <- [appending .. length colRoot - 1] ]
                    combined = Expr False [newCol]
                    combined' = combined { eCols = eCols combined ++ eCols replaced1 }
                in combined'
              else
                let newCol0 =
                      [ Entry (enValue (colTail !! k) + (j - 1) * (t - r))
                        (fromHeight (criticalHeights !! k) (r + j * (t - r)))
                      | k <- [0 .. length colTail - 2] ]
                    newCol =
                      newCol0
                      ++ [ Entry (enValue (colRoot !! k))
                           (wAscendReplace (enHeight (colRoot !! k)) (r + 1) r
                              (j * (t - r))
                              (if k == length colRoot - 1
                                 then Just (tLayer - rLayer - 1)
                                 else Nothing)
                              nt criticalVert)
                         | k <- [appending .. length colRoot - 1] ]
                in Expr False [newCol]
      in buildTail nt' (j - 1)

-- ---------------------------------------------------------------------------
-- BBM special
-- ---------------------------------------------------------------------------

bIsSpecial :: Expr -> Int -> Bool
bIsSpecial e tLayer
  | tLayer < 0 = False
  | otherwise =
    let current = skipLayers e tLayer
    in if not (hasNextLayer current)
         then False
         else
           let nxt = nextLayer current
           in not (null (eCols nxt)) && null (last (eCols nxt))

bExpandSpecial :: Expr -> Int -> Int -> Expr
bExpandSpecial (Expr _ cols) tLayer index = Expr False (init cols ++ [resultCol])
 where
  col = last cols
  prefix = take (length col - 1) col
  entry = last col
  added
    | tLayer > 0 =
      [Entry (enValue entry) (bExpandSpecial (enHeight entry) (tLayer - 1) index)]
    | otherwise =
      let h = Expr False (init (eCols (enHeight entry)))
          base = Entry (enValue entry) h
      in (if null prefix then [base] else []) ++ replicate index base
  resultCol = prefix ++ added

-- ---------------------------------------------------------------------------
-- 显示
-- ---------------------------------------------------------------------------

display :: Expr -> String
display e
  | eInf e = "Limit"
  | otherwise = concatMap displayColumn (eCols e)
 where
  displayColumn c
    | null c = "(0)"
    | otherwise = "(" ++ intercalateComma (map displayEntry c) ++ ")"

displayEntry :: Entry -> String
displayEntry en =
  let v = show (enValue en + 1)
  in if null (eCols (enHeight en))
       then v
       else v ++ "^" ++ display (enHeight en)

intercalateComma :: [String] -> String
intercalateComma [] = ""
intercalateComma [x] = x
intercalateComma (x : xs) = x ++ "," ++ intercalateComma xs

-- ---------------------------------------------------------------------------
-- 解析
-- ---------------------------------------------------------------------------

type P a = String -> Maybe (a, String)

pSp :: P ()
pSp s = Just ((), dropWhile (== ' ') s)

pChar :: Char -> P ()
pChar c (x : xs) | x == c = Just ((), xs)
pChar _ _ = Nothing

pDigits :: P String
pDigits s =
  let (ds, rest) = span isDigit s
  in if null ds then Nothing else Just (ds, rest)

pNumber :: P Int
pNumber s0 = do
  ((), s1) <- pSp s0
  (ds, s2) <- pDigits s1
  Just (foldl (\a d -> a * 10 + (fromEnum d - 48)) 0 ds, s2)

pSkipIndex :: P ()
pSkipIndex (':' : rest) = do
  ((), s1) <- pSp rest
  let (_, s2) = span isDigit s1
  pSp s2
pSkipIndex s = Just ((), s)

pColumn :: P Column
pColumn s0 = do
  ((), s1) <- pSp s0
  ((), s2) <- pChar '(' s1
  ((), s3) <- pSp s2
  (entries, s4) <-
    case s3 of
      (')' : _) -> Just ([], s3)
      (':' : _) -> Just ([], s3)
      _ -> do
        (e, s4) <- pEntry s3
        ((), s5) <- pSp s4
        (es, s6) <- entriesAfter s5
        Just (e : es, s6)
  ((), s7) <- pSp s4
  ((), s8) <- pSkipIndex s7
  ((), s9) <- pSp s8
  ((), s10) <- pChar ')' s9
  Just (dropWhileEndVal entries, s10)
 where
  dropWhileEndVal es = reverse (dropWhile (\en -> enValue en == (-1)) (reverse es))
  entriesAfter s = do
    ((), s') <- pSp s
    case s' of
      (',' : rest) -> do
        ((), s'') <- pSp rest
        case s'' of
          (')' : _) -> Just ([], s'')
          (':' : _) -> Just ([], s'')
          _ -> do
            (e, s3) <- pEntry s''
            ((), s4) <- pSp s3
            (es, s5) <- entriesAfter s4
            Just (e : es, s5)
      _ -> Just ([], s)

pEntry :: P Entry
pEntry s0 = do
  (v, s1) <- pNumber s0
  ((), s2) <- pSp s1
  case s2 of
    ('^' : rest) -> do
      (h, s3) <- pExpr rest
      Just (Entry (v - 1) h, s3)
    _ -> Just (Entry (v - 1) emptyHeight, s2)

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
        (Expr _ cols, rest) <- go s'''
        Just (Expr False (col : cols), rest)
      _ -> Just (Expr False [], s)

pParse :: P Expr
pParse s0 = do
  ((), s1) <- pSp s0
  case s1 of
    ('L' : 'i' : 'm' : 'i' : 't' : s2) -> do
      ((), s3) <- pSp s2
      if null s3
        then Just (makeInf, s3)
        else Nothing
    _ -> do
      (e, s2) <- pExpr s1
      ((), s3) <- pSp s2
      if null s3
        then Just (e, s3)
        else Nothing

-- ---------------------------------------------------------------------------
-- 对外入口
-- ---------------------------------------------------------------------------

runNotation :: (Expr -> Int -> Expr) -> String -> Int -> String
runNotation fsFn text term =
  case pParse text of
    Just (e, "") -> display (fsFn e term)
    _ -> kParseError

btbmExpandText :: String -> Int -> String
btbmExpandText = runNotation (fs isSpecial expandSpecial)

btbwExpandText :: String -> Int -> String
btbwExpandText = runNotation wFs

bbmExpandText :: String -> Int -> String
bbmExpandText = runNotation (fs bIsSpecial bExpandSpecial)
