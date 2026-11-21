-- | bt3ynss（weak BT*）：vertical/height + critical_vert + lnz_m1 alter。
module OmegaY.Fam.M1ynss.Bt3
  ( expandBt3
  , expandBt3Alter
  ) where

import Control.Exception (try, SomeException, evaluate)
import System.IO.Unsafe (unsafePerformIO)
import OmegaY.Fam.M1ynss.PP
import OmegaY.Fam.M1ynss.Bt (BtNode(..), BtColumn(..), btMakeNode)

-- ---------------------------------------------------------------------------
-- 显示
-- ---------------------------------------------------------------------------

bmColumnDisplay :: [Int] -> String
bmColumnDisplay lower =
  let nPos = lastPositive lower
   in if nPos < 0
        then "(0)"
        else "(" ++ commas (take (nPos + 1) lower) ++ ")"
  where
    lastPositive xs = go (length xs - 1)
      where
        go i
          | i < 0 = -1
          | xs !! i > 0 = i
          | otherwise = go (i - 1)
    commas xs = foldr (\x a -> show x ++ (if null a then "" else "," ++ a)) "" xs

topDisplay :: BtNode -> String
topDisplay e
  | null (btCols e) = "*"
  | otherwise = "*^" ++ display e

columnDisplay :: BtColumn -> String
columnDisplay c
  | null (bHigher c) = bmColumnDisplay (bArr c)
  | otherwise =
      "("
        ++ commas (map show (bArr c) ++ map topDisplay (bHigher c))
        ++ ")"
  where
    commas xs = foldr (\x a -> x ++ (if null a then "" else "," ++ a)) "" xs

display :: BtNode -> String
display e
  | otherwise = concatMap columnDisplay (btCols e)

-- ---------------------------------------------------------------------------
-- 比较
-- ---------------------------------------------------------------------------

numCmp :: Int -> Int -> Int
numCmp a b = if a == b then 0 else if a < b then (-1) else 1

boolCmp :: Bool -> Bool -> Int
boolCmp a b = (if a then 1 else 0) - (if b then 1 else 0)

compareNode :: BtNode -> BtNode -> Int
compareNode a b = go 0
  where
    m = min (length (btCols a)) (length (btCols b))
    go i
      | i >= m = numCmp (length (btCols a)) (length (btCols b))
      | otherwise =
          let c = colCmp (btCols a !! i) (btCols b !! i)
           in if c /= 0 then c else go (i + 1)

higherCmp :: [BtNode] -> [BtNode] -> Int
higherCmp a b = go 0
  where
    m = min (length a) (length b)
    go i
      | i >= m = numCmp (length a) (length b)
      | otherwise =
          let c = compareNode (a !! i) (b !! i)
           in if c /= 0 then c else go (i + 1)

colCmp :: BtColumn -> BtColumn -> Int
colCmp a b = go 0
  where
    m = min (length (bArr a)) (length (bArr b))
    go i
      | i >= m =
          let c = numCmp (length (bArr a)) (length (bArr b))
           in if c /= 0 then c else higherCmp (bHigher a) (bHigher b)
      | otherwise =
          let c = numCmp (bArr a !! i) (bArr b !! i)
           in if c /= 0 then c else go (i + 1)

-- ---------------------------------------------------------------------------
-- 父树
-- ---------------------------------------------------------------------------

data PCol = PCol [Int] [PNode]
newtype PNode = PNode [PCol]

lowerPass :: [BtColumn] -> [[Int]] -> Int -> [Int]
lowerPass extCols pStack iS = goArr (replicate (n + 1) (-1)) 0
  where
    n = length (bArr (last extCols))
    goArr arr j
      | j >= n = arr
      | otherwise =
          let v = bArr (last extCols) !! j
              p = walk arr iS j v
              arr' = if p < 0 then arr else repIdx j p arr
           in goArr arr' (j + 1)
    walk arr p jj vv
      | p < 0 = p
      | bArr (extCols !! p) !! jj < vv = p
      | otherwise =
          let pNext =
                if jj == 0
                  then p - 1
                  else
                    if p == iS
                      then arr !! (jj - 1)
                      else (pStack !! p) !! (jj - 1)
           in walk arr pNext jj vv

computeParents :: BtNode -> Int -> PNode
computeParents e n = computeTopParents e lowerTree [] [] []
  where
    -- 第一段：逐行求父列（entry[0..n-1]）。
    lowerTree = lowerPassTree e [] []
    lowerPassTree cur cStack pStack =
      PNode (reverse built)
      where
        (_, _, built) = foldl step (cStack, pStack, []) (btCols cur)
        step (cs, ps, builtAcc) col =
          let extCols = cs ++ [col]
              parr = lowerPass extCols ps (length cs)
              extParrs = ps ++ [parr]
              subs = [lowerPassTree (bHigher col !! ix) extCols extParrs | ix <- [0 .. length (bHigher col) - 1]]
           in (extCols, extParrs, PCol parr subs : builtAcc)

    -- 第二段（原 bt3_compute_top_parents）：用竖直方向的比较求 entry[n]。
    -- ps：parr 栈；vs：各列的竖直高度栈；outer：外层列下标栈。
    computeTopParents cur (PNode pcols) ps0 vs0 outer = PNode (reverse built)
      where
        (built, _, _) = foldl step ([], ps0, vs0) (zip (btCols cur) pcols)

        step (acc, ps, vs) (col, PCol parr subs) =
          let iS = length ps
              ps' = ps ++ [parr]
              oneTree = thresholds (btMakeNode { btCols = [col] }) (PNode [PCol parr subs]) iS n (replicate iS 0)
              oneHigher = case oneTree of
                            ANode (ac : _) -> aHigherA ac
                            _ -> []
              vertical =
                [ toHeight (bArr col) (bHigher col !! j) (Just (oneHigher !! j)) n
                | j <- [0 .. length (bHigher col) - 1]
                ]
              vs' = vs ++ [vertical]
              p = walkTop ps' vs' vertical iS
              parr' = repIdx n p parr
              subs' =
                [ computeTopParents (bHigher col !! j) (subs !! j) ps' vs' (outer ++ [iS])
                | j <- [0 .. length (bHigher col) - 1]
                ]
           in (PCol parr' subs' : acc, ps', vs')

        walkTop ps vs vertical p
          | p < 0 = -1
          | p `notElem` outer && hHeightCmp (vs !! p) vertical < 0 = p
          | otherwise = walkTop ps vs vertical ((ps !! p) !! (n - 1))

    aHigherA (ACol _ h) = h

hHeightCmp :: [HNode] -> [HNode] -> Int
hHeightCmp a b = go 0
  where
    m = min (length a) (length b)
    go i
      | i >= m = numCmp (length a) (length b)
      | otherwise =
          let c = heightCmp (a !! i) (b !! i)
           in if c /= 0 then c else go (i + 1)

-- ---------------------------------------------------------------------------
-- 阈值
-- ---------------------------------------------------------------------------

data ACol = ACol Int [ANode]
newtype ANode = ANode [ACol]

emptyAt :: BtNode -> ANode
emptyAt e = ANode [ACol 0 (map emptyAt (bHigher col)) | col <- btCols e]

thresholds :: BtNode -> PNode -> Int -> Int -> [Int] -> ANode
thresholds e (PNode pcols) r b stk =
  ANode (snd (foldl step (stk, []) (zip [0 ..] (btCols e))))
  where
    step (stk', acc) (i, col) =
      let iS = length stk'
       in if iS < r && i + 1 /= length (btCols e)
            then (stk' ++ [0], acc ++ [ACol 0 (map emptyAt (bHigher col))])
            else
              let ai
                    | iS == r = b
                    | iS > r =
                        let parents = bpnParr (pcols !! i)
                         in walkAi parents stk' r
                    | otherwise = 0
                  subs =
                    [ thresholds (bHigher col !! ix) (bpnHigher (pcols !! i) !! ix) r b (stk' ++ [ai])
                    | ix <- [0 .. length (bHigher col) - 1]
                    ]
               in (stk' ++ [ai], acc ++ [ACol ai subs])
    bpnParr (PCol p _) = p
    bpnHigher (PCol _ h) = h
    walkAi parents stk' r =
      let go ai =
            if ai < length parents
              && parents !! ai >= r
              && parents !! ai < length stk'
              && stk' !! (parents !! ai) > ai
              then go (ai + 1)
              else ai
       in go 0

-- ---------------------------------------------------------------------------
-- vertical / height
-- ---------------------------------------------------------------------------

data HCol = HCol [(Int, Bool)] [HNode]
newtype HNode = HNode [HCol]

hcLow :: HCol -> [(Int, Bool)]
hcLow (HCol low _) = low

hcHigher :: HCol -> [HNode]
hcHigher (HCol _ h) = h

hcThreshold :: HCol -> Int
hcThreshold (HCol low _) = length (filter snd low)

toHeight :: [Int] -> BtNode -> Maybe ANode -> Int -> HNode
toHeight base cur at n =
  HNode [oneCol i col | (i, col) <- zip [0 ..] (btCols cur)]
  where
    oneCol i col =
      let aCol = case at of
            Just (ANode acols) | i < length acols -> acols !! i
            _ -> ACol 0 []
          aVal = aThreshold aCol
          low = [ if j < aVal then (bArr col !! j - base !! j, True) else (bArr col !! j, False) | j <- [0 .. n - 1] ]
          subAt = case at of
            Just (ANode acols) | i < length acols -> Just (aHigher (acols !! i))
            _ -> Nothing
          subs =
            [ case subAt of
                Just subsArr | ix < length subsArr -> toHeight base (bHigher col !! ix) (Just (subsArr !! ix)) n
                _ -> toHeight base (bHigher col !! ix) Nothing n
            | ix <- [0 .. length (bHigher col) - 1]
            ]
       in HCol low subs
    aThreshold (ACol t _) = t
    aHigher (ACol _ h) = h

computeVertical :: BtColumn -> Int -> [HNode]
computeVertical col n =
  let one = BtNode [col]
      pnode = computeParents one n
      stk = []
      atTree = thresholds one pnode 0 n stk
      (ANode [ac]) = atTree
   in [ toHeight (bArr col) (bHigher col !! j) (Just (aHigherA ac !! j)) n | j <- [0 .. length (bHigher col) - 1] ]
  where
    aHigherA (ACol _ h) = h

fromHeightProper :: [Int] -> HNode -> Int -> BtNode
fromHeightProper base (HNode hcols) n =
  BtNode [ oneCol hc | hc <- hcols ]
  where
    oneCol hc =
      let th = hcThreshold hc
          arr = [ if j < th then fst (hcLow hc !! j) + base !! j else fst (hcLow hc !! j) | j <- [0 .. n - 1] ]
          subs = [ fromHeightProper arr x n | x <- hcHigher hc ]
       in BtColumn arr subs
    hcThreshold (HCol low _) = length (filter snd low)
    hcLow (HCol low _) = low
    hcHigher (HCol _ h) = h

-- height compare
heightCmp :: HNode -> HNode -> Int
heightCmp a b = go 0
  where
    m = min (length (unH a)) (length (unH b))
    unH (HNode cs) = cs
    go i
      | i >= m = numCmp (length (unH a)) (length (unH b))
      | otherwise =
          let c = hcolCmp (unH a !! i) (unH b !! i)
           in if c /= 0 then c else go (i + 1)
    hcolCmp x y = goL 0
      where
        mx = min (length (hcLow x)) (length (hcLow y))
        goL j
          | j >= mx =
              let c = numCmp (length (hcLow x)) (length (hcLow y))
               in if c /= 0 then c else hheightCmp (hcHigher x) (hcHigher y)
          | otherwise =
              let (vx, bx) = hcLow x !! j
                  (vy, by) = hcLow y !! j
               in case numCmp vx vy of
                    0 -> case boolCmp bx by of
                      0 -> goL (j + 1)
                      c -> c
                    c -> c
    hheightCmp xs ys = go 0
      where
        m = min (length xs) (length ys)
        go i
          | i >= m = numCmp (length xs) (length ys)
          | otherwise =
              let c = heightCmp (xs !! i) (ys !! i)
               in if c /= 0 then c else go (i + 1)

-- ---------------------------------------------------------------------------
-- 层 / 根
-- ---------------------------------------------------------------------------

nextLayer :: BtNode -> BtNode
nextLayer cur =
  let right = length (btCols cur) - 1
      col = btCols cur !! right
      hr = length (bHigher col) - 1
   in bHigher col !! hr

skipLayer :: BtNode -> Int -> BtNode
skipLayer e k = iterate nextLayer e !! k

nextLayerA :: ANode -> ANode
nextLayerA (ANode cols) =
  let right = length cols - 1
      ac = cols !! right
      hr = length (aHigherA ac) - 1
   in aHigherA ac !! hr
  where
    aHigherA (ACol _ h) = h

skipLayerA :: ANode -> Int -> ANode
skipLayerA a k = iterate nextLayerA a !! k

computeTail :: BtNode -> (Int, Int)
computeTail e
  | null (btCols e) || isZeroCol (last (btCols e)) = (-1, -1)
  | otherwise = go e 0 0
  where
    go cur layer len =
      let right = length (btCols cur) - 1
          col = btCols cur !! right
          hr = length (bHigher col) - 1
       in if hr == (-1) || null (btCols (bHigher col !! hr))
            then (len + right, layer)
            else go (bHigher col !! hr) (layer + 1) (len + length (btCols cur))

rootLayer :: BtNode -> Int -> (Int, Int)
rootLayer e r = go e 0 0
  where
    go cur layer len =
      if len + length (btCols cur) <= r
        then go (nextLayer cur) (layer + 1) (len + length (btCols cur))
        else (layer, r - len)

isZeroCol :: BtColumn -> Bool
isZeroCol c = all (== 0) (bArr c) && null (bHigher c)

lastNonneg :: [Int] -> Int
lastNonneg v = go (length v - 1)
  where
    go i
      | i < 0 = -1
      | v !! i >= 0 = i
      | otherwise = go (i - 1)

lastPos :: [Int] -> Int
lastPos v = go (length v - 1)
  where
    go i
      | i < 0 = -1
      | v !! i > 0 = i
      | otherwise = go (i - 1)

root :: BtNode -> PNode -> Int -> (Int, Int)
root e (PNode pcols) tLayer
  | null (btCols e) || isZeroCol (last (btCols e)) = (-1, -1)
  | otherwise =
      let PNode tailCols = skipLayerP (PNode pcols) tLayer
          right = length tailCols - 1
          parr = bpnParr (tailCols !! right)
          b = lastNonneg parr
       in if b < 0 then (-1, -1) else (parr !! b, b)
  where
    bpnParr (PCol p _) = p

skipLayerP :: PNode -> Int -> PNode
skipLayerP pn 0 = pn
skipLayerP (PNode cols) k =
  let right = length cols - 1
      pc = cols !! right
      hr = length (bpnH pc) - 1
   in skipLayerP (bpnH pc !! hr) (k - 1)
  where
    bpnH (PCol _ h) = h

ascensionVector :: BtNode -> Int -> Int -> Int -> Int -> [Int]
ascensionVector e b rLayer rIndex tLayer =
  let rE = skipLayer e rLayer
      tE = skipLayer rE (tLayer - rLayer)
      colR = bArr (btCols rE !! rIndex)
      colT = bArr (last (btCols tE))
   in [colT !! j - colR !! j | j <- [0 .. b - 1]]

ascendVector :: [Int] -> Int -> [Int] -> Int -> [Int]
ascendVector v a vv w =
  [ if j < a && j < length vv then v !! j + vv !! j * w else v !! j | j <- [0 .. length v - 1] ]

-- ---------------------------------------------------------------------------
-- special
-- ---------------------------------------------------------------------------

isSpecial :: BtNode -> Int -> Bool
isSpecial e tLayer
  | tLayer == 0 = False
  | tLayer > 1 = isSpecial (skipLayer e (tLayer - 1)) 1
  | otherwise =
      let tail = last (btCols e)
          next = nextLayer e
          nextTail = last (btCols next)
       in not (null (bHigher nextTail)) == False
            && lastPos (bArr nextTail) == 0
            && bArr nextTail !! 0 == bArr tail !! 0 + 1

fsSpecial :: BtNode -> Int -> Int -> BtNode
fsSpecial e tLayer index =
  let right = length (btCols e) - 1
      col = btCols e !! right
      hr = length (bHigher col) - 1
      keptCols = [btCols e !! i | i <- [0 .. right - 1]]
      keptHigher = [bHigher col !! k | k <- [0 .. hr - 1]]
      newHigher =
        if tLayer == 1
          then
            let h = bHigher col !! hr
                vertRight = length (btCols h) - 1
                newVert = BtNode [btCols h !! k | k <- [0 .. vertRight - 1]]
             in keptHigher ++ replicate index newVert
          else keptHigher ++ [fsSpecial (bHigher col !! hr) (tLayer - 1) index]
   in BtNode (keptCols ++ [BtColumn (bArr col) newHigher])

-- ---------------------------------------------------------------------------
-- ascend replace
-- ---------------------------------------------------------------------------

ascendReplace :: BtNode -> BtNode -> Maybe Int -> Maybe ANode -> [Int] -> Int -> [HNode] -> BtNode
ascendReplace e tail tl aTree vv w critical =
  BtNode (concatMap process (zip [0 ..] (btCols e)))
  where
    ncols = btCols e
    process (i, col)
      | tl == Just 0 && i + 1 == length ncols = map id (btCols tail)
      | otherwise =
          let aiVal = case aTree of
                Just (ANode acols) | i < length acols -> aTh (acols !! i)
                _ -> 0
              newLower = ascendVector (bArr col) aiVal vv w
              ntl = if i + 1 == length ncols then subtract 1 <$> tl else Nothing
              n = length (bArr col)
              colVertical = computeVertical col n
              hr = length (bHigher col) - 1
              m = pickM 0
              pickM mm
                | mm >= length colVertical || mm >= length critical = mm
                | mm == length colVertical - 1 && ntl /= Nothing = mm
                | otherwise =
                    let c = heightCmp (colVertical !! mm) (critical !! mm)
                     in if c > 0 then mm
                          else if c < 0
                            then if ntl /= Nothing then length colVertical - 1 else length colVertical
                            else pickM (mm + 1)
              kept = [fromHeightProper newLower (colVertical !! k) n | k <- [0 .. m - 1]]
              rest =
                [ let jtl = if j == hr then ntl else Nothing
                      subA = case aTree of
                        Just (ANode acols) | i < length acols, j < length (aThs (acols !! i)) -> Just (aThs (acols !! i) !! j)
                        _ -> Nothing
                   in ascendReplace (bHigher col !! j) tail jtl subA vv w critical
                | j <- [m .. length colVertical - 1]
                ]
           in [BtColumn newLower (kept ++ rest)]
    aTh (ACol t _) = t
    aThs (ACol _ h) = h

-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------


-- ---------------------------------------------------------------------------
-- expand
-- ---------------------------------------------------------------------------

isLimit :: BtNode -> Bool
isLimit e =
  not (null (btCols e)) && not (isZeroCol (last (btCols e)))

popLast :: BtNode -> BtNode
popLast e = BtNode (if null (btCols e) then [] else init (btCols e))

cloneSlice :: BtNode -> Int -> BtNode
cloneSlice e from = BtNode [btCols e !! i | i <- [from .. length (btCols e) - 1]]

aSlice :: ANode -> Int -> ANode
aSlice (ANode cols) from = ANode [cols !! i | i <- [from .. length cols - 1]]

expand :: BtNode -> Int -> Int -> Bool -> BtNode
expand e index n lnzM1
  | null (btCols e) = e
  | not (isLimit e) = popLast e
  | otherwise =
      let pnode = computeParents e n
          (_, tLayer) = computeTail e
          (r, b) = root e pnode tLayer
       in if isSpecial e tLayer
            then fsSpecial e tLayer index
            else
              let hasRoot = b >= 0
                  (rLayer, rIndex) = if hasRoot then rootLayer e r else (0, 0)
                  stk = []
                  aTree = if hasRoot then thresholds e pnode r b stk else emptyAt e
                  vVec = if hasRoot then ascensionVector e b rLayer rIndex tLayer else []
                  rE = skipLayer e rLayer
                  copyPart = cloneSlice rE rIndex
                  rA = skipLayerA aTree rLayer
                  copyPartA = aSlice rA rIndex
                  eT = skipLayer e tLayer
                  tail = last (btCols eT)
                  critical = computeVertical tail n
                  critical' = if null critical then critical else init critical
                  result0 = btMakeNode
                  (result1, index1) =
                    if lnzM1 && index > 0
                      then
                        let skip2 =
                              length (btCols copyPart) == 1
                                || (length (btCols copyPart) == 2 && rLayer == tLayer)
                            skip1
                              | b == n = True
                              | b == n - 1 = null (bHigher (btCols copyPart !! 0))
                              | otherwise = bArr (btCols copyPart !! 0) !! (b + 1) == 0
                            (resAfterSkip1, idxAfterSkip1) =
                              if not skip1 && index > 0
                                then
                                  if index == 1
                                    then
                                      let c0 =
                                            if b == n
                                              then BtColumn (bArr tail) (init (bHigher tail))
                                              else BtColumn (adjust b (bArr tail)) []
                                       in (BtNode [c0], 0)
                                    else (result0, index - 1)
                                else (result0, index)
                            (resAfterSkip2, idxAfterSkip2) =
                              if not skip2 && idxAfterSkip1 > 0
                                then
                                  if idxAfterSkip1 == 1
                                    then
                                      let one = BtNode [btCols copyPart !! 0]
                                          oneA = ANode [case copyPartA of ANode (ac : _) -> ac]
                                          r1 = ascendReplace one btMakeNode Nothing (Just oneA) vVec 1 []
                                          c0 =
                                            if b == n
                                              then
                                                let cLower = bArr (btCols r1 !! 0)
                                                    cHigher = [fromHeightProper cLower x n | x <- critical']
                                                 in BtColumn cLower cHigher
                                              else btCols r1 !! 0
                                       in (BtNode [c0], 0)
                                    else (resAfterSkip1, idxAfterSkip1 - 1)
                                else (resAfterSkip1, idxAfterSkip1)
                         in (resAfterSkip2, idxAfterSkip2)
                      else (result0, index)
                  result2 = foldl (\res w ->
                    let r2 = ascendReplace copyPart res (Just (tLayer - rLayer)) (Just copyPartA) vVec w critical'
                     in if b == n
                          then
                            let cLower = bArr (btCols r2 !! 0)
                                cHigher = [fromHeightProper cLower x n | x <- critical']
                                c0 = BtColumn cLower cHigher
                             in r2 { btCols = repIdx 0 c0 (btCols r2) }
                          else r2)
                    result1
                    [index1, index1 - 1 .. 1]
                  result3 = ascendReplace e result2 (Just tLayer) (Just aTree) vVec 0 []
               in result3
  where
    adjust b arr = repIdx b (arr !! b - 1) arr

repIdx :: Int -> a -> [a] -> [a]
repIdx k v xs = take k xs ++ [v] ++ drop (k + 1) xs

lnz :: Bool -> BtNode -> Int -> Int -> BtNode
lnz short_ e index n
  | not (isLimit e) = expand e 0 n short_
  | otherwise = expand e index n short_

-- ---------------------------------------------------------------------------
-- 入口
-- ---------------------------------------------------------------------------

expandBt3 :: String -> Int -> Int -> String
expandBt3 text term n0 =
  let n = max 1 (min 25 n0)
      term' = max 1 term
   in case runP text (parseNode n) of
        Right (e, i) | i >= length text -> safeRun (display (lnz False e term' n))
        _ -> kParseError

-- alter entry: short_ = True
expandBt3Alter :: String -> Int -> Int -> String
expandBt3Alter text term n0 =
  let n = max 1 (min 25 n0)
      term' = max 1 term
   in case runP text (parseNode n) of
        Right (e, i) | i >= length text -> safeRun (display (lnz True e term' n))
        _ -> kParseError

-- C++ 在越界/UB 路径会 segfault（stdout 为空）；Haskell 这边捕获运行时异常后返回空串，
-- 与预言机 segfault 后 stdout 为空对齐。
safeRun :: String -> String
safeRun action = unsafePerformIO $ do
  r <- try (do _ <- evaluate (length action); return action) :: IO (Either SomeException String)
  case r of
    Left _ -> return ""
    Right s -> return s

-- parser (same as bt1)
parseNode :: Int -> P BtNode
parseNode n = do
  sp
  do
      e <- parseExpr n
      sp
      end <- atEnd
      if end then return e else err
  where
    advanceN k = sequence_ (replicate k advance)

parseExpr :: Int -> P BtNode
parseExpr n = do
  sp
  cols <- parseCols
  return (BtNode cols)
  where
    parseCols = do
      sp
      mc <- peek
      case mc of
        Just '(' -> do
          c <- parseColumn n
          rest <- parseCols
          return (c : rest)
        _ -> return []

parseColumn :: Int -> P BtColumn
parseColumn n = do
  sp
  expect '('
  numbers <- readNumbers
  higher <- readHigher numbers
  sp
  expect ')'
  let arr = numbers ++ replicate (n - length numbers) 0
  return (BtColumn arr higher)
  where
    expect ch = do
      mc <- peek
      case mc of
        Just c | c == ch -> advance
        _ -> err
    readNumbers = go []
      where
        go acc = do
          sp
          mc <- peek
          case mc of
            Just c | c >= '0' && c <= '9', length acc < n -> do
              v <- numTok
              let acc' = acc ++ [v]
              sp
              mc2 <- peek
              case mc2 of
                Just ',' -> do
                  advance
                  sp
                  mc3 <- peek
                  case mc3 of
                    Just d | d >= '0' && d <= '9', length acc' < n -> go acc'
                    _ -> return acc'
                _ -> return acc'
            _ -> return acc
    readHigher nums = do
      sp
      mc <- peek
      case mc of
        Just '*' -> do
          if length nums /= n then err else return ()
          advance
          sp
          mc2 <- peek
          case mc2 of
            Just '^' -> do
              advance
              h <- parseExpr n
              sp
              mc3 <- peek
              case mc3 of
                Just ',' -> do
                  advance
                  rest <- readHigher nums
                  return (h : rest)
                _ -> return [h]
            _ -> do
              sp
              mc3 <- peek
              case mc3 of
                Just ',' -> do
                  advance
                  rest <- readHigher nums
                  return (btMakeNode : rest)
                _ -> return [btMakeNode]
        _ -> return []
