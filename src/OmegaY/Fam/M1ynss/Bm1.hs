-- | 分支族 abs-bm1ynss / rel-bm1ynss，逐字移植 @notation/bm1ynss.hpp@。
--   复用 @Tm1@ 的 Node/Column/Parser；仅父表顶层父求法不同（abs 绝对比较、rel 相对 key）。
module OmegaY.Fam.M1ynss.Bm1
  ( expandAbs
  , expandRel
  ) where

import OmegaY.Fam.M1ynss.PP
import OmegaY.Fam.M1ynss.Tm1

data BPCol = BPCol { bpArr :: [Int], bpSub :: [BPCol] }

data ACol = ACol { aA :: Maybe Int, aSub :: [ACol] }

data RKeyCol = RKeyCol { kLow :: [(Bool, Int)], kSub :: [RKeyCol] }

-- 基础谓词

brIsLimit :: Node -> Bool
brIsLimit e =
  not (null (nCols e)) && not (isZeroColumn (Prelude.last (nCols e)))

nextLayer :: Node -> Node
nextLayer e = cStep (Prelude.last (nCols e))

skipLayer :: Node -> Int -> Node
skipLayer e k = iterate nextLayer e !! k

repAt :: Int -> Int -> [Int] -> [Int]
repAt k v xs = take k xs ++ [v] ++ drop (k + 1) xs

repColAt :: Int -> Column -> [Column] -> [Column]
repColAt k v xs = take k xs ++ [v] ++ drop (k + 1) xs

lastNonneg :: [Int] -> Int
lastNonneg v = go (length v - 1)
  where
    go i
      | i < 0 = -1
      | v !! i >= 0 = i
      | otherwise = go (i - 1)

-- 下层父（j=0..n-1），abs/rel 共用。stack/pstack 在同节点内随列增长（不 resize）。

brLower :: Node -> Int -> [Column] -> [[Int]] -> [BPCol]
brLower e n outerCols outerParrs =
  reverse built
  where
    (_, _, built) = foldl step (outerCols, outerParrs, []) (nCols e)
    step (colsStack, parrStack, builtAcc) col =
      let iS = length colsStack
          extCols = colsStack ++ [col]
          parr = lowerPass extCols parrStack iS
          extParrs = parrStack ++ [parr]
          sub = brLower (cStep col) n extCols extParrs
          pc = BPCol parr sub
       in (extCols, extParrs, pc : builtAcc)
    lowerPass extCols parrStack iS = go (replicate (n + 1) (-1)) 0
      where
        go arr j
          | j >= n = arr
          | otherwise =
              let v = cArr (Prelude.last extCols) !! j
                  p = walk arr iS j v
                  arr' = if p < 0 then arr else repAt j p arr
               in go arr' (j + 1)
        walk arr p jj vv
          | p < 0 = p
          | (cArr (extCols !! p) !! jj) < vv = p
          | otherwise =
              let pNext =
                    if jj == 0
                      then p - 1
                      else
                        if p == iS
                          then arr !! (jj - 1)
                          else (parrStack !! p) !! (jj - 1)
               in walk arr pNext jj vv

-- abs：同趟求顶层父（绝对 step 比较 + 排除祖先链）

brAbs :: Node -> Int -> [Column] -> [[Int]] -> [Int] -> [BPCol]
brAbs e n outerCols outerParrs forbidden =
  reverse built
  where
    (_, _, built) = foldl step (outerCols, outerParrs, []) (nCols e)
    step (colsStack, parrStack, builtAcc) col =
      let iS = length colsStack
          extCols = colsStack ++ [col]
          lowerArr = lowerPass extCols parrStack iS
          parr = repAt n (topWalk extCols (parrStack ++ [lowerArr]) forbidden iS (cStep col)) lowerArr
          extParrs = parrStack ++ [parr]
          sub = brAbs (cStep col) n extCols extParrs (forbidden ++ [iS])
          pc = BPCol parr sub
       in (extCols, extParrs, pc : builtAcc)
    lowerPass extCols parrStack iS = go (replicate (n + 1) (-1)) 0
      where
        go arr j
          | j >= n = arr
          | otherwise =
              let v = cArr (Prelude.last extCols) !! j
                  p = walk arr iS j v
                  arr' = if p < 0 then arr else repAt j p arr
               in go arr' (j + 1)
        walk arr p jj vv
          | p < 0 = p
          | (cArr (extCols !! p) !! jj) < vv = p
          | otherwise =
              let pNext =
                    if jj == 0
                      then p - 1
                      else
                        if p == iS
                          then arr !! (jj - 1)
                          else (parrStack !! p) !! (jj - 1)
               in walk arr pNext jj vv
    topWalk extCols extParrs forbidden p stepV
      | p < 0 = p
      | compareNode (cStep (extCols !! p)) stepV < 0 && not (elem p forbidden) = p
      | otherwise =
          let pNext = if n == 0 then p - 1 else (extParrs !! p) !! (n - 1)
           in topWalk extCols extParrs forbidden pNext stepV

-- rel：阈值树 / 相对 key / fill_top

brBoolCmp :: Bool -> Bool -> Int
brBoolCmp a b = (if a then 1 else 0) - (if b then 1 else 0)

brThresholds :: Node -> Maybe [BPCol] -> Maybe Int -> Int -> [Maybe Int] -> [ACol]
brThresholds e pm ropt b stk =
  case ropt of
    Nothing ->
      [ ACol Nothing (brThresholds (cStep col) Nothing Nothing b [])
      | col <- nCols e
      ]
    Just r ->
      snd $ foldl (stepCol r) (stk, []) (zip [0 ..] (nCols e))
  where
    psub i = case pm of
      Just pt | i < length pt -> Just (bpSub (pt !! i))
      _ -> Nothing
    stepCol r (stk', acc) (i, col) =
      let iS = length stk'
       in if iS < r && i + 1 /= length (nCols e)
            then
              let subA = brThresholds (cStep col) Nothing Nothing b []
               in (stk' ++ [Nothing], acc ++ [ACol Nothing subA])
            else
              let ai
                    | iS == r = Just b
                    | iS > r =
                        let parr = case pm of
                              Just pt -> bpArr (pt !! i)
                              Nothing -> []
                         in Just (walkAi parr stk' r)
                    | otherwise = Nothing
                  subA = brThresholds (cStep col) (psub i) (Just r) b (stk' ++ [ai])
               in (stk' ++ [ai], acc ++ [ACol ai subA])
    walkAi parr stk' r =
      let go ai =
            if ai < length parr
              && parr !! ai >= r
              && parr !! ai < length stk'
              && maybe False (> ai) (stk' !! (parr !! ai))
              then go (ai + 1)
              else ai
       in if null parr then 0 else go 0

brToRelative :: Column -> Node -> Maybe [ACol] -> Int -> [RKeyCol]
brToRelative base cur tmproot n =
  [ oneCol i col | (i, col) <- zip [0 ..] (nCols cur) ]
  where
    oneCol i col =
      let t = case tmproot of
            Just tA | i < length tA -> aA (tA !! i)
            _ -> Nothing
          tv = maybe (-1) id t
          low =
            [ if j < tv
                then (True, cArr col !! j - cArr base !! j)
                else (False, cArr col !! j)
            | j <- [0 .. n - 1]
            ]
          subT = case tmproot of
            Just tA | i < length tA -> Just (aSub (tA !! i))
            _ -> Nothing
       in RKeyCol low (brToRelative base (cStep col) subT n)

brKeyCmp :: [RKeyCol] -> [RKeyCol] -> Int
brKeyCmp a b = go 0
  where
    m = min (length a) (length b)
    go i
      | i >= m = numberCmp (length a) (length b)
      | otherwise =
          let c = goLow 0
           in if c /= 0
                then c
                else
                  let sc = brKeyCmp (kSub (a !! i)) (kSub (b !! i))
                   in if sc /= 0 then sc else go (i + 1)
      where
        goLow j
          | j >= km = numberCmp (length (kLow (a !! i))) (length (kLow (b !! i)))
          | otherwise =
              let lx = kLow (a !! i) !! j
                  ly = kLow (b !! i) !! j
               in case brBoolCmp (fst lx) (fst ly) of
                    0 -> case numberCmp (snd lx) (snd ly) of
                      0 -> goLow (j + 1)
                      c -> c
                    c -> c
        km = min (length (kLow (a !! i))) (length (kLow (b !! i)))

brFillTop :: Node -> [BPCol] -> Int -> [[Int]] -> [[RKeyCol]] -> [Int] -> [BPCol]
brFillTop e pt n outerParrs keys outer =
  reverse built
  where
    (_, _, built) = foldl step (outerParrs, keys, []) (zip [0 ..] (nCols e))
    step (opStack, keyStack, builtAcc) (i, col) =
      let iS = length opStack
          extParrs = opStack ++ [bpArr (pt !! i)]
          oneTree =
            brThresholds (Node [col]) (Just [pt !! i]) (Just iS) n (replicate iS Nothing)
          at = case oneTree of
            (ACol _ s : _) -> s
            [] -> []
          key = brToRelative col (cStep col) (if null at then Nothing else Just at) n
          allKeys = keyStack ++ [key]
          walkTop p
            | p < 0 = p
            | not (elem p outer) && brKeyCmp (allKeys !! p) key < 0 = p
            | otherwise =
                let pNext = if n == 0 then p - 1 else (extParrs !! p) !! (n - 1)
                 in walkTop pNext
          p = walkTop iS
          parr' = repAt n p (bpArr (pt !! i))
          subP = brFillTop (cStep col) (bpSub (pt !! i)) n extParrs allKeys (outer ++ [iS])
          pc = BPCol parr' subP
       in extParrs `seq` allKeys `seq` (extParrs, allKeys, pc : builtAcc)

-- 层 / 根 / 提升向量

brTailLayer :: Node -> Int
brTailLayer e
  | null (nCols e) || isZeroColumn (Prelude.last (nCols e)) = -1
  | otherwise = go e 0
  where
    go cur layer =
      let right = length (nCols cur) - 1
          stepN = cStep (nCols cur !! right)
       in if null (nCols stepN) || not (brIsLimit stepN)
            then layer
            else go stepN (layer + 1)

brRootLayer :: Node -> Int -> (Int, Int)
brRootLayer e r = go e 0 (length (nCols e))
  where
    go cur layer len =
      if len <= r
        then
          let right = length (nCols cur) - 1
              cur' = cStep (nCols cur !! right)
           in go cur' (layer + 1) (len + length (nCols cur'))
        else (layer, r - (len - length (nCols cur)))

brDescendP :: [BPCol] -> Int -> [BPCol]
brDescendP ptree 0 = ptree
brDescendP ptree k = brDescendP (bpSub (Prelude.last ptree)) (k - 1)

brRoot :: Node -> [BPCol] -> Int -> (Int, Int)
brRoot e pin tailLayer
  | null (nCols e) || isZeroColumn (Prelude.last (nCols e)) = (-1, -1)
  | otherwise =
      let pt = brDescendP pin tailLayer
          right = length pt - 1
          parr = bpArr (pt !! right)
          b = lastNonneg parr
       in if b < 0 then (-1, -1) else (parr !! b, b)

brAscensionVector :: Node -> Int -> Int -> Int -> [Int]
brAscensionVector e r b tailLayer =
  let st = collect e 0
      collect cur k =
        nCols cur
          ++ if k >= tailLayer
               then []
               else collect (nextLayer cur) (k + 1)
      back = Prelude.last st
   in [cArr back !! j - cArr (st !! r) !! j | j <- [0 .. b - 1]]

-- 复制 / 提升

cloneSlice :: Node -> Int -> Node
cloneSlice e from =
  Node [nCols e !! i | i <- [from .. length (nCols e) - 1]]

brPopLast :: Node -> Node
brPopLast e = Node (if null (nCols e) then [] else init (nCols e))

aSlice :: [ACol] -> Int -> [ACol]
aSlice aTree from = [aTree !! i | i <- [from .. length aTree - 1]]

aPopLast :: [ACol] -> [ACol]
aPopLast aTree = if null aTree then [] else init aTree

brAscendReplace :: Node -> Node -> Maybe Int -> Maybe [ACol] -> [Int] -> Int -> Node
brAscendReplace e tail tl aTree vVec w =
  Node (concatMap process (zip [0 ..] (nCols e)))
  where
    ncols = nCols e
    process (i, col)
      | tl == Just 0 && i + 1 == length ncols = nCols tail
      | otherwise = [processCol i col]
    processCol i col =
      let aiVal = case aTree of
            Just a | i < length a -> maybe 0 id (aA (a !! i))
            _ -> 0
          newArr =
            [ if j < aiVal
                then cArr col !! j + (if j < length vVec then vVec !! j else 0) * w
                else cArr col !! j
            | j <- [0 .. length (cArr col) - 1]
            ]
          ntl = if i + 1 == length ncols then (subtract 1) <$> tl else Nothing
          subA = case aTree of
            Just a | i < length a -> Just (aSub (a !! i))
            _ -> Nothing
       in Column newArr (brAscendReplace (cStep col) tail ntl subA vVec w)

-- FS

brComputeParents :: Node -> Int -> Bool -> [BPCol]
brComputeParents e n rel =
  if not rel
    then brAbs e n [] [] []
    else
      let ptree = brLower e n [] []
       in brFillTop e ptree n [] [] []

skipLayerA :: [ACol] -> Int -> [ACol]
skipLayerA aTree 0 = aTree
skipLayerA aTree k = skipLayerA (aSub (Prelude.last aTree)) (k - 1)

brFs :: Node -> Int -> Int -> Bool -> Node
brFs e index n rel
  | null (nCols e) = e
  | not (brIsLimit e) = brPopLast e
  | otherwise =
      let ptree = brComputeParents e n rel
          tLayer = brTailLayer e
          (r0, b) = brRoot e ptree tLayer
          hasRoot = b >= 0
          (rLayer, ri) = if hasRoot then brRootLayer e r0 else (0, 0)
          aTree = brThresholds e (Just ptree) (if hasRoot then Just r0 else Nothing) b []
          vVec = if hasRoot then brAscensionVector e r0 b tLayer else []
          cur = skipLayer e rLayer
          curA = skipLayerA aTree rLayer
          copyPart = cloneSlice cur ri
          copyPartA = aSlice curA ri
          cur2 = skipLayer e tLayer
          curA2 = skipLayerA aTree tLayer
          right = length (nCols cur2) - 1
          tailTop = brPopLast (cStep (nCols cur2 !! right))
          tailTopA = aPopLast (aSub (curA2 !! right))
          addWave res w =
            let res1 = brAscendReplace copyPart res (Just (tLayer - rLayer)) (Just copyPartA) vVec w
             in if b == n
                  then
                    let c0 = nCols res1 !! 0
                        newC0 =
                          c0
                            { cStep =
                                brAscendReplace tailTop makeNode Nothing (Just tailTopA) vVec (w - 1)
                            }
                     in res1 { nCols = repColAt 0 newC0 (nCols res1) }
                  else res1
          result = foldl addWave makeNode [index, index - 1 .. 1]
       in brAscendReplace e result (Just tLayer) (Just aTree) vVec 0

-- 显示（严格版）

brIsOneColumn :: Column -> Bool
brIsOneColumn c =
  let arr = cArr c
      len = length arr
   in if len == 0
        then length (nCols (cStep c)) == 1 && isZeroColumn (nCols (cStep c) !! 0)
        else
          arr !! 0 == 1
            && null (nCols (cStep c))
            && all (== 0) (drop 1 arr)

brColumnDisplay :: Column -> String
brColumnDisplay c =
  let list = map show (cArr c) ++ [brRender (cStep c) False]
      trimmed = reverse (dropWhile (== "0") (reverse list))
   in "(" ++ commas trimmed ++ ")"
  where
    commas xs = foldr (\x acc -> x ++ (if null acc then "" else "," ++ acc)) "" xs

brRender :: Node -> Bool -> String
brRender e topLevel
  | not topLevel =
      let cols = nCols e
          allZero = all isZeroColumn cols
       in if allZero
            then show (length cols)
            else
              if length cols == 2
                && isZeroColumn (cols !! 0)
                && brIsOneColumn (cols !! 1)
                then omegaChar
                else concatMap brColumnDisplay cols
  | otherwise = concatMap brColumnDisplay (nCols e)

-- 入口

expandBm :: Bool -> String -> Int -> Int -> String
expandBm rel text term n0 =
  let n = max 0 (min 25 n0)
      term' = max 1 term
      top = do
        sp
        e <- parseNode True n
        sp
        end <- atEnd
        if end then return e else err
   in case runP text top of
        Left _ -> kParseError
        Right (m, _) ->
          let lnz e idx =
                if not (brIsLimit e)
                  then brFs e 0 n rel
                  else brFs e idx n rel
           in brRender (lnz m term') True

expandAbs :: String -> Int -> Int -> String
expandAbs = expandBm False

expandRel :: String -> Int -> Int -> String
expandRel = expandBm True
