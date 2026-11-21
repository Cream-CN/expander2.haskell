-- | BT 族公共骨架（bt1/bt2/bt3/btlynss）。逐字移植 bt1ynss.hpp / bt2ynss.hpp 的公共结构。
--   列子结构是 higher 数组（多个子表达式），显示 *^expr 前缀。
module OmegaY.Fam.M1ynss.Bt
  ( BtNode(..)
  , BtColumn(..)
  , btMakeNode
  , btIsZeroColumn
  , btIsLimit
  , btNumberCmp
  , btCompare
  , btColumnDisplay
  , btTopDisplay
  , btDisplay
  , btRemoveBase
  , btHighestWithoutBase
  , btIsOneLineColumn
  , btIsSpecialColumn
  , BtPCol(..)
  , btLastNonneg
  , BtACol(..)
  , btUndefinedAt
  , btThresholds
  , btAscendVector
  , btCloneSlice
  , btPopLast
  , btASlice
  , btAscendReplace
  , btTailLayer
  , btRootLayer
  , btDescendP
  , btRoot
  , btAscensionVector
  , btFsSpecial
  , btFs
  , BtVariant(..)
  , expandBt
  ) where

import OmegaY.Fam.M1ynss.PP

-- ---------------------------------------------------------------------------
-- 数据类型
-- ---------------------------------------------------------------------------

data BtNode = BtNode
  { btCols :: [BtColumn]
  } deriving (Eq, Show)

data BtColumn = BtColumn
  { bArr :: [Int]
  , bHigher :: [BtNode]
  } deriving (Eq, Show)

btMakeNode :: BtNode
btMakeNode = BtNode []


btIsZeroColumn :: BtColumn -> Bool
btIsZeroColumn c = all (== 0) (bArr c) && null (bHigher c)

btNumberCmp :: Int -> Int -> Int
btNumberCmp a b = if a == b then 0 else if a < b then (-1) else 1

-- ---------------------------------------------------------------------------
-- 比较
-- ---------------------------------------------------------------------------

btCompare :: BtNode -> BtNode -> Int
btCompare a b = go 0
  where
    m = min (length (btCols a)) (length (btCols b))
    go i
      | i >= m = btNumberCmp (length (btCols a)) (length (btCols b))
      | otherwise =
          let c = btColumnCompare (btCols a !! i) (btCols b !! i)
           in if c /= 0 then c else go (i + 1)

btCompareSharedHigher :: [BtNode] -> [BtNode] -> Int
btCompareSharedHigher a b = go 0
  where
    m = min (length a) (length b)
    go i
      | i >= m = btNumberCmp (length a) (length b)
      | otherwise =
          let c = btCompare (a !! i) (b !! i)
           in if c /= 0 then c else go (i + 1)

btColumnCompare :: BtColumn -> BtColumn -> Int
btColumnCompare a b = goArr 0
  where
    m = min (length (bArr a)) (length (bArr b))
    goArr i
      | i >= m =
          let c = btNumberCmp (length (bArr a)) (length (bArr b))
           in if c /= 0 then c else btCompareSharedHigher (bHigher a) (bHigher b)
      | otherwise =
          let c = btNumberCmp (bArr a !! i) (bArr b !! i)
           in if c /= 0 then c else goArr (i + 1)

-- ---------------------------------------------------------------------------
-- 显示
-- ---------------------------------------------------------------------------

btColumnDisplay :: BtColumn -> String
btColumnDisplay c =
  let list = map show (bArr c) ++ map btTopDisplay (bHigher c)
      trimmed = reverse (dropWhile (== "0") (reverse list))
   in "(" ++ commas trimmed ++ ")"
  where
    commas xs = foldr (\x acc -> x ++ (if null acc then "" else "," ++ acc)) "" xs

btTopDisplay :: BtNode -> String
btTopDisplay e
  | null (btCols e) = "*"
  | otherwise = "*^" ++ btDisplay e

btDisplay :: BtNode -> String
btDisplay e = concatMap btColumnDisplay (btCols e)

-- ---------------------------------------------------------------------------
-- 去基 / 特判
-- ---------------------------------------------------------------------------

btRemoveBase :: BtNode -> Int -> BtNode
btRemoveBase a base =
  BtNode (map fixCol (btCols a))
  where
    fixCol col =
      let arr = bArr col
          arr' = if null arr then arr else repIdx 0 (head arr - base) arr
       in BtColumn arr' (map (`btRemoveBase` base) (bHigher col))

repIdx :: Int -> Int -> [Int] -> [Int]
repIdx k v xs = take k xs ++ [v] ++ drop (k + 1) xs

btHighestWithoutBase :: BtColumn -> [BtNode]
btHighestWithoutBase c =
  let base = if null (bArr c) then 0 else head (bArr c) + 1
   in map (`btRemoveBase` base) (bHigher c)

btIsOneLineColumn :: BtColumn -> Int -> Bool
btIsOneLineColumn c value =
  not (null (bArr c))
    && bArr c !! 0 == value
    && all (== 0) (drop 1 (bArr c))
    && null (bHigher c)

btIsSpecialColumn :: BtColumn -> Bool
btIsSpecialColumn c =
  let hr = length (bHigher c) - 1
   in if hr < 0
        then False
        else
          let h = bHigher c !! hr
              vr = length (btCols h) - 1
           in vr >= 0 && btIsOneLineColumn (btCols h !! vr) (if null (bArr c) then 0 else head (bArr c) + 1)

-- ---------------------------------------------------------------------------
-- 父树
-- ---------------------------------------------------------------------------

data BtPCol = BtPCol
  { bpParr :: [Int]
  , bpSubs :: [BtNode -> BtNode]  -- 占位，实际由 compute 填充
  }

-- 我们用更直接的表示：PNode = [BtPCol']，其中每个 PCol 有 parr 和 subs(PNode)
data BtPNode = BtPNode [BtPColN]
data BtPColN = BtPColN
  { bpnParr :: [Int]
  , bpnSubs :: [BtPNode]
  }

btLastNonneg :: [Int] -> Int
btLastNonneg v = go (length v - 1)
  where
    go i
      | i < 0 = -1
      | v !! i >= 0 = i
      | otherwise = go (i - 1)

-- ---------------------------------------------------------------------------
-- 阈值树
-- ---------------------------------------------------------------------------

data BtACol = BtACol
  { baA :: Maybe Int
  , baSubs :: [BtANode]
  }
newtype BtANode = BtANode [BtACol]

btUndefinedAt :: BtNode -> BtANode
btUndefinedAt e =
  BtANode [BtACol Nothing (map btUndefinedAt (bHigher col)) | col <- btCols e]

btThresholds :: BtNode -> Maybe BtPNode -> Maybe Int -> Int -> [Maybe Int] -> BtANode
btThresholds e pm ropt b stk =
  case ropt of
    Nothing -> btUndefinedAt e
    Just r -> BtANode (snd (foldl (stepCol r) (stk, []) (zip [0 ..] (btCols e))))
  where
    psubSub i ix = case pm of
      Just (BtPNode cols) | i < length cols, ix < length (bpnSubs (cols !! i)) -> Just (bpnSubs (cols !! i) !! ix)
      _ -> Nothing
    stepCol r (stk', acc) (i, col) =
      let iS = length stk'
       in if iS < r && i + 1 /= length (btCols e)
            then
              let subs = map btUndefinedAt (bHigher col)
               in (stk' ++ [Nothing], acc ++ [BtACol Nothing subs])
            else
              let ai
                    | iS == r = Just b
                    | iS > r =
                        let parr = case pm of
                              Just (BtPNode cols) | i < length cols -> bpnParr (cols !! i)
                              _ -> []
                         in Just (walkAi parr stk' r)
                    | otherwise = Nothing
                  subs =
                    [ case psubSub i ix of
                        Just pn -> btThresholds (bHigher col !! ix) (Just pn) (Just r) b (stk' ++ [ai])
                        Nothing -> btThresholds (bHigher col !! ix) Nothing (Just r) b (stk' ++ [ai])
                    | ix <- [0 .. length (bHigher col) - 1]
                    ]
               in (stk' ++ [ai], acc ++ [BtACol ai subs])
    walkAi parr stk' r =
      let go ai =
            if ai < length parr
              && parr !! ai >= r
              && parr !! ai < length stk'
              && maybe False (> ai) (stk' !! (parr !! ai))
              then go (ai + 1)
              else ai
       in go 0

-- ---------------------------------------------------------------------------
-- 复制 / 提升
-- ---------------------------------------------------------------------------

btCloneSlice :: BtNode -> Int -> BtNode
btCloneSlice e from =
  BtNode [btCols e !! i | i <- [from .. length (btCols e) - 1]]

btPopLast :: BtNode -> BtNode
btPopLast e =
  BtNode (if null (btCols e) then [] else init (btCols e))

btASlice :: BtANode -> Int -> BtANode
btASlice (BtANode cols) from = BtANode [cols !! i | i <- [from .. length cols - 1]]

btAscendVector :: [Int] -> Int -> [Int] -> Int -> [Int]
btAscendVector v ai vv w =
  [ if j < ai && j < length vv then v !! j + vv !! j * w else v !! j | j <- [0 .. length v - 1] ]

btAscendReplace :: BtNode -> BtNode -> Maybe Int -> Maybe BtANode -> [Int] -> Int -> BtNode
btAscendReplace e tail tl aTree vVec w =
  BtNode (concatMap process (zip [0 ..] (btCols e)))
  where
    ncols = btCols e
    process (i, col)
      | tl == Just 0 && i + 1 == length ncols = map id (btCols tail)
      | otherwise =
          let aiVal = case aTree of
                Just (BtANode acols) | i < length acols -> maybe 0 id (baA (acols !! i))
                _ -> 0
              newArr = btAscendVector (bArr col) aiVal vVec w
              ntl = if i + 1 == length ncols then (subtract 1) <$> tl else Nothing
              hr = length (bHigher col) - 1
              newHigher =
                [ let xntl = if ix == hr then ntl else Nothing
                      subA = case aTree of
                        Just (BtANode acols) | i < length acols, ix < length (baSubs (acols !! i)) -> Just (baSubs (acols !! i) !! ix)
                        _ -> Nothing
                   in btAscendReplace (bHigher col !! ix) tail xntl subA vVec w
                | ix <- [0 .. length (bHigher col) - 1]
                ]
           in [BtColumn newArr newHigher]

-- ---------------------------------------------------------------------------
-- 层 / 根
-- ---------------------------------------------------------------------------

btTailLayer :: BtNode -> (Int, Bool)
btTailLayer e
  | null (btCols e) || btIsZeroColumn (Prelude.last (btCols e)) = (-1, False)
  | otherwise = go e 0
  where
    go cur layer =
      let right = length (btCols cur) - 1
          col = btCols cur !! right
          hr = length (bHigher col) - 1
       in if hr < 0
            then (layer, False)
            else if btIsSpecialColumn col
              then (layer, True)
              else
                let h = bHigher col !! hr
                 in if null (btCols h)
                      then (layer, False)
                      else go h (layer + 1)

btRootLayer :: BtNode -> Int -> (Int, Int)
btRootLayer e r = go e 0 (length (btCols e))
  where
    go cur layer len
      | len <= r =
          let right = length (btCols cur) - 1
              col = btCols cur !! right
              hr = length (bHigher col) - 1
              h = bHigher col !! hr
           in go h (layer + 1) (len + length (btCols h))
      | otherwise = (layer, r - (len - length (btCols cur)))

btDescendP :: BtPNode -> Int -> BtPNode
btDescendP pnode 0 = pnode
btDescendP (BtPNode cols) k =
  let right = length cols - 1
      pc = cols !! right
      hr = length (bpnSubs pc) - 1
   in btDescendP (bpnSubs pc !! hr) (k - 1)

btRoot :: BtNode -> BtPNode -> Int -> (Int, Int)
btRoot e pin tailLayer
  | null (btCols e) || btIsZeroColumn (Prelude.last (btCols e)) = (-1, -1)
  | otherwise =
      let p = btDescendP pin tailLayer
          BtPNode cols = p
          right = length cols - 1
          parr = bpnParr (cols !! right)
          b = btLastNonneg parr
       in if b < 0 then (-1, -1) else (parr !! b, b)

btAscensionVector :: BtNode -> Int -> Int -> Int -> [Int]
btAscensionVector e r b tailLayer =
  let st = collect e 0
      collect cur k =
        btCols cur
          ++ if k >= tailLayer
               then []
               else
                 let right = length (btCols cur) - 1
                     col = btCols cur !! right
                     hr = length (bHigher col) - 1
                     h = bHigher col !! hr
                  in collect h (k + 1)
      back = Prelude.last st
   in [bArr back !! j - bArr (st !! r) !! j | j <- [0 .. b - 1]]

-- ---------------------------------------------------------------------------
-- FS special
-- ---------------------------------------------------------------------------

btFsSpecial :: BtNode -> Int -> Int -> BtNode
btFsSpecial e tailLayer index =
  let right = length (btCols e) - 1
      col = btCols e !! right
      hr = length (bHigher col) - 1
      keptCols = [btCols e !! i | i <- [0 .. right - 1]]
      keptHigher = [bHigher col !! k | k <- [0 .. hr - 1]]
      newHigher =
        if tailLayer == 0
          then
            let h = bHigher col !! hr
                vr = length (btCols h) - 1
                newVert = btCloneSlice h 0
                newVert' = newVert { btCols = take vr (btCols newVert) }
             in keptHigher ++ replicate index newVert'
          else keptHigher ++ [btFsSpecial (bHigher col !! hr) (tailLayer - 1) index]
   in BtNode (keptCols ++ [BtColumn (bArr col) newHigher])

-- ---------------------------------------------------------------------------
-- FS
-- ---------------------------------------------------------------------------

data BtVariant = Bt1 | Bt2 | Bt3 | Btl


btFs :: BtNode -> Int -> Int -> BtVariant -> BtNode
btFs e index n variant
  | null (btCols e) = e
  | not (btIsLimit e) = btPopLast e
  | otherwise =
      let (tLayer, isSpecial) = btTailLayer e
       in if isSpecial
            then btFsSpecial e tLayer index
            else
              let ptree = btComputeParents e n variant
                  (r0, b) = btRoot e ptree tLayer
                  hasRoot = b >= 0
                  (rLayer, ri) = if hasRoot then btRootLayer e r0 else (0, 0)
                  aTree = btThresholds e (Just ptree) (if hasRoot then Just r0 else Nothing) b []
                  vVec = if hasRoot then btAscensionVector e r0 b tLayer else []
                  cur = btSkipLayer e rLayer
                  curA = btSkipLayerA aTree rLayer
                  copyPart = btCloneSlice cur ri
                  copyPartA = btASlice curA ri
                  cur2 = btSkipLayer e tLayer
                  curA2 = btSkipLayerA aTree tLayer
                  right = length (btCols cur2) - 1
                  colRight = btCols cur2 !! right
                  tailTop = init (bHigher colRight)
                  tailTopA = case curA2 of
                    BtANode acols -> init (baSubs (acols !! right))
                  addWave res w =
                    let res1 = btAscendReplace copyPart res (Just (tLayer - rLayer)) (Just copyPartA) vVec w
                     in if b == n
                          then
                            let c0 = btCols res1 !! 0
                                c0' = c0
                                  { bHigher =
                                      [ btAscendReplace t (BtNode []) Nothing (Just ta) vVec (w - 1)
                                      | (t, ta) <- zip tailTop tailTopA
                                      ]
                                  }
                             in res1 { btCols = repAt 0 c0' (btCols res1) }
                          else res1
                  result = foldl addWave btMakeNode [index, index - 1 .. 1]
               in btAscendReplace e result (Just tLayer) (Just aTree) vVec 0

btIsLimit :: BtNode -> Bool
btIsLimit e =
  not (null (btCols e)) && not (btIsZeroColumn (Prelude.last (btCols e)))

btSkipLayer :: BtNode -> Int -> BtNode
btSkipLayer e k = iterate nextLayer e !! k
  where
    nextLayer cur =
      let right = length (btCols cur) - 1
          col = btCols cur !! right
          hr = length (bHigher col) - 1
       in bHigher col !! hr

btSkipLayerA :: BtANode -> Int -> BtANode
btSkipLayerA aTree 0 = aTree
btSkipLayerA (BtANode acols) k =
  let right = length acols - 1
      col = acols !! right
      hr = length (baSubs col) - 1
   in btSkipLayerA (baSubs col !! hr) (k - 1)

repAt :: Int -> a -> [a] -> [a]
repAt k v xs = take k xs ++ [v] ++ drop (k + 1) xs

-- ---------------------------------------------------------------------------
-- 父计算（变体分发）
-- ---------------------------------------------------------------------------

btComputeParents :: BtNode -> Int -> BtVariant -> BtPNode
btComputeParents e n variant =
  case variant of
    Bt1 -> bt1ComputeParents e n
    Bt2 -> bt2ComputeParents e n
    _ -> error "bt3/btl not yet implemented"

-- bt1: 同趟求下层父 + 顶层父（highest_without_base + forbidden）
bt1ComputeParents :: BtNode -> Int -> BtPNode
bt1ComputeParents e n = go e [] [] []
  where
    go cur colsStack parrStack forbidden =
      BtPNode (reverse built)
      where
        (_, _, built) = foldl step (colsStack, parrStack, []) (btCols cur)
        step (cStack, pStack, builtAcc) col =
          let iS = length cStack
              extCols = cStack ++ [col]
              parr0 = lowerPass extCols pStack iS
              extParrs0 = pStack ++ [parr0]
              topP = walkTopFn extCols extParrs0 forbidden col iS n
              parr = repAt n topP parr0
              extParrs = pStack ++ [parr]
              subs = [go (bHigher col !! ix) extCols extParrs (forbidden ++ [iS]) | ix <- [0 .. length (bHigher col) - 1]]
              pc = BtPColN parr subs
           in (extCols, extParrs, pc : builtAcc)
        lowerPass extCols pStack iS = goArr (replicate (n + 1) (-1)) 0
          where
            goArr arr j
              | j >= n = arr
              | otherwise =
                  let v = bArr (Prelude.last extCols) !! j
                      p = walk arr iS j v
                      arr' = if p < 0 then arr else repAt j p arr
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

walkTopFn :: [BtColumn] -> [[Int]] -> [Int] -> BtColumn -> Int -> Int -> Int
walkTopFn extCols pStack forbidden col iS n = go iS
  where
    go p
      | p < 0 = p
      | not (elem p forbidden)
          && btCompareSharedHigher
               (btHighestWithoutBase (extCols !! p))
               (btHighestWithoutBase col)
          < 0 = p
      | otherwise =
          let pNext = if n == 0 then p - 1 else (pStack !! p) !! (n - 1)
           in go pNext

-- bt2: lower + fill_top（相对 key）
bt2ComputeParents :: BtNode -> Int -> BtPNode
bt2ComputeParents e n =
  let lower = bt2Lower e n
   in bt2FillTop e lower n

bt2Lower :: BtNode -> Int -> BtPNode
bt2Lower e n = go e [] []
  where
    go cur cStack pStack =
      BtPNode (reverse built)
      where
        (_, _, built) = foldl step (cStack, pStack, []) (btCols cur)
        step (cs, ps, builtAcc) col =
          let iS = length cs
              extCols = cs ++ [col]
              parr = lowerPass extCols ps iS
              extParrs = ps ++ [parr]
              subs = [go (bHigher col !! ix) extCols extParrs | ix <- [0 .. length (bHigher col) - 1]]
              pc = BtPColN parr subs
           in (extCols, extParrs, pc : builtAcc)
        lowerPass extCols pStack iS = goArr (replicate (n + 1) (-1)) 0
          where
            goArr arr j
              | j >= n = arr
              | otherwise =
                  let v = bArr (Prelude.last extCols) !! j
                      p = walk arr iS j v
                      arr' = if p < 0 then arr else repAt j p arr
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

-- 相对 key
data RKeyColN = RKeyColN [(Bool, Int)] [RKeyN]
newtype RKeyN = RKeyN [RKeyColN]

kLow :: RKeyColN -> [(Bool, Int)]
kLow (RKeyColN low _) = low

kSub :: RKeyColN -> [RKeyN]
kSub (RKeyColN _ subs) = subs

bt2ToRelative :: BtColumn -> BtNode -> Maybe BtANode -> Int -> RKeyN
bt2ToRelative base cur tmproot n =
  RKeyN [ oneCol i col | (i, col) <- zip [0 ..] (btCols cur) ]
  where
    oneCol i col =
      let t = case tmproot of
            Just (BtANode acols) | i < length acols -> baA (acols !! i)
            _ -> Nothing
          tv = maybe (-1) id t
          low = [ if j < tv then (True, bArr col !! j - bArr base !! j) else (False, bArr col !! j) | j <- [0 .. n - 1] ]
          subT = case tmproot of
            Just (BtANode acols) | i < length acols -> Just (baSubs (acols !! i))
            _ -> Nothing
          subs =
            [ case subT of
                Just subsArr | ix < length subsArr -> bt2ToRelative base (bHigher col !! ix) (Just (subsArr !! ix)) n
                _ -> bt2ToRelative base (bHigher col !! ix) Nothing n
            | ix <- [0 .. length (bHigher col) - 1]
            ]
       in RKeyColN low subs

bt2RelCompare :: RKeyN -> RKeyN -> Int
bt2RelCompare (RKeyN a) (RKeyN b) = go 0
  where
    m = min (length a) (length b)
    go i
      | i >= m = btNumberCmp (length a) (length b)
      | otherwise =
          let c = goLow 0 (a !! i) (b !! i)
           in if c /= 0
                then c
                else
                  let sc = goSub (kSub (a !! i)) (kSub (b !! i))
                   in if sc /= 0 then sc else go (i + 1)
    goLow j x y
      | j >= min (length (kLow x)) (length (kLow y)) =
          btNumberCmp (length (kLow x)) (length (kLow y))
      | otherwise =
          let (bx, vx) = kLow x !! j
              (by, vy) = kLow y !! j
           in case btNumberCmp (boolToInt bx) (boolToInt by) of
                0 -> case btNumberCmp vx vy of
                  0 -> goLow (j + 1) x y
                  c -> c
                c -> c
    boolToInt b = if b then 1 else 0
    goSub x y =
      let m = min (length x) (length y)
          go i
            | i >= m = btNumberCmp (length x) (length y)
            | otherwise =
                let c = bt2RelCompare (x !! i) (y !! i)
                 in if c /= 0 then c else go (i + 1)
       in go 0

bt2FillTop :: BtNode -> BtPNode -> Int -> BtPNode
bt2FillTop e pt n = fst (go e pt [] [])
  where
    go cur (BtPNode cols) pStack keys =
      let (_, _, built, keys') = foldl step (pStack, cols, [], keys) (zip [0 ..] (btCols cur))
       in (BtPNode (reverse built), keys')
      where
        step (ps, ptCols, builtAcc, keyStack) (i, col) =
          let iS = length ps
              parr = bpnParr (ptCols !! i)
              ps' = ps ++ [parr]
              oneTree = btThresholds (BtNode [col]) (Just (BtPNode [ptCols !! i])) (Just iS) n (replicate iS Nothing)
              atSubs = case oneTree of
                BtANode (ac : _) -> baSubs ac
                _ -> []
              key = [bt2ToRelative col (bHigher col !! j) (Just (atSubs !! j)) n | j <- [0 .. length (bHigher col) - 1]]
              keyStack' = keyStack ++ [key]
              walkTop p
                | p < 0 = p
                | not (elem iS []) && bt2RelCompareHigher (keyStack' !! p) key < 0 = p
                | otherwise =
                    let pNext = (ps' !! p) !! (n - 1)
                     in walkTop pNext
              p = walkTop iS
              parr' = repAt n p parr
              subs = [case go (bHigher col !! ix) (btPNodeSub (ptCols !! i) ix) ps' keyStack' of (pn, _) -> pn | ix <- [0 .. length (bHigher col) - 1]]
              pc = BtPColN parr' subs
           in (ps', ptCols, pc : builtAcc, keyStack')
    bt2RelCompareHigher a b =
      let m = min (length a) (length b)
          go i
            | i >= m = btNumberCmp (length a) (length b)
            | otherwise =
                let c = bt2RelCompare (a !! i) (b !! i)
                 in if c /= 0 then c else go (i + 1)
       in go 0

btPNodeSub :: BtPColN -> Int -> BtPNode
btPNodeSub pc ix = bpnSubs pc !! ix

-- ---------------------------------------------------------------------------
-- 入口
-- ---------------------------------------------------------------------------

expandBt :: BtVariant -> String -> Int -> Int -> String
expandBt variant text term n =
  let term' = max 1 term
   in case runP text (parseNode n) of
        Right (e, i) | i >= length text -> btDisplay (btFs e term' n variant)
        _ -> kParseError

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
