-- | ω-Y（medium / strong magma）展开，对齐 C++ 侧 @Omega-YMagma.hpp@。
--
--   原实现以 @Entry*@ 指针构成的「山脉」（每列一叠 Entry）为数据结构；
--   这里用 id + IntMap 表示同一张图，空指针统一表示为 @-1@。
module OmegaY.Notation.YMagma
  ( omegaYMediumNotation
  , omegaYStrongNotation
  , magmaExpand
  , MagmaKind (..)
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import OmegaY.EntryGraph
import OmegaY.Notation.Types

data MagmaKind = Medium | Strong
  deriving (Eq, Show)

-- ----------------------------------------------------------------------
-- 山脉（列号 → Entry id 列表）
-- ----------------------------------------------------------------------

newtype Mtn s = Mtn (STRef s (IntMap [Int]))

newMtn :: ST s (Mtn s)
newMtn = Mtn <$> newSTRef IM.empty

mRead :: Mtn s -> Int -> ST s [Int]
mRead (Mtn r) i = IM.findWithDefault [] i <$> readSTRef r

mWrite :: Mtn s -> Int -> [Int] -> ST s ()
mWrite (Mtn r) i col = writeSTRef r . IM.insert i col =<< readSTRef r

mLen :: Mtn s -> ST s Int
mLen (Mtn r) = do
  m <- readSTRef r
  pure (if IM.null m then 0 else fst (IM.findMax m) + 1)

mCol :: Mtn s -> Int -> ST s [Int]
mCol = mRead

pushCol :: Mtn s -> Int -> Int -> ST s ()
pushCol m i v = mWrite m i =<< (++ [v]) <$> mRead m i

-- ----------------------------------------------------------------------
-- 节点访问器
-- ----------------------------------------------------------------------

nullE :: EntryG
nullE = EntryG 0 0 [] [] Nothing Nothing Nothing

node :: Graph s -> Int -> ST s EntryG
node g i
  | i < 0 = pure nullE
  | otherwise = gGetDef g i

vOf, xOf :: EntryG -> Int
vOf = egValue
xOf = egX

yOf :: EntryG -> [Int]
yOf = egY

upLs :: EntryG -> [Int]
upLs = egUpL

upR, downR, downL :: EntryG -> Int
upR = fromMaybe (-1) . egUpR
downR = fromMaybe (-1) . egDownR
downL = fromMaybe (-1) . egDownL

sameRow :: EntryG -> EntryG -> Bool
sameRow a b = verticalCompare (egY a) (egY b) == 0

-- ----------------------------------------------------------------------
-- 序列 ⇄ 山脉
-- ----------------------------------------------------------------------

fromSequence :: Graph s -> Mtn s -> [Int] -> ST s ()
fromSequence g m seq = go 0
 where
  go i
    | i >= length seq = pure ()
    | otherwise = do
        bottom <- newNode g (seq !! i) i [1]
        phantom <- newNode g 0 i []
        gMod g bottom (\e -> e { egDownR = Just phantom })
        gMod g phantom (\e -> e { egUpR = Just bottom })
        when (i > 0) $ do
          prev <- mRead m (i - 1)
          let prevPhantom = prev !! 1
          gMod g bottom (\e -> e { egDownL = Just prevPhantom })
          pushUpL g prevPhantom bottom
        mWrite m i [bottom, phantom]
        go (i + 1)

toSequence :: Graph s -> Mtn s -> ST s [Int]
toSequence g m = do
  n <- mLen m
  mapM pick [0 .. n - 1]
 where
  pick i = do
    col <- mRead m i
    if length col < 2
      then pure 0
      else do
        e <- node g (col !! (length col - 2))
        pure (vOf e)

-- ----------------------------------------------------------------------
-- 绘图（补父项）与父项查找
-- ----------------------------------------------------------------------

createEntry :: Graph s -> Int -> Int -> ST s Int
createEntry g parent entry = do
  pe <- node g parent
  ee <- node g entry
  let dd = dimensionDifference (yOf pe) (yOf ee) + 1
  nid <- newNode g (vOf ee - vOf pe) (xOf ee) (verticalIncrease (yOf ee) dd)
  gMod g nid (\e -> e { egDownR = Just entry, egDownL = Just parent })
  gMod g entry (\e -> e { egUpR = Just nid })
  pushUpL g parent nid
  pure nid

drawMountain :: Graph s -> Mtn s -> ST s ()
drawMountain g m = do
  n <- mLen m
  mapM_ drawCol [0 .. n - 1]
 where
  drawCol i = do
    col0 <- mRead m i
    let loop col = case col of
          [] -> pure ()
          (entry : _) -> do
            ee <- node g entry
            if vOf ee == 1
              then pure ()
              else do
                parent <- ascendParent entry ee
                nid <- createEntry g parent entry
                mWrite m i (nid : col)
                loop (nid : col)
    loop col0

  -- 从 entry 的 leftleg_down 出发，沿 rightleg_up 上爬到第一个值更小的父项
  ascendParent entry ee = step (downL ee)
   where
    step parent = do
      pe <- node g parent
      up0 <- climb (downL pe) (yOf pe)
      ue <- node g up0
      if vOf ue < vOf ee then pure up0 else step up0
    climb up py = do
      ue <- node g up
      let ur = upR ue
      if ur >= 0
        then do
          ure <- node g ur
          if verticalCompare (yOf ure) py <= 0 then climb ur py else pure up
        else pure up

-- ----------------------------------------------------------------------
-- 列内二分查找
-- ----------------------------------------------------------------------

findLower :: Graph s -> [Int] -> [Int] -> ST s Int
findLower g col y
  | null col = pure (-1)
  | otherwise = go 0 (length col - 1)
 where
  go i1 i2
    | i1 >= i2 = pure (col !! i2)
    | otherwise = do
        let i = (i1 + i2) `div` 2
        e <- node g (col !! i)
        if verticalCompare (yOf e) y < 0 then go i1 i else go (i + 1) i2

findHigherEqual :: Graph s -> [Int] -> [Int] -> ST s Int
findHigherEqual g col y
  | null col = pure (-1)
  | otherwise = go 0 (length col - 1)
 where
  go i1 i2
    | i1 >= i2 = pure (col !! i1)
    | otherwise = do
        let i = (i1 + i2 + 1) `div` 2
        e <- node g (col !! i)
        if verticalCompare (yOf e) y >= 0 then go i i2 else go i1 (i - 1)

yslice :: Graph s -> [Int] -> [Int] -> [Int] -> ST s [Int]
yslice g col loweq high
  | null col = pure []
  | otherwise = do
      start <- binaryLow high 0 (length col - 1)
      end <- binaryLow loweq start (length col - 1)
      pure (if start >= end then [] else take (end - start) (drop start col))
 where
  binaryLow y i1 i2
    | i1 >= i2 = pure i2
    | otherwise = do
        let i = (i1 + i2) `div` 2
        e <- node g (col !! i)
        if verticalCompare (yOf e) y < 0 then binaryLow y i1 i else binaryLow y (i + 1) i2

-- ----------------------------------------------------------------------
-- 弱 / 强收集
-- ----------------------------------------------------------------------

collectWeak :: Graph s -> Int -> ST s [Int]
collectWeak g = go
 where
  go wid = do
    we <- node g wid
    foldM' we wid [] (upLs we)
  foldM' we wid acc [] = pure acc
  foldM' we wid acc (e : es) = do
    ee <- node g e
    let child = downR ee
    if child >= 0 && child `notElem` acc && sameRow we ee
      then do
        ce <- node g child
        if sameRow we ce
          then do
            acc' <- go child
            foldM' we wid (acc ++ [child] ++ acc') es
          else foldM' we wid acc es
      else foldM' we wid acc es

collectStrong :: Graph s -> Int -> ST s [Int]
collectStrong g = go
 where
  go wid = do
    we <- node g wid
    let dr = downR we
    if dr < 0
      then pure []
      else do
        de <- node g dr
        foldM' we (upLs de) []
  foldM' we [] acc = pure acc
  foldM' we (e : es) acc
    | e `elem` acc = foldM' we es acc
    | otherwise = do
        ee <- node g e
        if sameRow we ee
          then do
            acc' <- go e
            foldM' we es (acc ++ [e] ++ acc')
          else foldM' we es acc

-- ----------------------------------------------------------------------
-- 复制与补边
-- ----------------------------------------------------------------------

copySingleEdge :: Graph s -> Mtn s -> Int -> Int -> Int -> Maybe [Int] -> ST s ()
copySingleEdge g m src off brx mty = do
  se <- node g src
  let ty = fromMaybe (yOf se) mty
  nid <- newNode g 0 (xOf se + off) ty
  when (not (null (yOf se))) $ do
    dle <- node g (downL se)
    le <-
      if xOf dle >= brx
        then do
          col <- mRead m (xOf dle + off)
          findLower g col ty
        else pure (downL se)
    gMod g nid (\e -> e { egDownL = Just le })
    pushUpL g le nid
  pushCol m (xOf se + off) nid

fillMagmaEdge :: Graph s -> Mtn s -> Int -> Int -> ST s ()
fillMagmaEdge g m src ll = do
  se <- node g src
  dse <- node g (downL se)
  le <- node g ll
  lue <- node g (upR le)
  let targetx = xOf se - xOf dse + xOf le
      dd = dimensionDifference (yOf le) (yOf lue)
  mapM_ (mk targetx) [dd, dd - 1 .. 0]
 where
  mk targetx d = do
    leE <- node g ll
    nid <- newNode g 0 targetx (verticalIncrease (yOf leE) d)
    gMod g nid (\e -> e { egDownL = Just ll })
    pushUpL g ll nid
    pushCol m targetx nid

-- ----------------------------------------------------------------------
-- 岩浆展开主体
-- ----------------------------------------------------------------------

magmaExpand :: MagmaKind -> [Int] -> Int -> [Int]
magmaExpand kind seq term
  | null seq = []
  | last seq == 1 = init seq
  | otherwise = runST $ do
      g <- newGraph
      m0 <- newMtn
      fromSequence g m0 seq
      drawMountain g m0
      -- 定位 BR 与宽度、顶级切片
      n0 <- mLen m0
      childCol <- mRead m0 (n0 - 1)
      let childTop = head childCol
      cE <- node g childTop
      brE <- node g (downL cE)
      let br = downL cE
          brx = xOf brE
          width = n0 - 1 - brx
      top <- do
        col <- mRead m0 brx
        let midx = fromMaybe 0 (elemIndex br col)
        pure (childTop : take (length col - midx - 1) (drop (midx + 1) col))
      -- 末项减一后重画，并定位新的 BR
      m1 <- newMtn
      fromSequence g m1 (init seq ++ [last seq - 1])
      drawMountain g m1
      br2 <- do
        col <- mRead m1 brx
        let cands = [e | e <- col]
        findSameRow g cands br
      ents <- buildEntries g m1 kind brx br2 width
      mFinal <- loopN g m1 ents top brx width term 1
      toSequence g mFinal

findSameRow :: Graph s -> [Int] -> Int -> ST s Int
findSameRow g col ref = do
  re <- node g ref
  go re col
 where
  go _ [] = pure ref
  go re (e : es) = do
    ee <- node g e
    if sameRow ee re then pure e else go re es

-- | 按 dx（0..width）收集需要复制的 Entry。
buildEntries :: Graph s -> Mtn s -> MagmaKind -> Int -> Int -> Int -> ST s (IntMap [Int])
buildEntries g m kind brx br width = do
  start <- pure (IM.fromList [(d, []) | d <- [0 .. width]])
  case kind of
    Medium -> mediumLoop start br
    Strong -> strongLoop start br
 where
  addEnt m0 e = do
    ee <- node g e
    let dx = xOf ee - brx
    if dx >= 0 && dx <= width
      then pure (IM.adjust (++ [e]) dx m0)
      else pure m0

  mediumLoop acc br1 = do
    be <- node g br1
    coll <- collectWeak g br1
    acc' <- foldl' addEnt acc coll
    if null (yOf be)
      then pure acc'
      else
        if downR be < 0
          then pure acc'
          else mediumLoop acc' (downR be)

  strongLoop acc br1 = do
    be <- node g br1
    if not (null (yOf be))
      then do
        coll <- collectStrong g br1
        acc' <- foldl' addEnt acc coll
        if downR be < 0 then pure acc' else strongLoop acc' (downR be)
      else do
        n <- mLen m
        let dxs = [0 .. n - brx - 2]
        acc' <- foldl' (\a dx1 -> do
                          let idx = brx + 1 + dx1
                          col <- mRead m idx
                          if null col
                            then pure a
                            else addEnt a (last col)) acc dxs
        pure acc'

  foldl' f z [] = pure z
  foldl' f z (x : xs) = f z x >>= \z' -> foldl' f z' xs

loopN :: Graph s -> Mtn s -> IntMap [Int] -> [Int] -> Int -> Int -> Int -> Int -> ST s (Mtn s)
loopN g m ents top brx width term n
  | n > term = pure m
  | otherwise = do
      lastCol <- mRead m =<< ((subtract 1) <$> mLen m)
      ref <- mapM (\t -> node g t >>= \te -> findLower g lastCol (yOf te)) top
      m' <- loopDx ref 1 m
      loopN g m' ents top brx width term (n + 1)
 where
  loopDx ref dx m
    | dx > width = pure m
    | otherwise = do
        let idx = brx + n * width + dx
        mWrite m idx []
        mapM_ (oneEntry m ref dx) (IM.findWithDefault [] dx ents)
        col <- mRead m idx
        sorted <- sortCol g col
        mWrite m idx sorted
        rewire g sorted
        loopDx ref (dx + 1) m

  oneEntry m ref dx me = do
    copySingleEdge g m me (n * width) brx Nothing
    meE <- node g me
    ty0 <- do
      r <- findHigherEqual g ref (yOf meE)
      re <- node g r
      pure (yOf re)
    loopUp m ref dx me ty0
    when (not (null (yOf meE))) $ do
      dle <- node g (downL meE)
      let leftlegx = xOf dle + n * width
      col <- mRead m leftlegx
      slice <- yslice g col (yOf meE) ty0
      mapM_ (fillMagmaEdge g m me) slice

  loopUp m ref dx source ty = do
    se <- node g source
    let ur = upR se
        stop = vOf se <= 1 || ur `elem` IM.findWithDefault [] dx ents
    when (not stop) $ do
      ue <- node g ur
      let dd = dimensionDifference (yOf se) (yOf ue)
          ty2 = verticalIncrease ty dd
      copySingleEdge g m ur (n * width) brx (Just ty2)
      loopUp m ref dx ur ty2

sortCol :: Graph s -> [Int] -> ST s [Int]
sortCol g col = do
  ys <- mapM (\i -> node g i >>= \e -> pure (i, yOf e)) col
  pure (map fst (sortBy cmp ys))
 where
  cmp (_, a) (_, b) = case verticalCompare a b of
    x -> if x > 0 then LT else if x < 0 then GT else EQ

rewire :: Graph s -> [Int] -> ST s ()
rewire g col = do
  mapM_ link (zip col (drop 1 col))
  case col of
    [] -> pure ()
    (c0 : _) -> gMod g c0 (\e -> e { egValue = 1 })
  mapM_ setVal [1 .. length col - 2]
 where
  link (a, b) = do
    gMod g a (\e -> e { egDownR = Just b })
    gMod g b (\e -> e { egUpR = Just a })
  setVal i = do
    let ci = col !! i
    ce <- node g ci
    ue <- node g (upR ce)
    de <- node g (downL ue)
    gMod g ci (\e -> e { egValue = vOf ue + vOf de })

omegaYMediumNotation :: Notation
omegaYMediumNotation =
  seqNotation "omega-y-medium" "ω-Y (medium)" ["Y序列", "ω-Y", "中等", "数列"]
    (\seq t -> if null seq then [] else if seq == [maxBound :: Int] then [1, 1 + t]
               else if last seq == 1 then init seq else magmaExpand Medium seq (max 1 t))
    ""

omegaYStrongNotation :: Notation
omegaYStrongNotation =
  seqNotation "omega-y-strong" "ω-Y (strong)" ["Y序列", "ω-Y", "强", "数列"]
    (\seq t -> if null seq then [] else if seq == [maxBound :: Int] then [1, 1 + t]
               else if last seq == 1 then init seq else magmaExpand Strong seq (max 1 t))
    ""
