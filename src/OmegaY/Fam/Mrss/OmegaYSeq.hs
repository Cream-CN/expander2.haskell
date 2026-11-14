-- | omega-y-sequence（ω-Y sequence）：忠实移植 C++ @notation/omega_y.hpp@
--   的 @OmegaYNotation::omega_Y_limit@。
--
--   与已实现的 omega-y-medium/strong（@Omega-YMagma.hpp@）是两套不同算法：
--   本模块含原实现的全部防护分支（空列 / BR 缺失 / width<=0 / ref 为空 break）、
--   INT_MAX 幽灵节点、collect_usual/collect1D 收集规则，以及 copy_single_edge
--   的 lx<0 提前返回。SequenceNotation 适配器只绑定 T::expand，故 alter 模式
--   与 fs 走同一 expand，仅不再额外 pop 末元素（由共享 SeqCaps 实现）。
module OmegaY.Fam.Mrss.OmegaYSeq
  ( omegaYSeqNotation
  , omegaYMediumNotation
  , omegaYStrongNotation
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import OmegaY.EntryGraph hiding (verticalIncrease)
import OmegaY.Notation.Types (Notation, seqNotation)

-- C++ INT_MAX（32 位）；phantom 节点的值。
kInfinity :: Int
kInfinity = 2147483647

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

-- | 把山扩容到至少 n 列（空列补 []）。
mResize :: Mtn s -> Int -> ST s ()
mResize (Mtn r) n = do
  m <- readSTRef r
  let go i
        | i >= n = pure ()
        | IM.member i m = go (i + 1)
        | otherwise = do
            writeSTRef r (IM.insert i [] m)
            go (i + 1)
  go 0

pushCol :: Mtn s -> Int -> Int -> ST s ()
pushCol m i v = mWrite m i =<< (++ [v]) <$> mRead m i

node :: Graph s -> Int -> ST s EntryG
node g i = gGetDef g i

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

-- | 对齐 C++ omega_y.hpp::vertical_increase（不截断 d 之后的元素）。
verticalIncrease :: [Int] -> Int -> [Int]
verticalIncrease y d0 =
  let d = max 0 d0
      padded = if length y <= d then y ++ replicate (d - length y + 1) 0 else y
      cur = padded !! d
      newD = if cur == 0 then 1 else cur + 1
   in replicate d 0 ++ [newD] ++ drop (d + 1) padded

sameRow :: EntryG -> EntryG -> Bool
sameRow a b = verticalCompare (egY a) (egY b) == 0

-- | 严格 same_row（Omega-YMagma.hpp：dimension_difference == 0）。
--   两个空 y 不视为同行（dd=-1≠0），与 sequence 的 verticalCompare==0 不同。
sameRowStrict :: EntryG -> EntryG -> Bool
sameRowStrict a b = dimensionDifference (egY a) (egY b) == 0

-- ---------------------------------------------------------------------------
-- from_sequence / to_sequence（phantom 值为 INT_MAX，对齐 omega_y.hpp）
-- ---------------------------------------------------------------------------

fromSequence :: Int -> Graph s -> Mtn s -> [Int] -> ST s ()
fromSequence phantomVal g m seq = go 0
 where
  go i
    | i >= length seq = pure ()
    | otherwise = do
        bottom <- newNode g (seq !! i) i [1]
        phantom <- newNode g phantomVal i []
        gMod g bottom (\e -> e { egDownR = Just phantom })
        gMod g phantom (\e -> e { egUpR = Just bottom })
        when (i > 0) $ do
          prevPhantom <- (!! 1) <$> mRead m (i - 1)
          gMod g bottom (\e -> e { egDownL = Just prevPhantom })
          pushUpL g prevPhantom bottom
        mWrite m i [bottom, phantom]
        go (i + 1)

toSequenceG :: Graph s -> Mtn s -> ST s [Int]
toSequenceG g m = do
  n <- mLen m
  mapM pick [0 .. n - 1]
 where
  pick i = do
    col <- mRead m i
    if length col < 2
      then pure 0
      else vOf <$> node g (col !! (length col - 2))

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
    loop col0
   where
    loop col = case col of
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

  ascendParent entry ee = loopFrom (downL ee) entry
   where
    loopFrom up parentId
      | up < 0 = pure parentId
      | otherwise = do
          pE <- node g parentId
          up0 <- climb up (yOf pE)
          ue <- node g up0
          if vOf ue < vOf ee
            then pure up0
            else loopFrom (downL ue) up0
    climb up py
      | up < 0 = pure up
      | otherwise = do
          ue <- node g up
          let ur = upR ue
          if ur >= 0
            then do
              ure <- node g ur
              if verticalCompare (yOf ure) py <= 0 then climb ur py else pure up
            else pure up

-- ---------------------------------------------------------------------------
-- 列内二分
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- collect_usual / collect1D / collect（对齐 omega_y.hpp）
-- ---------------------------------------------------------------------------

contains :: [Int] -> Int -> Bool
contains = flip elem

collectUsual :: Graph s -> (EntryG -> EntryG -> Bool) -> Int -> [Int] -> ST s [Int]
collectUsual g sameR wid acc
  | wid < 0 = pure acc
  | otherwise = do
      we <- node g wid
      foldl' acc (upLs we)
 where
  foldl' acc [] = pure acc
  foldl' acc (e : es) = do
    if e < 0
      then foldl' acc es
      else do
        ee <- node g e
        let child = downR ee
        if child < 0 || child `elem` acc
          then foldl' acc es
          else do
            ce <- node g child
            weE <- node g wid
            if sameR weE ce
              then do
                acc' <- collectUsual g sameR child (acc ++ [child])
                foldl' acc' es
              else foldl' acc es

collect1D :: Graph s -> (EntryG -> EntryG -> Bool) -> Int -> [Int] -> ST s [Int]
collect1D g sameR wid acc
  | wid < 0 = pure acc
  | otherwise = do
      we <- node g wid
      let dr = downR we
      if dr < 0
        then pure acc
        else do
          de <- node g dr
          foldl' wid acc (upLs de)
 where
  foldl' wid acc [] = pure acc
  foldl' wid acc (e : es) = do
    if e < 0 || e `elem` acc
      then foldl' wid acc es
      else do
        ee <- node g e
        weE <- node g wid
        if sameR weE ee
          then do
            acc' <- collect1D g sameR e (acc ++ [e])
            foldl' wid acc' es
          else foldl' wid acc es

collectStrong :: Graph s -> (EntryG -> EntryG -> Bool) -> Int -> [Int] -> ST s [Int]
collectStrong g sameR wid acc
  | wid < 0 = pure acc
  | otherwise = do
      we <- node g wid
      let dr = downR we
      if dr < 0
        then pure acc
        else do
          de <- node g dr
          foldl' acc (upLs de)
 where
  foldl' acc [] = pure acc
  foldl' acc (e : es) = do
    if e < 0 || e `elem` acc
      then foldl' acc es
      else do
        ee <- node g e
        weE <- node g wid
        if sameR weE ee
          then do
            acc' <- collectStrong g sameR e (acc ++ [e])
            foldl' acc' es
          else foldl' acc es

collect :: Kind -> Graph s -> (EntryG -> EntryG -> Bool) -> Int -> ST s [Int]
collect kind g sameR wid
  | wid < 0 = pure []
  | kind == KindStrong = collectStrong g sameR wid []
  | otherwise = do
      we <- node g wid
      let dr = downR we
      use1D <-
        if verticalCompare (yOf we) [1] > 0 && dr >= 0
          then do
            de <- node g dr
            pure (dimensionDifference (yOf we) (yOf de) == 0)
          else pure False
      if use1D
        then collect1D g sameR wid []
        else collectUsual g sameR wid []

-- ---------------------------------------------------------------------------
-- fill_magma_edge / copy_single_edge（omega_y.hpp 版，含 lx<0 提前返回）
-- ---------------------------------------------------------------------------

fillMagmaEdge :: Graph s -> Mtn s -> Int -> Int -> ST s ()
fillMagmaEdge g m source ll
  | source < 0 || ll < 0 = pure ()
  | otherwise = do
      se <- node g source
      let dle = downL se
      if dle < 0
        then pure ()
        else do
          le <- node g ll
          let lue = upR le
          if lue < 0
            then pure ()
            else do
              dleE <- node g dle
              leE <- node g ll
              lueE <- node g lue
              let targetx = xOf se - xOf dleE + xOf leE
              if targetx < 0
                then pure ()
                else do
                  mResize m (targetx + 1)
                  let dd = dimensionDifference (yOf leE) (yOf lueE)
                  mapM_ (mk targetx ll) [dd, dd - 1 .. 0]
 where
  mk targetx ll d = do
    leE <- node g ll
    nid <- newNode g 0 targetx (verticalIncrease (yOf leE) d)
    gMod g nid (\e -> e { egDownL = Just ll })
    pushUpL g ll nid
    pushCol m targetx nid

copySingleEdge :: Graph s -> Mtn s -> Int -> Int -> Int -> Maybe [Int] -> ST s ()
copySingleEdge g m source off brx mty
  | source < 0 = pure ()
  | otherwise = do
      se <- node g source
      let ty = maybe (yOf se) id mty
          tx = xOf se + off
      if tx < 0
        then pure ()
        else do
          mResize m (tx + 1)
          nid <- newNode g 0 tx ty
          if null (yOf se) || downL se < 0
            then pushCol m tx nid
            else do
              let dle = downL se
              dleE <- node g dle
              let lx = xOf dleE + off
              if lx < 0
                then pushCol m tx nid
                else do
                  mResize m (lx + 1)
                  leftlegEntry <-
                    if xOf dleE >= brx
                      then do
                        col <- mRead m lx
                        findLower g col ty
                      else pure dle
                  if leftlegEntry < 0
                    then pushCol m tx nid
                    else do
                      gMod g nid (\e -> e { egDownL = Just leftlegEntry })
                      pushUpL g leftlegEntry nid
                      pushCol m tx nid

-- ---------------------------------------------------------------------------
-- omega_Y_limit 主体
-- ---------------------------------------------------------------------------

omegaYLimitKind :: Kind -> Graph s -> Mtn s -> [Int] -> Int -> ST s [Int]
omegaYLimitKind kind g m0 seq fsTerm = omegaYLimit kind g m0 seq fsTerm

omegaYLimit :: Kind -> Graph s -> Mtn s -> [Int] -> Int -> ST s [Int]
omegaYLimit kind g m0 seq fsTerm = do
  let phantomVal = if kind == KindSeq then kInfinity else 0
  fromSequence phantomVal g m0 seq
  drawMountain g m0
  n0 <- mLen m0
  if n0 == 0
    then pure seq
    else do
      childCol <- mRead m0 (n0 - 1)
      if null childCol
        then pure seq
        else do
          let childTop = head childCol
          cE <- node g childTop
          let br = downL cE
          if br < 0
            then pure seq
            else do
              brE <- node g br
              let brx = xOf brE
                  width = n0 - 1 - brx
              if brx < 0 || brx >= n0 || width <= 0
                then pure seq
                else do
                  brCol <- mRead m0 brx
                  case elemIndex br brCol of
                    Nothing -> pure seq
                    Just mi ->
                      if mi + 1 >= length brCol
                        then pure seq
                        else do
                          let top = childTop : take (length brCol - mi - 1) (drop mi brCol)
                              s = init seq ++ [last seq - 1]
                          m1 <- newMtn
                          fromSequence phantomVal g m1 s
                          drawMountain g m1
                          n1 <- mLen m1
                          if brx < 0 || brx >= n1
                            then pure seq
                            else do
                              col2 <- mRead m1 brx
                              br2 <- findSameRow g col2 br
                              if br2 < 0
                                then pure seq
                                else do
                                  magma <- collectMagma kind g m1 brx br2 width
                                  mFinal <- loopN g m1 magma top brx width fsTerm 1
                                  toSequenceG g mFinal

findSameRow :: Graph s -> [Int] -> Int -> ST s Int
findSameRow g col ref = do
  re <- node g ref
  go re col
 where
  go _ [] = pure ref
  go re (e : es) = do
    ee <- node g e
    if sameRow ee re then pure e else go re es

collectMagma :: Kind -> Graph s -> Mtn s -> Int -> Int -> Int -> ST s (IntMap [Int])
collectMagma kind g m brx br width = do
  let acc0 = IM.fromList [(d, []) | d <- [0 .. width]]
  go acc0 br
 where
  go acc br1
    | br1 < 0 = pure acc
    | otherwise = do
        be <- node g br1
        if kind == KindStrong && null (yOf be)
          then do
            -- phantom branch: push col.back() for each column to the right
            n <- mLen m
            let backFill acc dx1
                  | brx + 1 + dx1 >= n = pure acc
                  | otherwise = do
                      col <- mRead m (brx + 1 + dx1)
                      case col of
                        [] -> backFill acc (dx1 + 1)
                        _ -> backFill (IM.adjust (++ [last col]) (dx1 + 1) acc) (dx1 + 1)
            acc' <- backFill acc 0
            pure acc'
          else do
            coll <- collect kind g sameRow br1
            acc' <- foldlM' add acc coll
            let nxt = downR be
            if null (yOf be)
              then pure acc'
              else if nxt < 0 then pure acc' else go acc' nxt
  add acc e = do
    ee <- node g e
    let dx = xOf ee - brx
    if dx > 0 && dx <= width
      then pure (IM.adjust (++ [e]) dx acc)
      else pure acc
  foldlM' f z [] = pure z
  foldlM' f z (x : xs) = f z x >>= \z' -> foldlM' f z' xs

loopN :: Graph s -> Mtn s -> IntMap [Int] -> [Int] -> Int -> Int -> Int -> Int -> ST s (Mtn s)
loopN g m ents top brx width term n
  | n > term = pure m
  | otherwise = do
      lastColN <- mLen m
      lastCol <- mRead m (lastColN - 1)
      rawRef <- mapM one top
      let ref = [r | r <- rawRef, r >= 0]
      if null ref
        then pure m
        else do
          m' <- loopDx ref 1 m
          loopN g m' ents top brx width term (n + 1)
 where
  one t = do
    te <- node g t
    n <- mLen m
    lastColNow <- mRead m (n - 1)
    findLower g lastColNow (yOf te)

  loopDx ref dx m
    | dx > width = pure m
    | otherwise = do
        let colx = brx + n * width + dx
        if colx < 0
          then loopDx ref (dx + 1) m
          else do
            mResize m (colx + 1)
            mWrite m colx []
            if dx >= IM.size ents
              then loopDx ref (dx + 1) m
              else do
                let entries = IM.findWithDefault [] dx ents
                mapM_ (oneEntry ref dx) entries
                col <- mRead m colx
                sorted <- sortByY g col
                mWrite m colx sorted
                rewire g sorted
                loopDx ref (dx + 1) m

  oneEntry ref dx me = do
    copySingleEdge g m me (n * width) brx Nothing
    meE <- node g me
    he <- findHigherEqual g ref (yOf meE)
    if he < 0
      then pure ()
      else do
        heE <- node g he
        let targety0 = yOf heE
        loopUp dx me targety0
        when (not (null (yOf meE)) && downL meE >= 0) $ do
          let dle = downL meE
          dleE <- node g dle
          let leftlegx = xOf dleE + n * width
          nNow <- mLen m
          if leftlegx < 0 || leftlegx >= nNow
            then pure ()
            else do
              col <- mRead m leftlegx
              slice <- yslice g col (yOf meE) targety0
              mapM_ (fillMagmaEdge g m me) slice

  loopUp dx source targety
    | source < 0 = pure ()
    | otherwise = do
        se <- node g source
        let ur = upR se
            stop = vOf se <= 1 || ur `elem` IM.findWithDefault [] dx ents
        when (not stop && ur >= 0) $ do
          ue <- node g ur
          let dd = dimensionDifference (yOf se) (yOf ue)
              ty2 = verticalIncrease targety dd
          copySingleEdge g m ur (n * width) brx (Just ty2)
          loopUp dx ur ty2

sortByY :: Graph s -> [Int] -> ST s [Int]
sortByY g col = do
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
    let up = upR ce
    if up < 0
      then pure ()
      else do
        ue <- node g up
        let dle = downL ue
        if dle < 0
          then pure ()
          else do
            dleE <- node g dle
            gMod g ci (\e -> e { egValue = vOf ue + vOf dleE })

-- ---------------------------------------------------------------------------
-- 公开 expand
-- ---------------------------------------------------------------------------

data Kind = KindSeq | KindMedium | KindStrong deriving Eq

expandOmegaY :: Kind -> [Int] -> Int -> [Int]
expandOmegaY kind seq term
  | null seq = []
  | last seq == 1 = init seq
  | length seq == 1 = seq
  | otherwise = runST $ do
      g <- newGraph
      m0 <- newMtn
      let eff = if kind == KindSeq then term + length seq else term
      full <- omegaYLimitKind kind g m0 seq eff
      pure (if kind == KindSeq then (if length full < 2 then full else init full) else full)

expandOmegaYSeq :: [Int] -> Int -> [Int]
expandOmegaYSeq = expandOmegaY KindSeq

expandOmegaYMedium :: [Int] -> Int -> [Int]
expandOmegaYMedium = expandOmegaY KindMedium

expandOmegaYStrong :: [Int] -> Int -> [Int]
expandOmegaYStrong = expandOmegaY KindStrong

omegaYSeqNotation :: Notation
omegaYSeqNotation =
  seqNotation "omega-y-sequence" "ω-Y sequence" ["Y序列", "ω-Y", "数列"]
    expandOmegaYSeq ""

omegaYMediumNotation :: Notation
omegaYMediumNotation =
  seqNotation "omega-y-medium" "ω-Y (medium)" ["Y序列", "ω-Y", "中等"]
    expandOmegaYMedium ""

omegaYStrongNotation :: Notation
omegaYStrongNotation =
  seqNotation "omega-y-strong" "ω-Y (strong)" ["Y序列", "ω-Y", "强"]
    expandOmegaYStrong ""
