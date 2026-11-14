-- | G-Y 记号：忠实移植 C++ @notation/gy.hpp@。
module OmegaY.Fam.Yseq.GY
  ( expandGY
  , gYNotation
  ) where

import Control.Monad (when)
import Debug.Trace (trace)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import OmegaY.Notation.Types (Notation, seqNotation)

-- ---------------------------------------------------------------------------
-- Element
-- ---------------------------------------------------------------------------

data GEnt s = GEnt
  { gv     :: STRef s Int
  , gcol   :: STRef s Int
  , gidx   :: STRef s Int
  , grow   :: STRef s [Int]
  , gpar   :: STRef s (Maybe (GEnt s))
  , ghead :: STRef s (Maybe (GEnt s))
  , gfoot :: STRef s (Maybe (GEnt s))
  , gref  :: STRef s (Maybe (GEnt s))
  , gno    :: STRef s Int
  , gid    :: STRef s Int
  , gbs    :: STRef s Int
  , gbr    :: STRef s (Maybe (GEnt s))
  , goff   :: STRef s [Int]
  , ghasoff :: STRef s Bool
  , goffs  :: STRef s [[Int]]
  , gmaxr  :: STRef s [Int]
  , ghasmax :: STRef s Bool
  , gex    :: STRef s [Int]
  , ghasex :: STRef s Bool
  , gn     :: STRef s Int
  }

newEnt :: ST s (GEnt s)
newEnt = do
  gv <- newSTRef 0
  gcol <- newSTRef 0
  gidx <- newSTRef 0
  grow <- newSTRef []
  gpar <- newSTRef Nothing
  ghead <- newSTRef Nothing
  gfoot <- newSTRef Nothing
  gref <- newSTRef Nothing
  gno <- newSTRef 0
  gid <- newSTRef 0
  gbs <- newSTRef 0
  gbr <- newSTRef Nothing
  goff <- newSTRef []
  ghasoff <- newSTRef False
  goffs <- newSTRef []
  gmaxr <- newSTRef []
  ghasmax <- newSTRef False
  gex <- newSTRef []
  ghasex <- newSTRef False
  gn <- newSTRef 0
  pure (GEnt gv gcol gidx grow gpar ghead gfoot gref gno gid gbs gbr goff ghasoff goffs gmaxr ghasmax gex ghasex gn)

mkRoot :: ST s (GEnt s)
mkRoot = do
  e <- newEnt
  writeSTRef (grow e) [1]
  writeSTRef (gcol e) (-1)
  pure e

gV :: GEnt s -> ST s Int; gV = readSTRef . gv
gC :: GEnt s -> ST s Int; gC = readSTRef . gcol
gI :: GEnt s -> ST s Int; gI = readSTRef . gidx
gR :: GEnt s -> ST s [Int]; gR = readSTRef . grow
gP :: GEnt s -> ST s (Maybe (GEnt s)); gP = readSTRef . gpar
gH :: GEnt s -> ST s (Maybe (GEnt s)); gH = readSTRef . ghead
gF :: GEnt s -> ST s (Maybe (GEnt s)); gF = readSTRef . gfoot
gRef :: GEnt s -> ST s (Maybe (GEnt s)); gRef = readSTRef . gref
gN :: GEnt s -> ST s Int; gN = readSTRef . gno
gId :: GEnt s -> ST s Int; gId = readSTRef . gid
gBs :: GEnt s -> ST s Int; gBs = readSTRef . gbs
gBr :: GEnt s -> ST s (Maybe (GEnt s)); gBr = readSTRef . gbr
gHasOff :: GEnt s -> ST s Bool; gHasOff = readSTRef . ghasoff
gOffs :: GEnt s -> ST s [[Int]]; gOffs = readSTRef . goffs
gMaxR :: GEnt s -> ST s [Int]; gMaxR = readSTRef . gmaxr
gOff :: GEnt s -> ST s [Int]; gOff = readSTRef . goff
gHasMax :: GEnt s -> ST s Bool; gHasMax = readSTRef . ghasmax
gEx :: GEnt s -> ST s [Int]; gEx = readSTRef . gex
gHasEx :: GEnt s -> ST s Bool; gHasEx = readSTRef . ghasex
gNn :: GEnt s -> ST s Int; gNn = readSTRef . gn

sV :: GEnt s -> Int -> ST s (); sV e = writeSTRef (gv e)
sC :: GEnt s -> Int -> ST s (); sC e = writeSTRef (gcol e)
sI :: GEnt s -> Int -> ST s (); sI e = writeSTRef (gidx e)
sR :: GEnt s -> [Int] -> ST s (); sR e = writeSTRef (grow e)
sP :: GEnt s -> Maybe (GEnt s) -> ST s (); sP e = writeSTRef (gpar e)
sH :: GEnt s -> Maybe (GEnt s) -> ST s (); sH e = writeSTRef (ghead e)
sF :: GEnt s -> Maybe (GEnt s) -> ST s (); sF e = writeSTRef (gfoot e)
sRef :: GEnt s -> Maybe (GEnt s) -> ST s (); sRef e = writeSTRef (gref e)
sN :: GEnt s -> Int -> ST s (); sN e = writeSTRef (gno e)
sId :: GEnt s -> Int -> ST s (); sId e = writeSTRef (gid e)
sBs :: GEnt s -> Int -> ST s (); sBs e = writeSTRef (gbs e)
sBr :: GEnt s -> Maybe (GEnt s) -> ST s (); sBr e = writeSTRef (gbr e)
sOff :: GEnt s -> [Int] -> ST s (); sOff e = writeSTRef (goff e)
sHasOff :: GEnt s -> Bool -> ST s (); sHasOff e = writeSTRef (ghasoff e)
sOffs :: GEnt s -> [[Int]] -> ST s (); sOffs e = writeSTRef (goffs e)
sMaxR :: GEnt s -> [Int] -> ST s (); sMaxR e = writeSTRef (gmaxr e)
sHasMax :: GEnt s -> Bool -> ST s (); sHasMax e = writeSTRef (ghasmax e)
sEx :: GEnt s -> [Int] -> ST s (); sEx e = writeSTRef (gex e)
sHasEx :: GEnt s -> Bool -> ST s (); sHasEx e = writeSTRef (ghasex e)
sNn :: GEnt s -> Int -> ST s (); sNn e = writeSTRef (gn e)

-- ---------------------------------------------------------------------------
-- compareRow / rowAddition / rowDifference
-- ---------------------------------------------------------------------------

compareRow :: [Int] -> [Int] -> Maybe Int -> Int
compareRow r1 r2 midx = go 0
 where
  go i
    | i < length r1 && i < length r2 =
        if Just i == midx && r1 !! i == r2 !! i
          then 0
          else let a = r1 !! i; b = r2 !! i
                in if a > b then 1 else if a < b then (-1) else go (i + 1)
    | length r1 > i = 1
    | length r2 > i = (-1)
    | otherwise = 0

rowAddition :: [Int] -> [Int] -> [Int]
rowAddition r1 r2
  | length r2 <= 1 || (length r2 > 1 && r2 !! 1 == 1) = r1 ++ r2
  | otherwise = reverse (dropWhile (== 1) (reverse r1)) ++ r2

rowDifference :: [Int] -> [Int] -> [Int]
rowDifference r1 r2 =
  let i = findI 0
      findI k
        | k >= length r1 = k
        | k >= length r2 || r1 !! k < r2 !! k = k
        | otherwise = findI (k + 1)
      i' = if i < length r2 && r2 !! i > 1
             then let j = findJ (i - 1)
                      findJ k
                        | k < 0 = k
                        | r2 !! k > 1 = findJ (k - 1)
                        | otherwise = k
                   in if j > 0 then j else i
             else i
   in if i' >= length r2 then [] else drop i' r2

-- ---------------------------------------------------------------------------
-- toSequence / drawMountain
-- ---------------------------------------------------------------------------

toSequence :: [Int] -> ST s [GEnt s]
toSequence s = go 0 []
 where
  go i acc
    | i >= length s = pure (reverse acc)
    | otherwise = do
        let vi = s !! i
        if vi <= 1
          then do
            e <- newEnt
            sV e vi
            sC e i
            root <- mkRoot
            sP e (Just root)
            go (i + 1) (e : acc)
          else do
            let findJ j
                  | j < 0 = Nothing
                  | s !! j < vi = Just j
                  | otherwise = findJ (j - 1)
            case findJ (i - 1) of
              Nothing -> go (i + 1) acc
              Just j -> do
                e <- newEnt
                sV e vi
                sC e i
                sP e (Just (acc !! (length acc - 1 - j)))
                go (i + 1) (e : acc)

drawMountain :: [GEnt s] -> [Int] -> ST s [[GEnt s]]
drawMountain s d = do
  mRef <- newSTRef []
  mapM_ (mkFirst mRef) s
  m0 <- readSTRef mRef
  mapM_ (drawCol mRef) [0 .. length m0 - 1]
  readSTRef mRef
 where
  mkFirst mRef e = do
    eVal <- gV e
    eCol <- gC e
    ep <- gP e
    root <- newEnt
    sV root eVal
    sR root [1]
    sC root eCol
    sI root 0
    case ep of
      Nothing -> do
        fake <- mkRoot
        sP root (Just fake)
      Just p -> do
        pc <- gC p
        if pc >= 0
          then do
            mNow <- readSTRef mRef
            sP root (Just (head (mNow !! pc)))
          else do
            fake <- mkRoot
            sP root (Just fake)
    mNow <- readSTRef mRef
    writeSTRef mRef (mNow ++ [[root]])
  drawCol mRef i = do
    mNow <- readSTRef mRef
    loop mRef i (head (mNow !! i))
  loop mRef i it = do
    v <- gV it
    if v <= 1
      then pure ()
      else do
        foot <- newEnt
        itPar <- gP it
        itParVal <- maybe (pure 0) gV itPar
        sV foot (v - itParVal)
        fRow <- getFootRow it d
        sR foot fRow
        sC foot i
        itIdx <- gI it
        sI foot (itIdx + 1)
        sH foot (Just it)
        sF it (Just foot)
        mNow <- readSTRef mRef
        let col = mNow !! i
        writeSTRef mRef (take i mNow ++ [col ++ [foot]] ++ drop (i + 1) mNow)
        p <- case itPar of
          Nothing -> pure Nothing
          Just p0 -> do
            pf <- gF p0
            case pf of
              Nothing -> pure (Just p0)
              Just pfE -> do
                pfRow <- gR pfE
                fRowNow <- gR foot
                if compareRow pfRow fRowNow Nothing <= 0
                  then pure (Just pfE)
                  else pure (Just p0)
        footVal <- gV foot
        let climb mpNow = case mpNow of
              Nothing -> pure Nothing
              Just pE -> do
                pv <- gV pE
                if pv >= footVal
                  then gP pE >>= climb
                  else pure (Just pE)
        pFinal <- climb p
        sP foot pFinal
        loop mRef i foot

-- ---------------------------------------------------------------------------
-- getFootRow
-- ---------------------------------------------------------------------------

getFootRow :: GEnt s -> [Int] -> ST s [Int]
getFootRow it d = do
  itRow <- gR it
  itPar <- gP it
  pRow <- maybe (pure [1]) gR itPar
  if compareRow itRow pRow Nothing == 0
    then pure (itRow ++ [1])
    else do
      let loop row i = do
            exRaw <- expandImpl row i d False
            let ex = if null exRaw then [] else init exRaw
            let c = compareRow ex itRow Nothing
            pRowNow <- maybe (pure [1]) gR itPar
            if c == 0 || (c < 0 && compareRow pRowNow ex Nothing < 0)
              then pure row
              else if c < 0
                then loop row (i + 1)
                else do
                  let midx = length itRow - 1
                  if compareRow ex itRow (Just midx) == 0
                    then if length ex > length itRow + 1
                           then pure (take (length itRow + 1) ex)
                           else pure ex
                    else do
                      let j = findJ 1
                          findJ k
                            | k < length ex && k < length itRow && ex !! k <= itRow !! k = findJ (k + 1)
                            | otherwise = k
                          newRow0 = take j ex
                          newRow1 = if j < length itRow
                                      then newRow0 ++ [itRow !! j + 1]
                                      else newRow0 ++ [1]
                      loop newRow1 1
      loop [1, itRow !! 1 + 1] 1

-- ---------------------------------------------------------------------------
-- setElementRefrence / setElementNo / getTpElement / getBootIndex
-- ---------------------------------------------------------------------------

setElementRefrence :: [[GEnt s]] -> ST s ()
setElementRefrence m = mapM_ (mapM_ one) m
 where
  one e = do
    row <- gR e
    v <- gV e
    if length row <= 1 && v <= 1
      then do
        fake <- mkRoot
        sRef e (Just fake)
      else do
        eRow <- gR e
        p <- gP e
        pRow <- maybe (pure [1]) gR p
        if compareRow eRow pRow Nothing == 0
          then sRef e p
          else do
            h <- gH e
            sRef e =<< maybe (pure Nothing) gP h

setElementNo :: [[GEnt s]] -> GEnt s -> ST s ()
setElementNo m b = do
  idRef <- newSTRef 0
  bCol <- gC b
  mapM_ (one idRef bCol) (zip [0 ..] m)
 where
  one idRef bCol (i, col) = mapM_ (oneE idRef bCol i) (zip [0 ..] col)
  oneE idRef bCol i (j, e) = do
    if i == bCol
      then sN e (j + 1)
      else if i < bCol
        then sN e 0
        else do
          v <- gV e
          if v <= 1 && j == 0
            then sN e 0
            else do
              r <- gRef e
              case r of
                Nothing -> sN e 0
                Just rE -> sN e =<< gN rE
    when (i > bCol) $ do
      cur <- readSTRef idRef
      sId e cur
      writeSTRef idRef (cur + 1)
    when (i == bCol || i == length m - 1) $ do
      nn <- gN e
      sId e nn

getTpElement :: [[GEnt s]] -> GEnt s -> Int -> ST s (GEnt s)
getTpElement m t no = do
  tCol <- gC t
  let loop tp = do
        tpNo <- gN tp
        tpFoot <- gF tp
        if tpNo <= no
          then case tpFoot of
            Nothing -> pure tp
            Just fE -> loop fE
          else pure tp
  loop (head (m !! tCol))

getBootIndex :: [Int] -> [Int] -> ST s Int
getBootIndex s d = do
  seq <- toSequence s
  m <- drawMountain seq d
  if length (last m) < 2
    then pure (-1)
    else do
      let e = last m !! (length (last m) - 2)
      ep <- gP e
      maybe (pure (-1)) gC ep

-- ---------------------------------------------------------------------------
-- expandDimensionSequnece
-- ---------------------------------------------------------------------------

expandDimensionSequnece :: [[GEnt s]] -> GEnt s -> GEnt s -> Int -> [Int] -> ST s ()
expandDimensionSequnece m b t n d
  | n <= 0 = pure ()
  | otherwise = do
      bCol <- gC b
      bIdx <- gI b
      mapM_ (\i -> do
                let e = (m !! bCol) !! i
                sBs e 0
                sBr e (Just e)) [0 .. bIdx]
      tCol <- gC t
      mapM_ (pass1 bCol tCol) [bCol + 1 .. tCol]
      mapM_ (pass2 bCol tCol) [bCol + 1 .. tCol]
 where
  pass1 bCol tCol i = mapM_ (one1 bCol tCol i) [0 .. length (m !! i) - 1]
  one1 bCol tCol i j = do
    let it = (m !! i) !! j
    tp <- getTpElement m t =<< gN it
    itNo <- gN it
    if itNo < 0
      then do
        sBs it 0
        sBr it (Just it)
      else do
        hasEx <- gHasEx tp
        when (not hasEx) $ do
          tpRow <- gR tp
          ex1 <- expandImpl tpRow n d False
          sEx tp ex1
          sHasEx tp True
          sNn tp n
        let loop = do
              tpRow <- gR tp
              itRow <- gR it
              tpEx <- gEx tp
              if not (null tpRow) && last tpRow > 1
                   && compareRow tpRow itRow Nothing > 0
                   && compareRow tpEx itRow Nothing < 0
                then do
                  curN <- gNn tp
                  sNn tp (curN + 1)
                  tpRow2 <- gR tp
                  ex2 <- expandImpl tpRow2 (curN + 1) d False
                  sEx tp ex2
                  loop
                else pure ()
        loop
        itRow <- gR it
        tpRow <- gR tp
        idx <- getBootIndex tpRow d   -- 对齐 C++：getBootIndex(tp->row, …)
        tpEx <- gEx tp
        let midx = length itRow - 1
        if idx >= 0
             && compareRow itRow tpEx (Just midx) == 0
             && length tpRow > 1
             && ((length tpEx - 1 - length itRow) `mod` (length tpRow - 1 - idx)) == 0
          then do
            r <- gRef it
            case r of
              Nothing -> sBs it 0
              Just rE -> sBs it . (+ 1) =<< gBs rE
            sBr it (Just it)
          else do
            r <- gRef it
            case r of
              Nothing -> do
                sBs it 0
                sBr it (Just it)
              Just rE -> do
                sBs it =<< gBs rE
                sBr it =<< gBr rE
  pass2 bCol tCol i = mapM_ (one2 bCol tCol i) [0 .. length (m !! i) - 1]
  one2 bCol tCol i j = do
    let it = (m !! i) !! j
    itNo <- gN it
    if itNo <= 0
      then do
        itRow <- gR it
        sMaxR it itRow
        sHasMax it True
      else do
        itRef <- gRef it
        itRow <- gR it
        refRow <- maybe (pure [1]) gR itRef
        if compareRow (rowDifference refRow itRow) [2] Nothing < 0
          then do
            sOff it (rowDifference refRow itRow)
            sHasOff it True
          else do
            tp <- getTpElement m t =<< gN it
            tpRow <- gR tp
            if compareRow itRow tpRow Nothing >= 0
              then do
                sMaxR it itRow
                sHasMax it True
              else do
                itRowNow <- gR it
                tpRowNow <- gR tp
                tpHead <- gH tp
                tpHeadBs <- maybe (pure 0) gBs tpHead
                curN <- gNn tp
                let needed = itNo + tpHeadBs * n + length itRowNow - length tpRowNow
                when (curN < needed) $ do
                  sNn tp needed
                  tpRow2 <- gR tp
                  ex2 <- expandImpl tpRow2 needed d False
                  sEx tp ex2
                tpEx <- gEx tp
                let midx = length itRowNow - 1
                if compareRow itRowNow tpEx (Just midx) == 0 && compareRow itRowNow refRow Nothing > 0
                  then do
                    sMaxR it tpEx
                    sHasMax it True
                  else do
                    is <- toSequence itRowNow
                    ts <- toSequence tpRowNow
                    idx <- getBootIndex tpRowNow d
                    if idx < 0 || length tpRowNow <= idx + 1
                      then pure ()
                      else do
                        let headPart = take (idx + 1) tpRowNow
                        if compareRow itRowNow headPart Nothing <= 0
                          then do
                            sOff it (rowDifference refRow itRowNow)
                            sHasOff it True
                          else do
                            when (length is > idx && length ts > idx + 1) $ do
                              sP (ts !! (idx + 1)) (Just (is !! idx))
                              sRef (ts !! (idx + 1)) (Just (is !! idx))
                            let seq = is ++ drop (idx + 1) ts
                            mapM_ (\(k, e) -> sC e k) (zip [0 ..] seq)
                            ex <- expandElements seq (n * tpHeadBs + itNo) d False
                            sOffs it []
                            itBs <- gBs it
                            tpHeadBr <- maybe (pure Nothing) gBr tpHead
                            brColMatch <- case tpHeadBr of
                              Nothing -> pure False
                              Just br -> do
                                brC <- gC br
                                tC <- gC t
                                pure (brC == tC)
                            si <- if itBs > 0 || brColMatch
                                    then pure (length refRow)
                                    else pure 0
                            bRow <- gR b
                            let si' = if itBs > 0 || brColMatch then si else length bRow
                            let step = length seq - 1 - idx
                            let offsets = map (\k ->
                                  let start0 = si' + k * step * tpHeadBs
                                      end0 = length is + k * step * tpHeadBs
                                      start = max 0 start0
                                      end' = min (length ex) end0
                                   in if start < end' then take (end' - start) (drop start ex) else [])
                                  [1 .. n]
                            sOffs it offsets

-- ---------------------------------------------------------------------------
-- copyElement / copyCloumn
-- ---------------------------------------------------------------------------

chaseFoot :: GEnt s -> Int -> ST s (GEnt s)
chaseFoot r rid = do
  rf <- gF r
  case rf of
    Nothing -> pure r
    Just rfE -> do
      rfid <- gId rfE
      if rfid <= rid then chaseFoot rfE rid else pure r

copyElementImpl :: [[GEnt s]] -> GEnt s -> GEnt s -> GEnt s
                -> [GEnt s] -> Int -> [Int] -> Bool -> ST s [GEnt s]
copyElementImpl m b t it op i d f = do
  pt <- gP it
  itRow <- gR it
  when (length itRow > 1) $ do
    h <- gH it
    case h of
      Nothing -> pure ()
      Just hE -> do
        hp <- gP hE
        sP it hp
  itPar <- gP it
  case itPar of
    Nothing -> pure ()
    Just pE -> do
      pf <- gF pE
      case pf of
        Nothing -> pure ()
        Just pfE -> do
          pfRow <- gR pfE
          itRowNow <- gR it
          when (compareRow pfRow itRowNow Nothing <= 0) $ sP it (Just pfE)
  let climb = do
        itParNow <- gP it
        case itParNow of
          Nothing -> pure ()
          Just pE -> do
            pv <- gV pE
            itVal <- gV it
            when (pv > itVal) $ do
              sP it =<< gP pE
              climb
  climb
  -- 对齐 C++：判据是 it->row.size() > 1，且对 op.back() 求 getFootRow
  minRow <- if length itRow <= 1
    then pure [1]
    else case op of
      [] -> pure [1]
      _ -> getFootRow (last op) d
  maxRow0 <- gR it
  itCol <- gC it
  tp0 <- getTpElement m t =<< gN it
  tpNo <- gN tp0
  itNo <- gN it
  tp <- if tpNo > itNo
          then maybe (pure tp0) pure =<< gH tp0
          else pure tp0
  hasMax <- gHasMax it
  maxRow1 <-
    if hasMax
      then gMaxR it
      else do
        itRef <- gRef it
        hasOff <- gHasOff it
        tpBr <- gBr tp
        itBs <- gBs it
        bCol <- gC b
        tCol <- gC t
        itCol <- gC it
        brMatch <- case tpBr of
          Nothing -> pure False
          Just br -> do
            brC <- gC br
            pure (brC == tCol)
        if hasOff || brMatch || itBs > 0
          then do
            refCol <- maybe (pure 0) gC itRef
            let c = refCol + (tCol - bCol) * (i + 1)
            if c >= 0 && c < length m
              then do
                let r0 = head (m !! c)
                rid <- maybe (pure 0) gId itRef
                r <- chaseFoot r0 rid
                off <- gOff it
                offs <- gOffs it
                if hasOff
                  then do
                    rRow <- gR r
                    pure (rowAddition rRow off)
                  else if i < length offs
                    then do
                      rRow <- gR r
                      pure (rRow ++ (offs !! i))
                    else pure maxRow0
              else pure maxRow0
          else do
            offs <- gOffs it
            tpBrNow <- gBr tp
            case tpBrNow of
              Nothing -> pure maxRow0
              Just br -> do
                if null offs
                  then pure maxRow0
                  else do
                    brRow <- gR br
                    tpRow <- gR tp
                    tCol <- gC t
                    bCol <- gC b
                    if compareRow (head offs) (rowDifference brRow tpRow) Nothing <= 0
                      then do
                        let c = tCol + (tCol - bCol) * i
                        rid <- gId tp
                        if c >= 0 && c < length m
                          then do
                            let r0 = head (m !! c)
                            r <- chaseFoot r0 rid
                            rRow <- gR r
                            pure (rRow ++ (offs !! i))
                          else pure maxRow0
                      else do
                        brCol <- gC br
                        let c = brCol + (tCol - bCol) * (i + 1)
                        rid <- gId br
                        if c >= 0 && c < length m
                          then do
                            let r0 = head (m !! c)
                            r <- chaseFoot r0 rid
                            rRow <- gR r
                            pure (rRow ++ (offs !! i))
                          else pure maxRow0
  -- 循环生成
  let loop row opAcc
        | compareRow row maxRow1 Nothing > 0 = pure opAcc
        | otherwise = do
            itRowNow <- gR it
            when (compareRow row maxRow1 (Just (length row - 1)) == 0) $
              sP it pt
            e <- newEnt
            itValNow <- gV it
            sV e itValNow
            sR e row
            sC e (length m - 1)
            sI e (length opAcc)
            itNoNow <- gN it
            sN e itNoNow
            itIdNow <- gId it
            sId e itIdNow
            let opAcc' = opAcc ++ [e]
            case opAcc of
              [] -> pure ()
              _ -> do
                let prev = last opAcc
                sH e (Just prev)
                sF prev (Just e)
            itParNow <- gP it
            itParCol <- maybe (pure (-1)) gC itParNow
            bCol <- gC b
            let pc = if itParCol >= bCol
                       then length m - 1 + itParCol - itCol
                       else itParCol
            p <- if pc >= 0 && pc < length m
                   then pure (last (m !! pc))
                   else mkRoot
            let climbP pE = do
                  pRow <- gR pE
                  if compareRow pRow row Nothing > 0
                    then gH pE >>= \mh -> case mh of
                      Nothing -> pure pE
                      Just hE -> climbP hE
                    else pure pE
            pFinal <- climbP p
            sP e (Just pFinal)
            rowNext <- getFootRow e d
            loop rowNext opAcc'
  loop minRow op

copyCloumnImpl :: [[GEnt s]] -> GEnt s -> GEnt s -> Int -> Int
               -> [Int] -> [Int] -> Bool -> ST s [[GEnt s]]
copyCloumnImpl m b t c i ex d f = do
  let it0 = head (m !! c)
  let m' = m ++ [[]]
  newColRef <- newSTRef []
  itRef <- newSTRef it0
  let loop = do
        it <- readSTRef itRef
        opNow <- readSTRef newColRef
        opNew <- copyElementImpl m' b t it opNow i d f
        writeSTRef newColRef opNew
        itFoot <- gF it
        case itFoot of
          Nothing -> pure ()
          Just fE -> do
            writeSTRef itRef fE
            loop
  loop
  newCol <- readSTRef newColRef
  itEnd <- readSTRef itRef
  itVal <- gV itEnd
  sV (last newCol) itVal
  let fixVal e = do
        mh <- gH e
        case mh of
          Nothing -> pure ()
          Just hE -> do
            hPar <- gP hE
            case hPar of
              Nothing -> pure ()
              Just hParE -> do
                hParVal <- gV hParE
                eValNow <- gV e
                sV hE (eValNow + hParVal)
            fixVal hE
  fixVal (last newCol)
  pure (take (length m' - 1) m' ++ [newCol])

-- ---------------------------------------------------------------------------
-- expand_elements / expand_impl
-- ---------------------------------------------------------------------------

expandElements :: [GEnt s] -> Int -> [Int] -> Bool -> ST s [Int]
expandElements s n d f = do
  if null s
    then pure []
    else do
      lastVal <- gV (last s)
      if lastVal <= 1
        then mapM gV (take (length s - 1) s)
        else do
          m0 <- drawMountain s d
          let t0 = last (last m0)
          t0Val <- gV t0
          t <- if t0Val == 1
                 then maybe (pure t0) pure =<< gH t0
                 else pure t0
          bMay <- gP t
          let b = case bMay of { Just bE -> bE; Nothing -> error "gy: null b" }
          setElementRefrence m0
          setElementNo m0 b
          expandDimensionSequnece m0 b t n d
          -- if t.foot: pop back
          tFoot <- gF t
          m1 <- case tFoot of
            Nothing -> pure m0
            Just _ -> do
              let col = last m0
                  m0' = take (length m0 - 1) m0 ++ [take (length col - 1) col]
              sF t Nothing
              mbPar <- gP t
              case mbPar of
                Nothing -> pure ()
                Just p -> gP p >>= sP t
              pure m0'
          let lastCol = last m1
          mapM_ (\e -> gV e >>= sV e . subtract 1) lastCol
          bNo <- gN b
          bCol <- gC b
          tCol <- gC t
          tIdx <- gI t
          let copyRange i mNow
                | i >= length (mNow !! bCol) = pure mNow
                | otherwise = do
                    let src = (mNow !! bCol) !! i
                    srcVal <- gV src
                    srcRow <- gR src
                    srcNo <- gN src
                    srcPar <- gP src
                    e <- newEnt
                    sV e srcVal
                    sR e srcRow
                    sC e tCol
                    sMaxR e srcRow
                    sHasMax e True
                    sI e (tIdx + 1)
                    sN e srcNo
                    sId e srcNo
                    sP e srcPar
                    sH e (Just (last (mNow !! tCol)))
                    sRef e (Just src)
                    let col = mNow !! tCol
                    sF (last col) (Just e)
                    sI t (tIdx + 1)
                    copyRange (i + 1)
                      (take tCol mNow ++ [col ++ [e]] ++ drop (tCol + 1) mNow)
          m2 <- copyRange bNo m1
          let loopN i mNow
                | i >= n = pure mNow
                | otherwise = do
                    let loopJ j mNow2
                          | j > tCol = pure mNow2
                          | otherwise = do
                              mNow3 <- copyCloumnImpl mNow2 b t j i [] d f
                              loopJ (j + 1) mNow3
                    mNow' <- loopJ (bCol + 1) mNow
                    loopN (i + 1) mNow'
          mFinal <- loopN 0 m2
          mapM (\col -> if null col then pure 0 else gV (head col)) mFinal

expandImpl :: [Int] -> Int -> [Int] -> Bool -> ST s [Int]
expandImpl seq n d f = do
  s <- toSequence seq
  expandElements s n d f

expandGY :: [Int] -> Int -> [Int]
expandGY seq term
  | null seq || term < 1 = seq
  | otherwise = runST $ expandImpl seq term [0] True

gYNotation :: Notation
gYNotation =
  seqNotation "g-y" "G-Y" ["Y序列", "G-Y"] expandGY ""
