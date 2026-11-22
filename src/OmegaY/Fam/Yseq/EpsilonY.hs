-- | ε-Y 记号：忠实移植 C++ @notation/epsilon-y.hpp@。
module OmegaY.Fam.Yseq.EpsilonY
  ( expandEpsilonY
  , epsilonYNotation
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import OmegaY.Notation.Types (Notation, seqNotation)

-- Entry 与 arena（对齐 epsilon_y_detail::Entry / Arena）

data EEnt s = EEnt
  { ev    :: STRef s Int
  , er    :: STRef s [Int]
  , ec    :: STRef s Int   -- cloumn
  , ei    :: STRef s Int   -- idx
  , en    :: STRef s Int   -- no
  , eid   :: STRef s Int   -- id
  , epar  :: STRef s (Maybe (EEnt s))
  , ehead :: STRef s (Maybe (EEnt s))
  , efoot :: STRef s (Maybe (EEnt s))
  , eref  :: STRef s (Maybe (EEnt s))
  }

newEnt :: Int -> [Int] -> Int -> Int -> Int -> Int
       -> Maybe (EEnt s) -> Maybe (EEnt s) -> Maybe (EEnt s) -> Maybe (EEnt s)
       -> ST s (EEnt s)
newEnt v row c i no idd par head foot ref = do
  ev <- newSTRef v
  er <- newSTRef row
  ec <- newSTRef c
  ei <- newSTRef i
  en <- newSTRef no
  eid <- newSTRef idd
  epar <- newSTRef par
  ehead <- newSTRef head
  efoot <- newSTRef foot
  eref <- newSTRef ref
  pure (EEnt ev er ec ei en eid epar ehead efoot eref)

makeRoot :: ST s (EEnt s)
makeRoot = newEnt 0 [1] (-1) 0 0 0 Nothing Nothing Nothing Nothing

gV :: EEnt s -> ST s Int;        gV = readSTRef . ev
gR :: EEnt s -> ST s [Int];     gR = readSTRef . er
gC :: EEnt s -> ST s Int;       gC = readSTRef . ec
gI :: EEnt s -> ST s Int;       gI = readSTRef . ei
gN :: EEnt s -> ST s Int;       gN = readSTRef . en
gId :: EEnt s -> ST s Int;      gId = readSTRef . eid
gP :: EEnt s -> ST s (Maybe (EEnt s)); gP = readSTRef . epar
gH :: EEnt s -> ST s (Maybe (EEnt s)); gH = readSTRef . ehead
gF :: EEnt s -> ST s (Maybe (EEnt s)); gF = readSTRef . efoot
gRef :: EEnt s -> ST s (Maybe (EEnt s)); gRef = readSTRef . eref

sV :: EEnt s -> Int -> ST s ();    sV e = writeSTRef (ev e)
sR :: EEnt s -> [Int] -> ST s (); sR e = writeSTRef (er e)
sI :: EEnt s -> Int -> ST s ();   sI e = writeSTRef (ei e)
sN :: EEnt s -> Int -> ST s ();   sN e = writeSTRef (en e)
sId :: EEnt s -> Int -> ST s ();  sId e = writeSTRef (eid e)
sP :: EEnt s -> Maybe (EEnt s) -> ST s (); sP e = writeSTRef (epar e)
sH :: EEnt s -> Maybe (EEnt s) -> ST s (); sH e = writeSTRef (ehead e)
sF :: EEnt s -> Maybe (EEnt s) -> ST s (); sF e = writeSTRef (efoot e)
sRef :: EEnt s -> Maybe (EEnt s) -> ST s (); sRef e = writeSTRef (eref e)

-- 基础工具

rowGenerator :: Int -> [Int]
rowGenerator n
  | n <= 0 = []
  | otherwise = 1 : replicate (n - 1) 0

compareRow :: [Int] -> [Int] -> Int
compareRow r1 r2 = go 0
 where
  go i
    | i < length r1 && i < length r2 =
        let a = r1 !! i; b = r2 !! i
         in if a > b then 1 else if a < b then (-1) else go (i + 1)
    | length r1 > i = 1
    | length r2 > i = (-1)
    | otherwise = 0

compareDimension :: [Int] -> [Int] -> Int
compareDimension r1 r2 = a - b
 where
  a = if length r1 <= 1 then 1 else r1 !! 1
  b = if length r2 <= 1 then 1 else r2 !! 1

rowAddition :: [Int] -> [Int] -> [Int]
rowAddition r1 r2 = go 0 0
 where
  go i j
    | i >= length r1 = take i r1 ++ drop j r2
    | j >= length r2 = r1
    | r1 !! i < r2 !! j = take i r1 ++ drop j r2
    | r1 !! i == r2 !! j && j + 1 < length r2 && r2 !! (j + 1) > r2 !! j =
        go (i + 1) (j + 1)
    | otherwise = go (i + 1) j

rowDifference :: [Int] -> [Int] -> [Int]
rowDifference r1 r2 = go 0 0
 where
  go i j
    | i >= length r1 = []
    | j >= length r2 || r1 !! i > r2 !! j =
        let row = drop i r1
         in if null row then row else prepend (i - 1) row
    | otherwise = go (i + 1) (j + 1)
  -- k 从 i-1 向下到 0；条件 r1[k] < row[0]（row[0] 随前插变化）
  prepend (-1) row = row
  prepend k row =
    if k >= 0 && k < length r1 && r1 !! k < head row
      then prepend (k - 1) (r1 !! k : row)
      else prepend (k - 1) row

divide :: [Int] -> [[Int]]
divide d =
  let dd = if length d > 1 then drop 1 d else []
      n = length dd
      go i acc
        | i + 1 >= n = acc
        | otherwise =
            let cnt = dd !! i
                rows = replicate cnt (rowGenerator (n - i))
             in go (i + 1) (acc ++ rows)
      ret = go 0 []
   in if not (null dd) && last dd /= 0
        then ret ++ [[last dd]]
        else ret

merge :: [[Int]] -> [Int]
merge d
  | null d = []
  | otherwise =
      let base = rowGenerator (length (head d))
          ret1 = (head base - 1) : drop 1 base
          upRet = foldl (\r i ->
                     let sz = length (d !! i)
                         pos = length r - sz
                         l = r !! pos
                      in take pos r ++ [l + 1] ++ drop (pos + 1) r)
                   ret1 [0 .. length d - 2]
          pos = length upRet - length (last d)
          l = upRet !! pos
          upRet2 = take pos upRet ++ [l + head (last d)] ++ drop (pos + 1) upRet
       in 0 : upRet2

proc :: [Int] -> [Int]
proc d =
  if length d > 2 && d !! 0 == 0 && d !! 1 > 0
    then let div = divide d
          in if length div > 1
               then [length (head div) - 1]
               else if length div == 1 && length (head div) > 2
                 then [length (head div) - 2]
                 else d
    else d

isDimLimitedD :: [Int] -> Bool
isDimLimitedD d =
  (length d == 1 && d !! 0 == 0) ||
  (length d == 2 && d !! 0 == 0) ||
  (length d == 3 && d !! 0 == 0 && d !! 1 == 1 && d !! 2 == 0)

-- rowStandardization / getFootRow / isDimensionLimited

rowStandardization :: [Int] -> [Int] -> ST s [Int]
rowStandardization r d =
  if length r <= 2 || r !! (length r - 1) <= r !! (length r - 2)
    then pure r
    else do
      let s = take (length r - 1) r
          s' = take (length s - 1) s ++ [last s + 1]
      seq <- toSequence s'
      rr <- expandImpl seq 2 d False
      if length rr > length r - 1 && rr !! (length r - 1) <= r !! (length r - 1) - 1
        then rowStandardization s' d
        else pure r

getFootRow :: EEnt s -> [Int] -> ST s [Int]
getFootRow it d = do
  mp <- gP it
  case mp of
    Nothing -> pure [1]
    Just p -> do
      itRow <- gR it
      pRow <- gR p
      if compareRow itRow pRow == 0 || null d || isDimLimitedD d
        then pure (itRow ++ [1])
        else do
          let loop i row = do
                let i' = i + 1
                row' <- if i' < length itRow
                          then pure (row ++ [itRow !! i'])
                          else pure (row ++ [0])
                pRowNow <- gR p
                itRowNow <- gR it
                if length pRowNow <= i' || pRowNow !! i' < itRowNow !! i'
                  then pure row'
                  else loop i' row'
          row0 <- loop 0 [1]
          let row1 = take (length row0 - 1) row0 ++ [last row0 + 1]
          rowStandardization row1 [0]

isDimensionLimited :: EEnt s -> [Int] -> ST s Bool
isDimensionLimited it d = do
  let pd = proc d
  foot <- getFootRow it d
  if length pd == 1 && pd !! 0 /= 0 && length foot > 1 && foot !! 1 > pd !! 0
    then pure True
    else if length d == 2 && d !! 0 == 0 && d !! 1 > 0 && length foot > d !! 1
      then pure True
      else pure False

-- toSequence

toSequence :: [Int] -> ST s [EEnt s]
toSequence s = go 0 []
 where
  go i acc
    | i >= length s = pure (reverse acc)
    | otherwise = do
        let vi = s !! i
        if vi <= 1
          then do
            root <- makeRoot
            e <- newEnt vi [] i 0 0 0 (Just root) Nothing Nothing Nothing
            go (i + 1) (e : acc)
          else do
            let findJ j
                  | j < 0 = Nothing
                  | s !! j < vi = Just j
                  | otherwise = findJ (j - 1)
            case findJ (i - 1) of
              Nothing -> do
                root <- makeRoot
                e <- newEnt vi [] i 0 0 0 (Just root) Nothing Nothing Nothing
                go (i + 1) (e : acc)
              Just j -> do
                let parent = acc !! (length acc - 1 - j)
                e <- newEnt vi [] i 0 0 0 (Just parent) Nothing Nothing Nothing
                go (i + 1) (e : acc)

-- drawMountain

drawMountainImpl :: [EEnt s] -> [Int] -> ST s [[EEnt s]]
drawMountainImpl s d = do
  mRef <- newSTRef []
  mapM_ (mkFirst mRef) (zip [0 ..] s)
  mapM_ (drawCol mRef) [0 .. length s - 1]
  readSTRef mRef
 where
  mkFirst mRef (i, e) = do
    mep <- gP e
    parent <- case mep of
      Nothing -> makeRoot
      Just p -> do
        pc <- gC p
        if pc < 0
          then makeRoot
          else do
            mNow <- readSTRef mRef
            pure (head (mNow !! pc))
    eVal <- gV e
    eCol <- gC e
    first <- newEnt eVal [1] eCol 0 0 0 (Just parent) Nothing Nothing Nothing
    mNow <- readSTRef mRef
    writeSTRef mRef (mNow ++ [[first]])
  drawCol mRef i = do
    mNow <- readSTRef mRef
    let col = mNow !! i
    loop mRef i (head col)
  loop mRef i it = do
    v <- gV it
    if v <= 1
      then pure ()
      else do
        limited <- isDimensionLimited it d
        if limited
          then pure ()
          else do
            footRow <- getFootRow it d
            mp <- gP it
            parentVal <- case mp of
              Nothing -> pure 0
              Just p -> gV p
            itVal <- gV it
            itIdx <- gI it
            foot <- newEnt (itVal - parentVal) footRow i (itIdx + 1) 0 0
                     Nothing (Just it) Nothing Nothing
            mNow <- readSTRef mRef
            let col = mNow !! i
            writeSTRef mRef (take i mNow ++ [col ++ [foot]] ++ drop (i + 1) mNow)
            p <- case mp of
              Nothing -> pure Nothing
              Just p0 -> do
                pf <- gF p0
                case pf of
                  Nothing -> pure (Just p0)
                  Just pfE -> do
                    pfRow <- gR pfE
                    fRow <- gR foot
                    if compareRow pfRow fRow <= 0 then pure (Just pfE) else pure (Just p0)
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

-- getReferenceChain / getOds / getMds

getReferenceChain :: EEnt s -> ST s [EEnt s]
getReferenceChain it = go it []
 where
  go e acc = do
    v <- gV e
    r <- gRef e
    row <- gR e
    let acc' = e : acc
    if v <= 1 && length row == 1
      then pure (reverse acc')
      else case r of
        Nothing -> pure (reverse acc')
        Just rE -> go rE acc'

getOdsImpl :: [[EEnt s]] -> ST s [EEnt s]
getOdsImpl m = do
  oRef <- newSTRef []
  mapM_ (mk oRef) m
  readSTRef oRef
 where
  mk oRef col = do
    let lastE = last col
    lv <- gV lastE
    lp <- gP lastE
    parent <-
      if lv <= 1
        then makeRoot
        else case lp of
          Nothing -> makeRoot
          Just p -> do
            pc <- gC p
            if pc >= 0
              then do
                oNow <- readSTRef oRef
                pure (oNow !! pc)
              else makeRoot
    col0 <- head <$> pure col
    c0 <- gC col0
    e <- newEnt lv [1] c0 0 0 0 (Just parent) Nothing Nothing Nothing
    oNow <- readSTRef oRef
    writeSTRef oRef (oNow ++ [e])

getMds :: [[EEnt s]] -> ST s [EEnt s]
getMds m = do
  let lastE = last (last m)
  chain <- getReferenceChain lastE
  seq <- mapM (\e -> do
                  row <- gR e
                  pure (if length row <= 1 then 1 else row !! 1)) chain
  toSequence seq

-- setElementRefrence / setElementNo

setElementRefrence :: [[EEnt s]] -> ST s ()
setElementRefrence m = mapM_ (mapM_ one) m
 where
  one e = do
    row <- gR e
    v <- gV e
    if length row <= 1 && v <= 1
      then pure ()
      else do
        mp <- gP e
        case mp of
          Nothing -> pure ()
          Just p -> do
            eRow <- gR e
            pRow <- gR p
            if compareRow eRow pRow == 0
              then sRef e (Just p)
              else do
                mh <- gH e
                case mh of
                  Nothing -> pure ()
                  Just hE -> do
                    hp <- gP hE
                    case hp of
                      Nothing -> sRef e (Just hE)
                      Just hpE -> do
                        hpf <- gF hpE
                        case hpf of
                          Nothing -> sRef e (Just hpE)
                          Just hpfE -> do
                            hpfRow <- gR hpfE
                            eRowNow <- gR e
                            if compareRow hpfRow eRowNow <= 0
                              then do
                                sRef e (Just hpfE)
                                let chase rNow = case rNow of
                                      Nothing -> pure ()
                                      Just rE -> do
                                        rRow <- gR rE
                                        eRowNow2 <- gR e
                                        if compareRow rRow eRowNow2 == 0
                                          then gRef rE >>= chase
                                          else pure ()
                                gRef e >>= chase
                              else sRef e (Just hpE)

setElementNo :: [[EEnt s]] -> Maybe (EEnt s) -> ST s ()
setElementNo m b = do
  idRef <- newSTRef 0
  bCol <- maybe (pure (-1)) gC b
  mapM_ (one idRef bCol) (zip [0 ..] m)
 where
  one idRef bCol (i, col) = mapM_ (oneE idRef bCol i) (zip [0 ..] col)
  oneE idRef bCol i (j, e) = do
    if i == bCol
      then sN e (j + 1)
      else case b of
        Nothing -> do
          r <- gRef e
          case r of
            Nothing -> pure ()
            Just rE -> sN e =<< gN rE
        Just bE -> do
          if i < bCol
            then sN e 0
            else do
              v <- gV e
              if v <= 1 && j == 0
                then sN e 0
                else do
                  r <- gRef e
                  case r of
                    Nothing -> pure ()
                    Just rE -> sN e =<< gN rE
    when (i > bCol) $ do
      cur <- readSTRef idRef
      sId e cur
      writeSTRef idRef (cur + 1)
    when (i == bCol || i == length m - 1) $ do
      nn <- gN e
      sId e nn

-- copyElement / copyCloumn

copyElementImpl :: [[EEnt s]] -> Maybe (EEnt s) -> EEnt s -> EEnt s
                -> [EEnt s] -> Int -> [Int] -> Bool -> ST s [EEnt s]
copyElementImpl m b t it op i d f = do
  bCol <- maybe (pure (-1)) gC b
  itRow <- gR it
  itVal <- gV it
  when (itVal <= 1 && length itRow > 1) $ do
    r <- gRef it
    sP it r
    let climb = do
          p <- gP it
          case p of
            Nothing -> pure ()
            Just pE -> do
              pv <- gV pE
              if pv > 1
                then do
                  gRef pE >>= sP it
                  climb
                else pure ()
    climb
  minRow <- case op of
    [] -> pure [1]
    (opE : _) -> do
      oRow <- gR opE
      if length oRow > 1 then getFootRow opE d else pure [1]
  maxRow0 <- gR it
  refIt <- gRef it
  tCol <- gC t
  itColNow <- gC it
  baseCol <- maybe (pure itColNow) gC refIt
  let c = if f
            then baseCol + (tCol - bCol) * (i + 1)
            else tCol + i
  cOk <- if c < 0 || c >= length m
           then if tCol >= 0 && tCol < length m then pure tCol else pure 0
           else pure c
  let r0 = head (m !! cOk)
  rFinal <- case refIt of
    Nothing -> pure r0
    Just rI -> do
      rid <- gId rI
      let loop rNow = do
            rf <- gF rNow
            case rf of
              Nothing -> pure rNow
              Just rfE -> do
                rfid <- gId rfE
                if rfid <= rid then loop rfE else pure rNow
      loop r0
  fr <- do
    itFoot <- gF it
    case itFoot of
      Nothing -> getFootRow it d
      Just fE -> gR fE
  maxRow1 <- case refIt of
    Nothing -> pure maxRow0
    Just rI -> do
      rIRow <- gR rI
      let diff = rowDifference maxRow0 rIRow
      if compareRow diff [1, 2] < 0
        then do
          rRow <- gR rFinal
          pure (rowAddition rRow diff)
        else do
          let diffFr = rowDifference fr rIRow
          seq <- toSequence diffFr
          expanded <- expandImpl seq (length maxRow0 + i + 1) [0] False
          rRow <- gR rFinal
          pure (rowAddition rRow (rowDifference expanded rIRow))
  tIdx <- gI t
  itIdxNow <- gI it
  maxRow2 <- case refIt of
    Nothing -> pure maxRow1
    Just rI -> do
      rIRow <- gR rI
      eRowNow <- gR it
      let diff = rowDifference eRowNow rIRow
      if itColNow == tCol && itIdxNow == tIdx && compareRow diff [1, 2] >= 0
        then do
          p <- gP t
          sP t b
          maxR <- gRef t >>= \mtR -> case mtR of
            Nothing -> pure maxRow1
            Just mtRE -> do
              mtRRow <- gR mtRE
              tFootRow <- getFootRow t d
              let diff2 = rowDifference tFootRow mtRRow
              seq <- toSequence diff2
              expanded <- expandImpl seq (length eRowNow + i + 1) [0] False
              rRow <- gR rFinal
              pure (rowAddition rRow (rowDifference expanded mtRRow))
          sP t p
          pure maxR
        else pure maxRow1
  let maxRow3 = if null d || isDimLimitedD d then itRow else maxRow2
  let loop row opAcc
        | compareRow row maxRow3 > 0 = pure (reverse opAcc)
        | otherwise = do
            eNo <- gN it
            eId <- gId it
            eValNow <- gV it
            e <- newEnt eValNow row (length m - 1) (length opAcc) eNo eId
                 Nothing Nothing Nothing Nothing
            let opAcc' = e : opAcc
            case opAcc' of
              (_ : prev : _) -> do
                sH e (Just prev)
                sF prev (Just e)
              _ -> pure ()
            itParCol <- maybe (pure (-1)) gC =<< gP it
            let pc = if f
                       then if itParCol >= bCol
                              then length m - 1 + itParCol - itColNow
                              else itParCol
                       else if itParCol >= bCol
                              then length m - 2
                              else itParCol
            p <- if pc >= 0 && pc < length m
                   then pure (last (m !! pc))
                   else makeRoot
            let climbP pE = do
                  pRow <- gR pE
                  if compareRow pRow row > 0
                    then gH pE >>= \mh -> case mh of
                      Nothing -> pure pE
                      Just hE -> climbP hE
                    else pure pE
            pFinal <- climbP p
            sP e (Just pFinal)
            rowNext <- getFootRow e d
            loop rowNext opAcc'
  loop minRow op

copyCloumn :: [[EEnt s]] -> Maybe (EEnt s) -> EEnt s -> Int -> Int
           -> [Int] -> [Int] -> Bool -> ST s [[EEnt s]]
copyCloumn m b t c i ex d f = do
  let it0 = head (m !! c)
  let m' = m ++ [[]]
  newColRef <- newSTRef []
  let loop it = do
        opNow <- readSTRef newColRef
        opNew <- copyElementImpl m' b t it opNow i d f
        writeSTRef newColRef opNew
        itFoot <- gF it
        case itFoot of
          Nothing -> pure ()
          Just fE -> loop fE
  loop it0
  newCol <- readSTRef newColRef
  let newIdx = length m' - 1
  itVal <- gV it0
  sV (last newCol) (if newIdx < length ex then ex !! newIdx else itVal)
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

-- expand_impl

expandImpl :: [EEnt s] -> Int -> [Int] -> Bool -> ST s [Int]
expandImpl s n d f = do
  if null s
    then pure []
    else do
      lastVal <- gV (last s)
      if lastVal <= 1
        then mapM gV (take (length s - 1) s)
        else do
          m0 <- drawMountainImpl s d
          let t0 = last (last m0)
          t0Val <- gV t0
          t0Head <- gH t0
          t <- if t0Val == 1
                 then maybe (pure t0) pure t0Head
                 else pure t0
          b <- gP t
          setElementRefrence m0
          setElementNo m0 b
          tVal <- gV t
          (m1, ex) <- case b of
            Nothing -> pure (m0, [])
            Just bE -> do
              bVal <- gV bE
              if tVal - bVal > 1 && length (proc d) == 1
                then do
                  o <- getOdsImpl m0
                  let newD = if length d == 1 || length (divide d) == 1
                               then d
                               else merge (drop 1 (divide d))
                  ex1 <- expandImpl o n newD f
                  pure (m0, ex1)
                else do
                  tFoot <- gF t
                  case tFoot of
                    Nothing -> pure (m0, [])
                    Just _ -> do
                      let col = last m0
                          m0' = take (length m0 - 1) m0
                                  ++ [take (length col - 1) col]
                      sF t Nothing
                      mbPar <- gP t
                      case mbPar of
                        Nothing -> pure ()
                        Just p -> gP p >>= sP t
                      pure (m0', [])
          let lastCol = last m1
          mapM_ (\e -> gV e >>= sV e . subtract 1) lastCol
          m2 <- case b of
            Nothing -> pure m1
            Just bE -> do
              bCol <- gC bE
              tCol <- gC t
              if bCol >= 0 && bCol < length m1 && tCol >= 0 && tCol < length m1
                then do
                  let limit = length (m1 !! bCol)
                  bNo <- gN bE
                  tIdx <- gI t
                  let copyRange i mNow
                        | i >= limit = pure mNow
                        | otherwise = do
                            let src = (mNow !! bCol) !! i
                            srcVal <- gV src
                            srcRow <- gR src
                            srcNo <- gN src
                            srcPar <- gP src
                            let newIdx = tIdx + 1
                            e <- newEnt srcVal srcRow tCol newIdx srcNo srcNo
                              srcPar (Just (last (mNow !! tCol))) Nothing (Just src)
                            let col = mNow !! tCol
                            sF (last col) (Just e)
                            sI t newIdx
                            copyRange (i + 1)
                              (take tCol mNow ++ [col ++ [e]] ++ drop (tCol + 1) mNow)
                  copyRange bNo m1
                else pure m1
          let loopN i mNow
                | i >= n = pure mNow
                | otherwise = do
                    if f
                      then case b of
                        Nothing -> loopN (i + 1) mNow
                        Just bE -> do
                          bCol <- gC bE
                          tCol <- gC t
                          let loopJ j mNow2
                                | j > tCol = pure mNow2
                                | j < 0 = loopJ (j + 1) mNow2
                                | j >= length mNow2 = loopJ (j + 1) mNow2
                                | otherwise = do
                                    mNow3 <- copyCloumn mNow2 b t j i ex d True
                                    loopJ (j + 1) mNow3
                          mNow3 <- loopJ (bCol + 1) mNow
                          loopN (i + 1) mNow3
                      else do
                        tCol <- gC t
                        if tCol >= 0 && tCol < length mNow
                          then do
                            mNow' <- copyCloumn mNow b t tCol i ex d False
                            loopN (i + 1) mNow'
                          else loopN (i + 1) mNow
          mFinal <- loopN 0 m2
          mapM (\col -> if null col then pure 0 else gV (head col)) mFinal

expandEpsilonY :: [Int] -> Int -> [Int]
expandEpsilonY seq term
  | term < 1 || null seq = seq
  | otherwise = runST $ do
      s <- toSequence seq
      expandImpl s term [1] False

epsilonYNotation :: Notation
epsilonYNotation =
  seqNotation "epsilon-y" "ε-Y" ["Y序列", "ε-Y"] expandEpsilonY ""
