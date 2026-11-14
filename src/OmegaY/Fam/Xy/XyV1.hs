-- | xy-v1（X-Y V1）：忠实移植 C++ @notation/xy-v1.hpp@。
module OmegaY.Fam.Xy.XyV1
  ( xyV1Notation
  , expandXyV1
  ) where

import Control.Monad.ST (ST, runST)
import Data.STRef
  ( STRef
  , modifySTRef'
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import OmegaY.Fam.Xy.Common
import OmegaY.Notation.Types (Notation, seqNotation)

dXY :: [Int]
dXY = [1, 0, 1]

valOf :: World s -> Int -> ST s Int
valOf w eid = e_value <$> getE w eid

rowOf :: World s -> Int -> ST s [Int]
rowOf w eid = e_row <$> getE w eid

-- | parent 的 row（无父则 Nothing）。
parentRow :: World s -> Int -> ST s (Maybe [Int])
parentRow w eid = do
  e <- getE w eid
  case e_parent e of
    Nothing -> pure Nothing
    Just p -> Just . e_row <$> getE w p

-- ---------------------------------------------------------------------------
-- drawMountain：建山，就地更新 mref，返回各列顶元素 id（按列顺序）。
-- ---------------------------------------------------------------------------

drawMountain :: World s -> STRef s [[Int]] -> ST s [Int]
drawMountain w mref = do
  m <- readSTRef mref
  tops <- mapM (drawCol dXY) (zip [0 ..] m)
  pure tops
 where
  drawCol d (i, col) = do
    m <- readSTRef mref
    let itId = head (m !! i)
    (topId, stack) <- climb itId [itId]
    writeSTRef mref (take i m ++ [reverse stack] ++ drop (i + 1) m)
    pure topId
   where
    climb curId stack = do
      cur <- getE w curId
      if e_value cur <= 1
        then pure (curId, stack)
        else do
          mpRow <- parentRow w curId
          if isDimensionLimitedV12 cur mpRow d
            then pure (curId, stack)
            else do
              pv <- case e_parent cur of
                Nothing -> pure 0
                Just p -> valOf w p
              let footVal = e_value cur - pv
              if footVal >= e_value cur
                then pure (curId, stack)
                else do
                  fr <- getFootRow cur <$> parentRow w curId <*> pure d
                  foot <- allocE w (E footVal fr (e_cloumn cur) 0 Nothing (Just curId) Nothing [] [])
                  modE w curId (\e -> e { e_foot = Just foot })
                  p0 <- case e_parent cur of
                    Nothing -> pure Nothing
                    Just p -> do
                      pe <- getE w p
                      case e_foot pe of
                        Nothing -> pure (Just p)
                        Just pf -> do
                          pfe <- getE w pf
                          pure (if compareRow (e_row pfe) fr <= 0 then Just pf else Just p)
                  pFinal <- climbUp p0 footVal
                  modE w foot (\e -> e { e_parent = pFinal })
                  climb foot (foot : stack)
    climbUp mp fv = case mp of
      Nothing -> pure Nothing
      Just p -> do
        pe <- getE w p
        if e_value pe >= fv then climbUp (e_parent pe) fv else pure (Just p)

-- ---------------------------------------------------------------------------
-- fetchDimensionSequence
-- ---------------------------------------------------------------------------

fetchDimensionSequence :: World s -> Int -> ST s [Int]
fetchDimensionSequence w tId = do
  t <- getE w tId
  prowLen <- case e_parent t of
    Nothing -> pure 0
    Just p -> length . e_row <$> getE w p
  let seq0 = [prowLen, length (e_row t) + 1]
  case e_parent t of
    Nothing -> pure seq0
    Just p -> loopUp p seq0
 where
  loopUp curId seq = do
    cur <- getE w curId
    if length (e_row cur) <= 1
      then pure seq
      else case e_head cur of
        Nothing -> pure seq
        Just h -> do
          he <- getE w h
          case e_parent he of
            Nothing -> pure seq
            Just hp -> do
              hpe <- getE w hp
              let seq' =
                    if length (e_row hpe) < head seq
                      then length (e_row hpe) : seq
                      else seq
              loopUp hp seq'

-- ---------------------------------------------------------------------------
-- expandDimensionSequence（d=[1,0,1] → 在维度序列上递归）
-- ---------------------------------------------------------------------------

expandDimensionSequence :: World s -> [Int] -> Int -> ST s [Int]
expandDimensionSequence w s0 n0 = do
  let len = length s0
  if len >= 2 && (last s0 - atM s0 (len - 2)) == 1
    then do
      let p = last s0 - 1
          s1 = take (len - 1) s0 ++ [p]
      pure (s1 ++ replicate n0 p)
    else if compareRow dXY [1, 0, 1] == 0
      then do
        seq <- toSequenceV12 w s0
        expandImpl w seq n0 dXY
      else if null s0
        then pure s0
        else do
          let p = last s0 - 1
              s1 = take (len - 1) s0 ++ [p]
          pure (s1 ++ replicate n0 p)

-- ---------------------------------------------------------------------------
-- preprocess
-- ---------------------------------------------------------------------------

preprocess :: World s -> STRef s [[Int]] -> Int -> ST s ()
preprocess w mref bId = do
  b <- getE w bId
  let bcol = e_cloumn b
  m <- readSTRef mref
  mapM_ (\(j, eid) -> modE w eid (\e -> e { e_no = j + 1 })) (zip [0 ..] (m !! bcol))
  mapM_ (setAfter bcol) [bcol + 1 .. length m - 1]
 where
  setAfter bcol i = do
    m <- readSTRef mref
    let col = m !! i
    mapM_ (\eid -> do
      e <- getE w eid
      no <- case e_parent e of
        Nothing -> pure 0
        Just p -> do
          pe <- getE w p
          pure (if e_cloumn pe < bcol then 0 else e_no pe)
      modE w eid (\ee -> ee { e_no = no })) col

-- ---------------------------------------------------------------------------
-- calcMaxCopyrow
-- ---------------------------------------------------------------------------

calcMaxCopyrow :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> [Int] -> Int -> Int -> ST s [Int]
calcMaxCopyrow w mref bId tId srcId dimSeq index i = do
  src <- getE w srcId
  b <- getE w bId
  t <- getE w tId
  if compareRow dXY [0] == 0 || e_no src == 0
    then pure (e_row src)
    else do
      m <- readSTRef mref
      let bcol = e_cloumn b
          bcolCol = if bcol >= 0 && bcol < length m then m !! bcol else []
      diff' <-
        if bcol >= 0
          && bcol < length m
          && e_no src >= 1
          && e_no src - 1 < length bcolCol
          then do
            refE <- getE w (bcolCol !! (e_no src - 1))
            pure (rowDifference (e_row src) (e_row refE))
          else pure (e_row src)
      let diff2 =
            if e_no src == e_no b
              && length diff' >= length (e_row t)
              && index + i + 1 >= 0
              && index + i + 1 < length dimSeq
              then
                let l = atM dimSeq (index + i + 1) - length (e_row t)
                 in case diff' of
                      (x : xs) -> x : replicate l 0 ++ xs
                      [] -> replicate l 0
              else diff'
      let pc = e_cloumn t + (e_cloumn t - e_cloumn b) * i
      p <- if pc >= 0 && pc < length m && not (null (m !! pc))
             then pure (Just (last (m !! pc)))
             else pure Nothing
      pFinal <- advanceP p (e_no src)
      case pFinal of
        Just pid -> do
          pe <- getE w pid
          pure (rowAddition (e_row pe) diff2)
        Nothing -> pure (rowAddition (e_row src) diff2)
 where
  advanceP mp no = case mp of
    Nothing -> pure Nothing
    Just p -> do
      pe <- getE w p
      if e_no pe > no
        then case e_head pe of
          Nothing -> pure (Just p)
          Just h -> advanceP (Just h) no
        else pure (Just p)

-- ---------------------------------------------------------------------------
-- copyItem：把 head 向上延伸的脚推入末列
-- ---------------------------------------------------------------------------

copyItem :: World s -> STRef s [[Int]] -> Int -> [Int] -> ST s ()
copyItem w mref headId maxRow = do
  headE <- getE w headId
  case e_parent headE of
    Nothing -> pure ()
    Just p -> do
      pe <- getE w p
      if e_cloumn pe < 0
        then pure ()
        else go headId
 where
  go curId = do
    cur <- getE w curId
    mpRow <- parentRow w curId
    let row = getFootRow cur mpRow dXY
    if compareRow row maxRow > 0
      then pure ()
      else do
        foot <- allocE w (E (e_value cur) row (e_cloumn cur) (e_no cur) (e_parent cur) (Just curId) Nothing [] [])
        modE w curId (\e -> e { e_foot = Just foot })
        case e_parent cur of
          Nothing -> pure ()
          Just hp -> do
            hpe <- getE w hp
            case e_foot hpe of
              Nothing -> pure ()
              Just hpf -> do
                hpfe <- getE w hpf
                if compareRow (e_row hpfe) row <= 0
                  then do
                    footE <- getE w foot
                    case e_parent footE of
                      Nothing -> pure ()
                      Just fp -> do
                        fpe <- getE w fp
                        case e_foot fpe of
                          Nothing -> pure ()
                          Just fpf -> modE w foot (\e -> e { e_parent = Just fpf })
                  else pure ()
        modifySTRef' mref (\mm -> take (length mm - 1) mm ++ [last mm ++ [foot]])
        go foot

-- ---------------------------------------------------------------------------
-- expandImpl
-- ---------------------------------------------------------------------------

expandImpl :: World s -> [Int] -> Int -> [Int] -> ST s [Int]
expandImpl w s0 n d = do
  if null s0
    then pure []
    else do
      lastE <- getE w (last s0)
      if e_value lastE <= 1
        then mapM (valOf w) (init s0)
        else do
          mref <- newSTRef (map (: []) s0)
          o <- drawMountain w mref
          let topLast = last o
          topLastE <- getE w topLast
          case e_head topLastE of
            Nothing -> mapM (valOf w) (init s0)
            Just t -> do
              tE <- getE w t
              case e_parent tE of
                Nothing -> mapM (valOf w) (init s0)
                Just bId0 -> do
                  bE0 <- getE w bId0
                  let len0 = e_cloumn tE - e_cloumn bE0
                  dimSeq <- fetchDimensionSequence w t
                  let index = length dimSeq - 1
                  dimSeq' <- expandDimensionSequence w dimSeq n
                  topLastNowE <- getE w (last o)
                  (ex, bId, len) <-
                    if e_value topLastNowE > 1
                      then do
                        outerRef <- newSTRef []
                        mapM_ (buildOuter w outerRef) o
                        outer <- readSTRef outerRef
                        ex <- expandImpl w outer n d
                        let lenR = if n > 0 then (length ex - length o) `div` n else len0
                            bcol = e_cloumn tE - lenR
                        bNew <-
                          if bcol >= 0 && bcol < length o
                            then pure (outer !! bcol)
                            else pure bId0
                        pure (ex, bNew, lenR)
                      else do
                        modE w t (\e -> e { e_foot = Nothing })
                        mN <- readSTRef mref
                        let tcol = e_cloumn tE
                        writeSTRef mref (take tcol mN ++ [init (mN !! tcol)] ++ drop (tcol + 1) mN)
                        pure ([], bId0, len0)
                  if bId == (-1)
                    then mapM (valOf w) (init s0)
                    else do
                      preprocess w mref bId
                      mN2 <- readSTRef mref
                      let tcol = e_cloumn tE
                      mapM_ (\eid -> modE w eid (\e -> e { e_value = e_value e - 1 })) (mN2 !! tcol)
                      bE <- getE w bId
                      mapM_ (\i -> do
                        mapM_ (\j -> copyCol w mref bId t j i len dimSeq' index ex) [e_cloumn bE + 1 .. e_cloumn tE]) [0 .. n - 1]
                      mFinal <- readSTRef mref
                      mapM (valOf w . head) mFinal

buildOuter :: World s -> STRef s [Int] -> Int -> ST s ()
buildOuter w outerRef e = do
  oe <- getE w e
  outer <- readSTRef outerRef
  p <- case e_parent oe of
    Nothing -> pure Nothing
    Just pc ->
      if e_cloumn oe >= 0 && e_cloumn oe < length outer
        then pure (Just (outer !! e_cloumn oe))
        else pure Nothing
  pRoot <- case p of
    Just pid -> pure pid
    Nothing -> allocE w makeRootV12
  ne <- allocE w (E (e_value oe) [0] (e_cloumn oe) 0 (Just pRoot) Nothing Nothing [] [])
  modifySTRef' outerRef (++ [ne])

copyCol :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int -> [Int] -> ST s ()
copyCol w mref bId tId j i len dimSeq index ex = do
  m <- readSTRef mref
  if j < 0 || j >= length m
    then pure ()
    else do
      let l = length m
      writeSTRef mref (m ++ [[]])
      let srcCol = m !! j
      mapM_ (copyRowElt w mref bId tId j i len dimSeq index) (zip [0 ..] srcCol)
      m2 <- readSTRef mref
      let newCol2 = m2 !! l
      lv <- if null ex
              then e_value <$> getE w (last srcCol)
              else pure (let eidx = j + len * i + len in if eidx >= 0 && eidx < length ex then ex !! eidx else 0)
      modE w (last newCol2) (\e -> e { e_value = lv })
      m3 <- readSTRef mref
      let col3 = m3 !! l
      mapM_ (\k -> do
        let below = col3 !! k
            above = col3 !! (k - 1)
        belowV <- valOf w below
        aboveE <- getE w above
        pval <- case e_parent aboveE of
          Nothing -> pure 0
          Just p -> valOf w p
        modE w above (\e -> e { e_value = belowV + pval })) (reverse [1 .. length col3 - 1])

copyRowElt :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int -> (Int, Int) -> ST s ()
copyRowElt w mref bId tId j i len dimSeq index (k, src) = do
  srcE <- getE w src
  m <- readSTRef mref
  let l = length m - 1
      newCol = m !! l
  maxRow <- calcMaxCopyrow w mref bId tId src dimSeq index i
  headId <-
    if null newCol
      then do
        par <- case e_parent srcE of
          Nothing -> allocE w makeRootV12
          Just p -> pure p
        srcCol <- (!! j) <$> readSTRef mref
        firstSrc <- getE w (head srcCol)
        allocE w (E 0 [0] l (e_no firstSrc) (Just par) Nothing Nothing [] [])
      else do
        let belowId = last newCol
        belowE <- getE w belowId
        fr <- getFootRow belowE <$> parentRow w belowId <*> pure dXY
        headId <- allocE w (E 0 fr l (e_no srcE) (e_parent srcE) (Just belowId) Nothing [] [])
        modE w belowId (\e -> e { e_foot = Just headId })
        pure headId
  modifySTRef' mref (\mm -> take l mm ++ [mm !! l ++ [headId]])
  m2 <- readSTRef mref
  headE <- getE w headId
  bE <- getE w bId
  case e_parent headE of
    Nothing -> pure ()
    Just hp -> do
      hpe <- getE w hp
      if e_cloumn hpe >= e_cloumn bE
        then do
          let pc = e_cloumn hpe + len * i + len
          m3 <- readSTRef mref
          if pc >= 0 && pc < length m3 && not (null (m3 !! pc))
            then modE w headId (\e -> e { e_parent = Just (last (m3 !! pc)) })
            else pure ()
        else pure ()
  fixParent w headId
  m4 <- readSTRef mref
  copyItem w mref (last (m4 !! l)) maxRow

fixParent :: World s -> Int -> ST s ()
fixParent w headId = go headId
 where
  go hid = do
    he <- getE w hid
    case e_parent he of
      Nothing -> pure ()
      Just p -> do
        pe <- getE w p
        case e_head pe of
          Nothing -> pure ()
          Just h ->
            if compareRow (e_row pe) (e_row he) > 0
              then do
                modE w hid (\e -> e { e_parent = Just h })
                go hid
              else pure ()

-- ---------------------------------------------------------------------------
-- 公开入口
-- ---------------------------------------------------------------------------

expandXyV1 :: [Int] -> Int -> [Int]
expandXyV1 seq term
  | term < 1 || null seq = seq
  | otherwise = runST $ do
      let term' = min term 10
      w <- newWorld
      s <- toSequenceV12 w seq
      expandImpl w s term' dXY

xyV1Notation :: Notation
xyV1Notation =
  seqNotation "xy-v1" "X-Y V1" ["X-Y", "数列"] expandXyV1 ""
