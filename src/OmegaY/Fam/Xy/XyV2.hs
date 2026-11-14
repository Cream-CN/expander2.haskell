-- | xy-v2（X-Y V2）：忠实移植 C++ @notation/xy-v2.hpp@。
module OmegaY.Fam.Xy.XyV2
  ( xyV2Notation
  , expandXyV2
  ) where

import Control.Monad (filterM)
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

parentRow :: World s -> Int -> ST s (Maybe [Int])
parentRow w eid = do
  e <- getE w eid
  case e_parent e of
    Nothing -> pure Nothing
    Just p -> Just . e_row <$> getE w p

-- ---------------------------------------------------------------------------
-- drawMountain（v2：void，就地修改 mref）
-- ---------------------------------------------------------------------------

drawMountain :: World s -> STRef s [[Int]] -> ST s ()
drawMountain w mref = do
  m <- readSTRef mref
  mapM_ (drawCol dXY) (zip [0 ..] m)
 where
  drawCol d (i, _) = do
    m <- readSTRef mref
    let itId = head (m !! i)
    climb itId [itId]
   where
    climb curId stack = do
      cur <- getE w curId
      if e_value cur <= 1
        then do
          m <- readSTRef mref
          writeSTRef mref (take i m ++ [reverse stack] ++ drop (i + 1) m)
        else do
          mpRow <- parentRow w curId
          if isDimensionLimitedV12 cur mpRow d
            then do
              m <- readSTRef mref
              writeSTRef mref (take i m ++ [reverse stack] ++ drop (i + 1) m)
            else do
              pv <- case e_parent cur of
                Nothing -> pure 0
                Just p -> valOf w p
              let footVal = e_value cur - pv
              if footVal >= e_value cur
                then do
                  m <- readSTRef mref
                  writeSTRef mref (take i m ++ [reverse stack] ++ drop (i + 1) m)
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
-- setElementNo
-- ---------------------------------------------------------------------------

setElementNo :: World s -> STRef s [[Int]] -> Int -> ST s ()
setElementNo w mref bId = do
  b <- getE w bId
  let bcol = e_cloumn b
  m <- readSTRef mref
  mapM_ (goCol bcol) (zip [0 ..] m)
 where
  goCol bcol (i, col) = mapM_ (goElt bcol i) (zip [0 ..] col)
  goElt bcol i (j, eid) = do
    e <- getE w eid
    no <-
      if i == bcol
        then pure (j + 1)
        else if i < bcol || (e_value e <= 1 && j == 0)
          then pure 0
          else if e_value e > 1
            then case e_parent e of
              Nothing -> pure 0
              Just p -> do
                pe <- getE w p
                if compareRow (e_row e) (e_row pe) == 0
                  then pure (e_no pe)
                  else case e_head e of
                    Nothing -> pure 0
                    Just h -> do
                      he <- getE w h
                      case e_parent he of
                        Nothing -> pure 0
                        Just hp -> e_no <$> getE w hp
            else case e_head e of
              Nothing -> pure 0
              Just h -> do
                he <- getE w h
                case e_parent he of
                  Nothing -> pure 0
                  Just hp -> e_no <$> getE w hp
    modE w eid (\ee -> ee { e_no = no })

-- ---------------------------------------------------------------------------
-- getReferenceChain
-- ---------------------------------------------------------------------------

getReferenceChain :: World s -> Int -> ST s [Int]
getReferenceChain w itId = go itId []
 where
  go curId acc
    | curId < 0 = pure (reverse acc)
    | otherwise = do
        cur <- getE w curId
        let acc' = curId : acc
        stop <-
          if e_value cur <= 1 && compareRow (e_row cur) [0] == 0
            then pure True
            else if e_value cur > 1
              then case e_parent cur of
                Nothing -> pure False
                Just p -> do
                  pe <- getE w p
                  if compareRow (e_row cur) (e_row pe) == 0
                    then pure False  -- 继续沿 parent
                    else headBranch cur acc'
              else headBranch cur acc'
        if stop
          then pure (reverse acc')
          else do
            next <- nextNode cur
            case next of
              Nothing -> pure (reverse acc')
              Just nxt -> go nxt acc'
   where
    headBranch cur acc' = do
      case e_head cur of
        Nothing -> pure True
        Just h -> do
          he <- getE w h
          case e_parent he of
            Nothing -> pure True
            Just hp -> do
              hpe <- getE w hp
              case e_foot hpe of
                Nothing -> pure True
                Just hpf -> do
                  hpfe <- getE w hpf
                  pure (compareRow (e_row cur) (e_row hpfe) < 0)
    nextNode cur = do
      case e_parent cur of
        Just p -> do
          pe <- getE w p
          if compareRow (e_row cur) (e_row pe) == 0
            then pure (Just p)
            else case e_head cur of
              Nothing -> pure Nothing
              Just h -> do
                he <- getE w h
                case e_parent he of
                  Nothing -> pure Nothing
                  Just hp -> do
                    hpe <- getE w hp
                    case e_foot hpe of
                      Nothing -> pure (Just hp)
                      Just hpf -> do
                        hpfe <- getE w hpf
                        if compareRow (e_row cur) (e_row hpfe) >= 0
                          then pure (Just hpf)
                          else pure (Just hp)
        Nothing -> pure Nothing

-- ---------------------------------------------------------------------------
-- getMainDimensionSequence
-- ---------------------------------------------------------------------------

getMainDimensionSequence :: World s -> [Int] -> Int -> ST s [Int]
getMainDimensionSequence w chain tId = do
  t <- getE w tId
  sRef <- newSTRef []
  mapM_ (\e -> do
    ee <- getE w e
    ne <- allocE w (E (length (e_row ee)) [0] (e_cloumn ee) 0 Nothing Nothing Nothing [] [])
    modifySTRef' sRef (++ [ne])) chain
  last <- allocE w (E (length (e_row t) + 1) [0] (e_cloumn t) 0 Nothing Nothing Nothing [] [])
  modifySTRef' sRef (++ [last])
  s <- readSTRef sRef
  -- parent 链接
  mapM_ (\(i, eid) -> do
    e <- getE w eid
    if e_value e <= 1
      then do
        root <- allocE w makeRootV12
        modE w eid (\ee -> ee { e_parent = Just root })
      else do
        smaller <- filterM (\j -> do
          ej <- getE w (s !! j)
          pure (e_value ej < e_value e)) [i - 1, i - 2 .. 0]
        case smaller of
          (j : _) -> modE w eid (\ee -> ee { e_parent = Just (s !! j) })
          [] -> do
            root <- allocE w makeRootV12
            modE w eid (\ee -> ee { e_parent = Just root })) (zip [0 ..] s)
  pure s

-- ---------------------------------------------------------------------------
-- getBootCloumn
-- ---------------------------------------------------------------------------

getBootCloumn :: World s -> [Int] -> ST s Int
getBootCloumn w s = do
  mref <- newSTRef (map (: []) s)
  drawMountain w mref
  m <- readSTRef mref
  if null m || length (last m) < 2
    then pure (-1)
    else do
      let lc = last m
          t = lc !! (length lc - 2)
      tE <- getE w t
      case e_parent tE of
        Nothing -> pure (-1)
        Just b -> do
          bE <- getE w b
          if length (e_row tE) == length (e_row bE) || length dXY < 3
            then pure (e_cloumn bE)
            else do
              chain <- getReferenceChain w b
              mds <- getMainDimensionSequence w chain t
              getBootCloumn w mds

-- ---------------------------------------------------------------------------
-- isEqualSequnce
-- ---------------------------------------------------------------------------

isEqualSequnce :: World s -> [Int] -> [Int] -> ST s Bool
isEqualSequnce w s1 s2 = do
  if length s1 /= length s2
    then pure False
    else and <$> mapM check (zip s1 s2)
 where
  check (a, b) = do
    ae <- getE w a
    be <- getE w b
    ca <- case e_parent ae of
      Nothing -> pure (-1)
      Just p -> e_cloumn <$> getE w p
    cb <- case e_parent be of
      Nothing -> pure (-1)
      Just p -> e_cloumn <$> getE w p
    pure (e_value ae == e_value be && ca == cb)

-- ---------------------------------------------------------------------------
-- copyElement（v2）
-- ---------------------------------------------------------------------------

copyElement :: World s -> STRef s [[Int]] -> Int -> [Int] -> ST s ()
copyElement w mref headId maxRow = do
  headE <- getE w headId
  case e_parent headE of
    Nothing -> pure ()
    Just p -> do
      pe <- getE w p
      if e_cloumn pe < 0
        then do
          case e_head headE of
            Nothing -> pure ()
            Just h -> do
              he <- getE w h
              case e_parent he of
                Nothing -> pure ()
                Just hp -> do
                  hpe <- getE w hp
                  if compareRow (e_row hpe) maxRow < 0
                    then modE w headId (\e -> e { e_parent = Just hp })
                    else pure ()
        else pure ()
  go headId
 where
  go curId = do
    cur <- getE w curId
    case e_parent cur of
      Nothing -> pure ()
      Just p -> do
        pe <- getE w p
        if e_cloumn pe < 0
          then pure ()
          else do
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
-- copyCloumn（v2）
-- ---------------------------------------------------------------------------

copyCloumn :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> ST s ()
copyCloumn w mref bId tId j i len ex = do
  m <- readSTRef mref
  if j < 0 || j >= length m
    then pure ()
    else do
      let l = length m
      writeSTRef mref (m ++ [[]])
      let srcCol = m !! j
      mapM_ (copyElt w mref bId tId j i len ex) (zip [0 ..] srcCol)
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

copyElt :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> (Int, Int) -> ST s ()
copyElt w mref bId tId j i len ex (k, src) = do
  srcE <- getE w src
  bE <- getE w bId
  m <- readSTRef mref
  let l = length m - 1
      newCol = m !! l
  maxRow <- calcMaxRow w m bE srcE i len
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
  fixParentV2 w headId
  m4 <- readSTRef mref
  copyElement w mref (last (m4 !! l)) maxRow

calcMaxRow :: World s -> [[Int]] -> E -> E -> Int -> Int -> ST s [Int]
calcMaxRow w m bE srcE i len =
  if not (null (e_dimseq srcE))
    then do
      let kk = if i < length (e_dimseq srcE) then e_dimseq srcE !! i else 0
          rowDif =
            if null (e_row srcE) || head (e_row srcE) - 1 == 0
              then []
              else (head (e_row srcE) - 1) : [e_row srcE !! jj | jj <- [1 .. length (e_row srcE) - 2]]
          beg = 1 : replicate (kk - 1) 0
      pure (rowAddition beg rowDif)
    else if e_no srcE > 0
      then do
        let bc = e_cloumn bE + len * (i + 1)
            baseCol = if bc >= 0 && bc < length m then m !! bc else []
            bcolCol = m !! e_cloumn bE
            bsrc = if e_cloumn bE >= 0 && e_no srcE - 1 < length bcolCol
                     then Just (bcolCol !! (e_no srcE - 1))
                     else Nothing
        case bsrc of
          Nothing -> pure (e_row srcE)
          Just bsrcId -> do
            bsrcE <- getE w bsrcId
            let diff = rowDifference (e_row srcE) (e_row bsrcE)
            goBase baseCol diff
      else pure (e_row srcE)
 where
  goBase baseCol diff = case [cc | cc <- [length baseCol - 1, length baseCol - 2 .. 0]] of
    [] -> pure (e_row srcE)
    (cc : _) -> do
      be <- getE w (baseCol !! cc)
      if e_no be > e_no srcE
        then pure (e_row srcE)
        else pure (rowAddition (e_row be) diff)

fixParentV2 :: World s -> Int -> ST s ()
fixParentV2 w headId = go headId
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
-- expandElementDimensionSequence（v2，含 memo）
-- ---------------------------------------------------------------------------

type Memo s = STRef s ([([Int], [Int], Int, [Int])])

expandElementDimensionSequence :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> Memo s -> ST s ()
expandElementDimensionSequence w mref bId tId n memo = do
  b <- getE w bId
  t <- getE w tId
  if length (e_row b) == length (e_row t) || length dXY < 3
    then pure ()
    else do
      chainB <- getReferenceChain w bId
      if null chainB
        then pure ()
        else do
          tParentChain <- case e_parent t of
            Nothing -> pure []
            Just tp -> getReferenceChain w tp
          if null tParentChain
            then pure ()
            else do
              m <- readSTRef mref
              mapM_ (goCol bId tId n memo chainB tParentChain) [e_cloumn b + 1 .. length m - 1]
 where
  goCol bId tId n memo chainB tParentChain i = do
    m <- readSTRef mref
    let col = m !! i
    mapM_ (goElt bId tId n memo chainB tParentChain i) (zip [0 ..] col)

  goElt bId tId n memo chainB tParentChain i (j, itId) = do
    it <- getE w itId
    b <- getE w bId
    if e_no it < e_no b || length (e_row it) <= length (e_row b)
      then pure ()
      else do
        refchain <- getReferenceChain w itId
        let index = length refchain
        cRef <- newSTRef []
        mapM_ (\e -> do
          ee <- getE w e
          ne <- allocE w (E (length (e_row ee)) [0] 0 0 Nothing Nothing Nothing [] [])
          modifySTRef' cRef (++ [ne])) refchain
        let tailStart = length chainB
        mapM_ (\k -> do
          tpE <- getE w (tParentChain !! k)
          ne <- allocE w (E (length (e_row tpE)) [0] 0 0 Nothing Nothing Nothing [] [])
          modifySTRef' cRef (++ [ne])) [tailStart .. length tParentChain - 1]
        c <- readSTRef cRef
        -- 设置 row/cloumn/parent
        mapM_ (\(k, eid) -> do
          e <- getE w eid
          modE w eid (\ee -> ee { e_row = [0], e_cloumn = k })
          if e_value e <= 1
            then do
              root <- allocE w makeRootV12
              modE w eid (\ee -> ee { e_parent = Just root })
            else if k == index
              then if length chainB - 1 < length c
                then modE w eid (\ee -> ee { e_parent = Just (c !! (length chainB - 1)) })
                else pure ()
            else do
              smaller <- filterM (\kk -> do
                ck <- getE w (c !! kk)
                pure (e_value ck < e_value e)) [k, k - 1 .. 0]
              case smaller of
                (kk : _) -> modE w eid (\ee -> ee { e_parent = Just (c !! kk) })
                [] -> pure ()) (zip [0 ..] c)
        -- 末元素
        t <- getE w tId
        last <- allocE w (E (length (e_row t) + 1) [0] (length c) 0 Nothing Nothing Nothing [] [])
        if length tParentChain - 1 < length c
          then modE w last (\e -> e { e_parent = Just (c !! (length tParentChain - 1)) })
          else pure ()
        modifySTRef' cRef (++ [last])
        c2 <- readSTRef cRef
        let len = length c2 - length chainB
        -- memo 查找
        entries <- readSTRef memo
        hit <- findMemo w entries c2 n dXY
        ex <- case hit of
          Just ex -> pure ex
          Nothing -> do
            ex <- expandImpl w c2 n dXY memo
            modifySTRef' memo (++ [(c2, dXY, n, ex)])
            pure ex
        dimvals <- mapM (\k -> do
          let eidx = index - 1 + len * k
          let v = if eidx >= 0 && eidx < length ex then ex !! eidx else 0
          pure v) [1 .. n]
        mapM_ (\v -> modE w itId (\ee -> ee { e_dimseq = e_dimseq ee ++ [v] })) dimvals

findMemo :: World s -> [([Int], [Int], Int, [Int])] -> [Int] -> Int -> [Int] -> ST s (Maybe [Int])
findMemo w entries c n d = go entries
 where
  go [] = pure Nothing
  go ((es, ds, ns, ex) : rest) = do
    if ns >= n && ds == d
      then do
        eq <- isEqualSequnce w c es
        if eq then pure (Just ex) else go rest
      else go rest

-- ---------------------------------------------------------------------------
-- expandImpl（v2）
-- ---------------------------------------------------------------------------

expandImpl :: World s -> [Int] -> Int -> [Int] -> Memo s -> ST s [Int]
expandImpl w s0 n d memo = do
  if null s0
    then pure []
    else do
      vs <- mapM (valOf w) s0
      lastE <- getE w (last s0)
      if e_value lastE <= 1
        then mapM (valOf w) (init s0)
        else do
          c <- getBootCloumn w s0
          -- 建 m
          mref <- newSTRef []
          mapM_ (\e -> do
            ee <- getE w e
            p <- case e_parent ee of
              Nothing -> allocE w makeRootV12
              Just pc -> do
                pce <- getE w pc
                mm <- readSTRef mref
                if e_cloumn pce >= 0 && e_cloumn pce < length mm && not (null (mm !! e_cloumn pce))
                  then pure (head (mm !! e_cloumn pce))
                  else allocE w makeRootV12
            ne <- allocE w (E (e_value ee) [0] (e_cloumn ee) 0 (Just p) Nothing Nothing [] [])
            modifySTRef' mref (\mm -> mm ++ [[ne]])) s0
          drawMountain w mref
          m <- readSTRef mref
          if null (last m) || length (last m) < 2
            then mapM (valOf w) (init s0)
            else do
              let lastCol = last m
                  t = lastCol !! (length lastCol - 2)
              tE <- getE w t
              case e_parent tE of
                Nothing -> mapM (valOf w) (init s0)
                Just bId0 -> do
                  bE0 <- getE w bId0
                  let len = e_cloumn tE - e_cloumn bE0
                  bId <-
                    if c < e_cloumn bE0 && c >= 0 && c < length m
                      then pure (last (m !! c))
                      else pure bId0
                  setElementNo w mref bId
                  -- chain / dim seq
                  if length d > 2
                    then do
                      cb <- getReferenceChain w bId
                      modE w bId (\e -> e { e_chain = cb })
                      case e_parent tE of
                        Nothing -> pure ()
                        Just tp -> do
                          ctp <- getReferenceChain w tp
                          modE w tp (\e -> e { e_chain = ctp })
                      expandElementDimensionSequence w mref bId t n memo
                    else pure ()
                  -- ex 分支：以 t.foot 为条件
                  tE2 <- getE w t
                  (ex, bId', t') <-
                    case e_foot tE2 of
                      Just footId -> do
                        footE <- getE w footId
                        if e_value footE > 1
                          then do
                            -- outer 递归：先建 o（用减一前的值），再减一
                            m3 <- readSTRef mref
                            oRef <- newSTRef []
                            mapM_ (\col -> do
                              let lastE = last col
                              le <- getE w lastE
                              firstE <- getE w (head col)
                              p <- if e_value le <= 1
                                then allocE w makeRootV12
                                else case e_parent le of
                                  Nothing -> allocE w makeRootV12
                                  Just pc -> do
                                    pce <- getE w pc
                                    outer <- readSTRef oRef
                                    if e_cloumn pce >= 0 && e_cloumn pce < length outer
                                      then pure (outer !! e_cloumn pce)
                                      else allocE w makeRootV12
                              ne <- allocE w (E (e_value le) [0] (e_cloumn firstE) 0 (Just p) Nothing Nothing [] [])
                              modifySTRef' oRef (++ [ne])) m3
                            outer <- readSTRef oRef
                            mapM_ (\eid -> modE w eid (\e -> e { e_value = e_value e - 1 })) (last m3)
                            ex <- expandImpl w outer n d memo
                            pure (ex, bId, t)
                          else do
                            -- else：弹出末列顶（t.foot），末列减一，父上移
                            m3 <- readSTRef mref
                            writeSTRef mref (take (length m3 - 1) m3 ++ [init (last m3)])
                            modE w t (\e -> e { e_foot = Nothing })
                            m4 <- readSTRef mref
                            mapM_ (\eid -> modE w eid (\e -> e { e_value = e_value e - 1 })) (last m4)
                            tE3 <- getE w t
                            case e_parent tE3 of
                              Nothing -> pure ([], bId, t)
                              Just tp -> do
                                tE4 <- getE w tp
                                case e_parent tE4 of
                                  Nothing -> pure ([], bId, t)
                                  Just tpp -> do
                                    modE w t (\e -> e { e_parent = Just tpp })
                                    pure ([], bId, t)
                      Nothing -> do
                        m3 <- readSTRef mref
                        writeSTRef mref (take (length m3 - 1) m3 ++ [init (last m3)])
                        modE w t (\e -> e { e_foot = Nothing })
                        m4 <- readSTRef mref
                        mapM_ (\eid -> modE w eid (\e -> e { e_value = e_value e - 1 })) (last m4)
                        tE3 <- getE w t
                        case e_parent tE3 of
                          Nothing -> pure ([], bId, t)
                          Just tp -> do
                            tE4 <- getE w tp
                            case e_parent tE4 of
                              Nothing -> pure ([], bId, t)
                              Just tpp -> do
                                modE w t (\e -> e { e_parent = Just tpp })
                                pure ([], bId, t)
                  -- 把 b 列 no>=b.no 的元素复制到末列
                  bE <- getE w bId'
                  tE4 <- getE w t'
                  m5 <- readSTRef mref
                  let bcol = e_cloumn bE
                      tcol = e_cloumn tE4
                  if bcol >= 0 && bcol < length m5 && tcol >= 0 && tcol < length m5
                    then do
                      let bcolCol = m5 !! bcol
                      mapM_ (\i -> do
                        let src = bcolCol !! i
                        srcE <- getE w src
                        head <- allocE w (E (e_value srcE) (e_row srcE) tcol (e_no srcE) (e_parent srcE) (Just (last (m5 !! tcol))) Nothing [] [])
                        modifySTRef' mref (\mm -> take tcol mm ++ [(mm !! tcol) ++ [head]])
                        m6 <- readSTRef mref
                        when (length (m6 !! tcol) >= 2) $
                          modE w ((m6 !! tcol) !! (length (m6 !! tcol) - 2)) (\e -> e { e_foot = Just head })) [e_no bE .. length bcolCol - 1]
                    else pure ()
                  -- 复制循环
                  bE2 <- getE w bId'
                  tE5 <- getE w t'
                  m7 <- readSTRef mref
                  mapM_ (\i -> do
                    mapM_ (\j -> copyCloumn w mref bId' t' j i len ex) [e_cloumn bE2 + 1 .. e_cloumn tE5]) [0 .. n - 1]
                  mFinal <- readSTRef mref
                  mapM (valOf w . head) mFinal
 where
  lengthM mref = do
    mm <- readSTRef mref
    pure (length mm)
  when c act = if c then act else pure ()

-- ---------------------------------------------------------------------------
-- 公开入口
-- ---------------------------------------------------------------------------

expandXyV2 :: [Int] -> Int -> [Int]
expandXyV2 seq term
  | term < 1 || null seq = seq
  | otherwise = runST $ do
      let term' = min term 10
      w <- newWorld
      s <- toSequenceV12 w seq
      memo <- newSTRef []
      expandImpl w s term' dXY memo

xyV2Notation :: Notation
xyV2Notation =
  seqNotation "xy-v2" "X-Y V2" ["X-Y", "数列"] expandXyV2 ""
