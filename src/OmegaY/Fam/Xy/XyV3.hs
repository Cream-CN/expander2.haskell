-- | xy-v3（X-Y V3）：忠实移植 C++ @notation/xy-v3.hpp@。
--   用 ST + IntMap 模拟 C++ 的 deque Arena 与裸指针图结构。
module OmegaY.Fam.Xy.XyV3
  ( xyV3Notation
  , expandXyV3
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.STRef
  ( STRef
  , modifySTRef'
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import OmegaY.Notation.Types (Notation, seqNotation)

-- Entry 与 Arena
data V3 = V3
  { vValue :: Int
  , vRow :: [Int]
  , vCloumn :: Int
  , vIdx :: Int
  , vNo :: Int
  , vId :: Int
  , vParent :: Maybe Int
  , vHead :: Maybe Int
  , vFoot :: Maybe Int
  , vRef :: Maybe Int
  , vOffset :: [[Int]]
  }

data World s = World
  { wAr :: STRef s (IntMap V3)
  , wNext :: STRef s Int
  }

newWorld :: ST s (World s)
newWorld = do
  ar <- newSTRef IM.empty
  nx <- newSTRef 0
  pure (World ar nx)

alloc :: World s -> V3 -> ST s Int
alloc w e = do
  i <- readSTRef (wNext w)
  writeSTRef (wNext w) (i + 1)
  modifySTRef' (wAr w) (IM.insert i e)
  pure i

getN :: World s -> Int -> ST s V3
getN w i = do
  m <- readSTRef (wAr w)
  pure (IM.findWithDefault (V3 0 [0] (-1) 0 0 0 Nothing Nothing Nothing Nothing []) i m)

modN :: World s -> Int -> (V3 -> V3) -> ST s ()
modN w i f = modifySTRef' (wAr w) (IM.adjust f i)

makeRoot :: V3
makeRoot = V3 0 [1] (-1) 0 0 0 Nothing Nothing Nothing Nothing []

-- 行运算（v3 版）
atI :: [Int] -> Int -> Int
atI xs i = if i >= 0 && i < length xs then xs !! i else 0

setAt :: Int -> a -> [a] -> [a]
setAt i x xs =
  if i >= 0 && i < length xs then take i xs ++ [x] ++ drop (i + 1) xs else xs

rowGenerator :: Int -> [Int]
rowGenerator n = 1 : replicate (max 0 (n - 1)) 0

compareRowV3 :: [Int] -> [Int] -> Int
compareRowV3 = go
 where
  go [] [] = 0
  go (a : as) (b : bs) =
    case compare a b of
      LT -> -1
      GT -> 1
      EQ -> go as bs
  go [] _ = -1
  go _ [] = 1

compareDimensionV3 :: [Int] -> [Int] -> Int
compareDimensionV3 r1 r2 = a - b
 where
  a = if length r1 <= 1 then 1 else r1 !! 1
  b = if length r2 <= 1 then 1 else r2 !! 1

rowAdditionV3 :: [Int] -> [Int] -> [Int]
rowAdditionV3 r1 r2
  | length r2 <= 1 || r2 !! 1 == 1 = r1 ++ r2
  | length r1 <= 1 || r1 !! 1 < r2 !! 1 = r2
  | otherwise = take i r1 ++ (if length r2 > 1 then drop 1 r2 else [])
 where
  i = findI 1
  findI k =
    let k' = k + 1
     in if k' >= length r1 then k' else if r1 !! k' < r2 !! 1 then k' else findI k'

rowDifferenceV3 :: [Int] -> [Int] -> [Int]
rowDifferenceV3 r1 r2
  | compareRowV3 r1 r2 <= 0 = []
  | length r1 > 1 && r1 !! 1 == 1 =
      if length r1 >= length r2 then take (length r1 - length r2) r1 else []
  | otherwise =
      if i < length r1 && r1 !! i == 1
        then drop i r1
        else 1 : (if i < length r1 then drop i r1 else [])
 where
  i = findI 0
  findI k
    | k < length r1 && k < length r2 = if r1 !! k > r2 !! k then k else findI (k + 1)
    | otherwise = k

divideV3 :: [Int] -> [[Int]]
divideV3 d = ret ++ tailPart
 where
  dd = if length d > 1 then drop 1 d else []
  ret = concat [replicate (dd !! i) (rowGenerator (length dd - i)) | i <- [0 .. length dd - 2]]
  tailPart = if not (null dd) && last dd /= 0 then [[last dd]] else []

mergeV3 :: [[Int]] -> [Int]
mergeV3 d
  | null d = []
  | otherwise = 0 : ret3
 where
  ret0 = rowGenerator (length (head d))
  ret1 = setAt 0 (head ret0 - 1) ret0
  ret2 = foldl step ret1 [0 .. length d - 2]
  step r i = let k = length r - length (d !! i) in setAt k (atI r k + 1) r
  ret3 = let k = length ret2 - length (last d) in setAt k (atI ret2 k + head (last d)) ret2

getFootRowV3 :: Maybe [Int] -> [Int] -> [Int] -> [Int]
getFootRowV3 mParentRow row d =
  let row0 = case mParentRow of
        Nothing -> []
        Just pr -> rowDifferenceV3 row pr
   in if null row0
        || null d
        || (length d == 1 && d !! 0 == 0)
        || (length d == 2 && d !! 0 == 0)
        || (length d == 3 && d !! 0 == 1 && d !! 1 == 0 && d !! 2 == 1)
        then rowAdditionV3 row [1]
        else if length row0 == 1
          then rowAdditionV3 row [1, 2]
          else rowAdditionV3 row [1, row0 !! 1 + 1]

-- 图结构处理
setElementRefrence :: World s -> STRef s [[Int]] -> ST s ()
setElementRefrence w mRef = do
  m <- readSTRef mRef
  forM_ [0 .. length m - 1] $ \i ->
    forM_ [0 .. length (m !! i) - 1] $ \j -> do
      let eid = (m !! i) !! j
      e <- getN w eid
      when (not (length (vRow e) <= 1 && vValue e <= 1)) $
        case vParent e of
          Just p -> do
            pe <- getN w p
            if compareRowV3 (vRow e) (vRow pe) == 0
              then modN w eid (\x -> x {vRef = Just p})
              else fromHead eid e
          Nothing -> fromHead eid e
 where
  fromHead eid e = case vHead e of
    Nothing -> pure ()
    Just h -> do
      he <- getN w h
      case vParent he of
        Nothing -> pure ()
        Just hp -> do
          hpe <- getN w hp
          case vFoot hpe of
            Just f -> do
              fe <- getN w f
              if compareRowV3 (vRow fe) (vRow e) <= 0
                then do
                  modN w eid (\x -> x {vRef = Just f})
                  chase eid e
                else modN w eid (\x -> x {vRef = Just hp})
            Nothing -> modN w eid (\x -> x {vRef = Just hp})
  chase eid e = do
    let go cur = case cur of
          Nothing -> pure Nothing
          Just rid -> do
            r <- getN w rid
            if compareRowV3 (vRow r) (vRow e) == 0 then go (vRef r) else pure cur
    r <- go (vRef e)
    modN w eid (\x -> x {vRef = r})

setElementNo :: World s -> STRef s [[Int]] -> Maybe Int -> ST s ()
setElementNo w mRef mb = do
  bCol <- case mb of
    Nothing -> pure (-1)
    Just b -> vCloumn <$> getN w b
  idRef <- newSTRef 0
  m <- readSTRef mRef
  forM_ [0 .. length m - 1] $ \i ->
    forM_ [0 .. length (m !! i) - 1] $ \j -> do
      let eid = (m !! i) !! j
      e <- getN w eid
      if i == bCol
        then modN w eid (\x -> x {vNo = j + 1})
        else if i < bCol || (vValue e <= 1 && j == 0)
          then modN w eid (\x -> x {vNo = 0})
          else case vRef e of
            Nothing -> modN w eid (\x -> x {vNo = 0})
            Just r -> do
              re <- getN w r
              modN w eid (\x -> x {vNo = vNo re})
      when (i > bCol) $ do
        c <- readSTRef idRef
        modN w eid (\x -> x {vId = c})
        writeSTRef idRef (c + 1)
      when (i == bCol || i == length m - 1) $ do
        e2 <- getN w eid
        modN w eid (\x -> x {vId = vNo e2})

getReferenceChain :: World s -> Int -> ST s [Int]
getReferenceChain w eid = go [] (Just eid)
 where
  go acc Nothing = pure (reverse acc)
  go acc (Just i) = do
    e <- getN w i
    let acc' = i : acc
    if vValue e <= 1 && length (vRow e) == 1
      then pure (reverse acc')
      else go acc' (vRef e)

toSequenceV3 :: World s -> [Int] -> ST s [Int]
toSequenceV3 w s = go 0 []
 where
  n = length s
  go i acc
    | i >= n = pure (reverse acc)
    | otherwise = do
        let vi = s !! i
        if vi <= 1
          then do
            root <- alloc w makeRoot
            e <- alloc w (V3 vi [] i 0 0 0 (Just root) Nothing Nothing Nothing [])
            go (i + 1) (e : acc)
          else do
            let smaller = [j | j <- [i - 1, i - 2 .. 0], atI s j < vi]
            case smaller of
              (j : _) -> do
                e <- alloc w (V3 vi [] i 0 0 0 (Just (acc !! (i - 1 - j))) Nothing Nothing Nothing [])
                go (i + 1) (e : acc)
              [] -> do
                root <- alloc w makeRoot
                e <- alloc w (V3 vi [] i 0 0 0 (Just root) Nothing Nothing Nothing [])
                go (i + 1) (e : acc)

drawMountain :: World s -> [Int] -> [Int] -> ST s (STRef s [[Int]])
drawMountain w s d = do
  mRef <- newSTRef []
  forM_ (zip [0 ..] s) $ \(i, eid) -> do
    e <- getN w eid
    parent <- case vParent e of
      Just p -> do
        pe <- getN w p
        if vCloumn pe >= 0
          then do
            m <- readSTRef mRef
            pure (head (m !! vCloumn pe))
          else alloc w makeRoot
      Nothing -> alloc w makeRoot
    first <- alloc w (V3 (vValue e) [1] i 0 0 0 (Just parent) Nothing Nothing Nothing [])
    modifySTRef' mRef (++ [[first]])
  m0 <- readSTRef mRef
  let climb i = do
        m <- readSTRef mRef
        let loop itId = do
              it <- getN w itId
              when (vValue it > 1) $ do
                mp <- traverse (getN w) (vParent it)
                let pv = maybe 0 vValue mp
                    footVal = vValue it - pv
                when (footVal < vValue it) $ do
                  let fr = getFootRowV3 (vRow <$> mp) (vRow it) d
                  foot <- alloc w (V3 footVal fr i (vIdx it + 1) 0 0 Nothing (Just itId) Nothing Nothing [])
                  modifySTRef' mRef (\xs -> take i xs ++ [xs !! i ++ [foot]] ++ drop (i + 1) xs)
                  p1 <- case vParent it of
                    Nothing -> pure Nothing
                    Just p -> do
                      pe <- getN w p
                      case vFoot pe of
                        Just f -> do
                          fe <- getN w f
                          if compareRowV3 (vRow fe) fr <= 0 then pure (Just f) else pure (Just p)
                        Nothing -> pure (Just p)
                  p2 <- climbParent p1 footVal
                  modN w foot (\x -> x {vParent = p2})
                  loop foot
        loop (head (m !! i))
      climbParent p fv = case p of
        Nothing -> pure Nothing
        Just pid -> do
          pe <- getN w pid
          if vValue pe >= fv then climbParent (vParent pe) fv else pure (Just pid)
  forM_ [0 .. length m0 - 1] climb
  pure mRef

getOds :: World s -> [[Int]] -> ST s [Int]
getOds w m = go m []
 where
  go [] acc = pure (reverse acc)
  go (col : rest) acc = do
    le <- getN w (last col)
    fe <- getN w (head col)
    parent <- if vValue le <= 1
      then alloc w makeRoot
      else case vParent le of
        Just p -> do
          pe <- getN w p
          if vCloumn pe >= 0 then pure (acc !! vCloumn pe) else alloc w makeRoot
        Nothing -> alloc w makeRoot
    e <- alloc w (V3 (vValue le) [1] (vCloumn fe) 0 0 0 (Just parent) Nothing Nothing Nothing [])
    go rest (acc ++ [e])

getMds :: World s -> [[Int]] -> ST s [Int]
getMds w m = do
  chain <- getReferenceChain w (last (last m))
  seqv <- mapM
    (\eid -> do e <- getN w eid; pure (if length (vRow e) <= 1 then 1 else vRow e !! 1))
    chain
  toSequenceV3 w seqv

getBootIndex :: World s -> [Int] -> [Int] -> ST s (Int, Int)
getBootIndex w s d = do
  mRef <- drawMountain w s d
  setElementRefrence w mRef
  m <- readSTRef mRef
  let t0 = last (last m)
  t <- do
    e <- getN w t0
    case vHead e of
      Just h | vValue e == 1 -> pure h
      _ -> pure t0
  te <- getN w t
  case vParent te of
    Just b -> do
      be <- getN w b
      if vValue te - vValue be > 1 && length d == 1
        then do
          o <- getOds w m
          let subD = if length d == 1 || length (divideV3 d) == 1 then d else mergeV3 (tail (divideV3 d))
          boot <- getBootIndex w o subD
          m2 <- readSTRef mRef
          pure (fst boot, length (m2 !! fst boot) - 1)
        else if compareDimensionV3 (vRow be) (vRow te) < 0
          then do
            ch <- getReferenceChain w t
            mds <- getMds w m
            let dd = if length d == 3 && d !! 0 == 1 && d !! 1 == 0 && d !! 2 == 1 then d else [0, 1]
            boot <- getBootIndex w mds dd
            ce <- getN w (ch !! fst boot)
            m2 <- readSTRef mRef
            let c = vCloumn ce
            topE <- getN w (last (m2 !! c))
            pure (c, vIdx topE)
          else pure (vCloumn be, vIdx be)
    Nothing -> pure (-1, 0)

-- 复制与展开
getFootRowFor :: World s -> Int -> [Int] -> ST s [Int]
getFootRowFor w eid d = do
  e <- getN w eid
  mp <- traverse (getN w) (vParent e)
  pure (getFootRowV3 (vRow <$> mp) (vRow e) d)

copyElement :: World s -> STRef s [[Int]] -> Maybe Int -> Int -> Int -> [Int] -> ST s ()
copyElement w mRef mb _tId itId d = do
  bCol <- case mb of
    Nothing -> pure (-1)
    Just b -> vCloumn <$> getN w b
  it0 <- getN w itId
  when (vValue it0 <= 1 && length (vRow it0) > 1) $ do
    modN w itId (\x -> x {vParent = vRef x})
    fixParent itId
  mm <- readSTRef mRef
  it <- getN w itId
  minRow <-
    if length (vRow it) > 1 && not (null (last mm))
      then getFootRowFor w (last (last mm)) d
      else pure [1]
  build bCol it minRow (vRow it)
 where
  fixParent eid = do
    e <- getN w eid
    case vParent e of
      Just p -> do
        pe <- getN w p
        when (vValue pe > 1) $ do
          modN w eid (\x -> x {vParent = vRef pe})
          fixParent eid
      Nothing -> pure ()
  build bCol it row maxRow
    | compareRowV3 row maxRow > 0 = pure ()
    | otherwise = do
        m <- readSTRef mRef
        let lastCol = last m
            newCl = length m - 1
        eid <- alloc w (V3 (vValue it) row newCl (length lastCol) (vNo it) (vId it)
                        Nothing Nothing Nothing Nothing [])
        modifySTRef' mRef (\xs -> init xs ++ [last xs ++ [eid]])
        when (not (null lastCol)) $ do
          let prev = last lastCol
          modN w eid (\x -> x {vHead = Just prev})
          modN w prev (\x -> x {vFoot = Just eid})
        pcol <- case vParent it of
          Nothing -> pure (-1)
          Just p -> vCloumn <$> getN w p
        let pc = if pcol >= bCol then length m - 1 + pcol - vCloumn it else pcol
        p0 <- do
          m2 <- readSTRef mRef
          if pc >= 0 && pc < length m2 && not (null (m2 !! pc))
            then pure (Just (last (m2 !! pc)))
            else Just <$> alloc w makeRoot
        pFinal <- descend p0 row
        modN w eid (\x -> x {vParent = pFinal})
        row' <- getFootRowFor w eid d
        build bCol it row' maxRow
  descend p row = case p of
    Nothing -> pure Nothing
    Just pid -> do
      pe <- getN w pid
      if compareRowV3 (vRow pe) row > 0 then descend (vHead pe) row else pure (Just pid)

copyCloumn :: World s -> STRef s [[Int]] -> Maybe Int -> Int -> Int -> Int -> [Int] -> [Int] -> ST s ()
copyCloumn w mRef mb tId c _i ex d = do
  mm <- readSTRef mRef
  let it0 = head (mm !! c)
  modifySTRef' mRef (++ [[]])
  finalIt <- step it0
  m2 <- readSTRef mRef
  let newIdx = length m2 - 1
      newCol = last m2
  fe <- getN w finalIt
  let val = if not (null ex) && newIdx < length ex then ex !! newIdx else vValue fe
  modN w (last newCol) (\x -> x {vValue = val})
  propagate (last newCol)
 where
  step itId = do
    copyElement w mRef mb tId itId d
    e <- getN w itId
    case vFoot e of
      Just f -> step f
      Nothing -> pure itId
  propagate itId = do
    e <- getN w itId
    case vHead e of
      Nothing -> pure ()
      Just h -> do
        he <- getN w h
        case vParent he of
          Nothing -> pure ()
          Just p -> do
            pe <- getN w p
            modN w h (\x -> x {vValue = vValue e + vValue pe})
        propagate h

expandDimensionSequnece :: World s -> STRef s [[Int]] -> Int -> Int -> Int -> [Int] -> ST s ()
expandDimensionSequnece w mRef bId tId n d = do
  b <- getN w bId
  t <- getN w tId
  when (compareDimensionV3 (vRow b) (vRow t) < 0) $ do
    let dd = if length d == 3 && d !! 0 == 1 && d !! 1 == 0 && d !! 2 == 1 then d else [0, 1]
    m <- readSTRef mRef
    s <- getMds w m
    c <- fst <$> getBootIndex w s dd
    bu <- getN w bId
    let tCol = vCloumn t
        go i
          | i > tCol = pure ()
          | otherwise = do
              m2 <- readSTRef mRef
              if i < 0 || i >= length m2
                then go (i + 1)
                else do
                  stop <- goJ bu i tCol 0 (length (m2 !! i)) s c dd
                  if stop then pure () else go (i + 1)
        goJ bu i tCol j len s c dd
          | j >= len = pure False
          | i == tCol && j == len - 1 = pure True
          | otherwise = do
              let next = goJ bu i tCol (j + 1) len s c dd
              m3 <- readSTRef mRef
              let itId = (m3 !! i) !! j
              it <- getN w itId
              if vNo it /= vNo bu
                then next
                else case vRef it of
                  Nothing -> next
                  Just refId -> do
                    refE <- getN w refId
                    let diffRow = rowDifferenceV3 (vRow it) (vRow refE)
                        val0 = if length diffRow < 2 then 1 else diffRow !! 1
                    if compareDimensionV3 diffRow (vRow bu) <= 0
                      then next
                      else do
                        ne <- alloc w (V3 val0 [] (c + 1) 0 0 0 (Just (s !! c)) Nothing Nothing Nothing [])
                        let ds0 = take (c + 1) s ++ [ne] ++ drop (c + 1) s
                        (ds1, pos) <- upchain w bu itId ds0 1
                        forM_ (zip [0 ..] ds1) $ \(k, eid) -> do
                          modN w eid (\x -> x {vCloumn = k})
                          fixParentUp w eid
                        modN w itId (\x -> x {vOffset = []})
                        let len0 = length ds1 - 1 - c
                        ex <- expand_impl w ds1 n dd False
                        let offsets =
                              [ [1, if idx >= 0 && idx < length ex then ex !! idx else 0]
                              | ii <- [0 .. n - 1]
                              , let idx = c + len0 * (ii + 1) + pos
                              ]
                        modN w itId (\x -> x {vOffset = offsets})
                        next
    go (vCloumn b + 1)

upchain :: World s -> V3 -> Int -> [Int] -> Int -> ST s ([Int], Int)
upchain w bu itId ds pos = do
  e <- getN w itId
  case vRef e of
    Nothing -> pure (ds, pos)
    Just rid -> do
      re <- getN w rid
      if vCloumn re > vCloumn bu
        then do
          mre <- traverse (getN w) (vRef re)
          let dr = case mre of
                Just re2 -> rowDifferenceV3 (vRow re) (vRow re2)
                Nothing -> []
          if compareDimensionV3 dr (vRow bu) <= 0
            then upchain w bu rid ds pos
            else do
              let c = vCloumn bu
                  val = if length dr < 2 then 1 else dr !! 1
              ne <- alloc w (V3 val [] (c + 1) 0 0 0 (Just (ds !! c)) Nothing Nothing Nothing [])
              let ds1 = take (c + 1) ds ++ [ne] ++ drop (c + 1) ds
              when (c + 2 < length ds1) $
                modN w (ds1 !! (c + 2)) (\x -> x {vParent = Just (ds1 !! (c + 1))})
              upchain w bu rid ds1 (pos + 1)
        else pure (ds, pos)

fixParentUp :: World s -> Int -> ST s ()
fixParentUp w eid = do
  e <- getN w eid
  case vParent e of
    Nothing -> pure ()
    Just p -> do
      pe <- getN w p
      if vValue e <= vValue pe
        then do
          modN w eid (\x -> x {vParent = vParent pe})
          fixParentUp w eid
        else pure ()

expand_impl :: World s -> [Int] -> Int -> [Int] -> Bool -> ST s [Int]
expand_impl w s n d f = do
  if null s
    then pure []
    else do
      le <- getN w (last s)
      if vValue le <= 1
        then mapM (fmap vValue . getN w) (init s)
        else do
          (bi, bj) <- getBootIndex w s d
          mRef <- drawMountain w s d
          m0 <- readSTRef mRef
          let bId = (m0 !! bi) !! bj
              t0 = last (last m0)
          tId <- do
            e <- getN w t0
            case vHead e of
              Just h | vValue e == 1 -> pure h
              _ -> pure t0
          setElementRefrence w mRef
          setElementNo w mRef (Just bId)
          expandDimensionSequnece w mRef bId tId n d
          b <- getN w bId
          t <- getN w tId
          let crossDim = vValue t - vValue b > 1 && length d == 1
          ex <- if crossDim
            then do
              o <- getOds w m0
              let subD = if length d == 1 || length (divideV3 d) == 1 then d else mergeV3 (tail (divideV3 d))
              expand_impl w o n subD False
            else do
              case vFoot t of
                Just _ -> do
                  modifySTRef' mRef (\xs -> init xs ++ [init (last xs)])
                  modN w tId (\x -> x {vFoot = Nothing})
                  case vParent t of
                    Just p -> do
                      pe <- getN w p
                      modN w tId (\x -> x {vParent = vParent pe})
                    Nothing -> pure ()
                Nothing -> pure ()
              pure []
          m1 <- readSTRef mRef
          forM_ (last m1) $ \eid -> modN w eid (\x -> x {vValue = vValue x - 1})
          let bCol = vCloumn b
              tCol = vCloumn t
          m2 <- readSTRef mRef
          when (bCol >= 0 && bCol < length m2 && tCol >= 0 && tCol < length m2) $ do
            let bColSz = length (m2 !! bCol)
                bNo = vNo b
                tIdx = vIdx t
            forM_ [bNo .. bColSz - 1] $ \i -> do
              src <- getN w ((m2 !! bCol) !! i)
              m3 <- readSTRef mRef
              let headId = last (last m3)
              eid <- alloc w
                (V3 (vValue src) (vRow src) tCol (tIdx + 1) (vNo src) (vNo src)
                   (vParent src) (Just headId) Nothing (Just ((m2 !! bCol) !! i)) [])
              modifySTRef' mRef (\xs -> init xs ++ [last xs ++ [eid]])
              m4 <- readSTRef mRef
              when (length (m4 !! tCol) >= 2) $ do
                let prev = (m4 !! tCol) !! (length (m4 !! tCol) - 2)
                modN w prev (\x -> x {vFoot = Just eid})
          forM_ [0 .. n - 1] $ \i ->
            forM_ [bCol + 1 .. tCol] $ \j -> do
              m5 <- readSTRef mRef
              when (j >= 0 && j < length m5) $ copyCloumn w mRef (Just bId) tId j i ex d
          m6 <- readSTRef mRef
          mapM
            (\col -> if null col then pure 0 else vValue <$> getN w (head col))
            m6

-- 入口
dXY :: [Int]
dXY = [1, 0, 1]

expandXyV3 :: [Int] -> Int -> [Int]
expandXyV3 seqv term
  | term < 1 || null seqv = seqv
  | otherwise = runST $ do
      w <- newWorld
      let term' = if term > 10 then 10 else term
      s <- toSequenceV3 w seqv
      expand_impl w s term' dXY False

xyV3Notation :: Notation
xyV3Notation =
  seqNotation "xy-v3" "X-Y V3" ["X-Y", "数列"] expandXyV3 ""
