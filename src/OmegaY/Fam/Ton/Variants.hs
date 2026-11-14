-- | 9 个 TON 变体的 standard_q / builtq 实现，配置化。
module OmegaY.Fam.Ton.Variants
  ( tonMConfig
  , tonIConfig
  , tonDrConfig
  , tonDrcConfig
  , tonDrpConfig
  , tonDrpcConfig
  , tonIbpConfig
  , tonMcConfig
  , tonMpcConfig
  ) where

import Control.Monad.Trans.State (State, get, put)
import qualified Data.Map.Strict as M

import OmegaY.Fam.Ton.TonCore

-- | std_memoized 的 State 版本。
stdMemo :: TonNode -> State TonState Bool -> State TonState Bool
stdMemo a compute
  | isLeaf a = return True
  | otherwise = do
      st <- get
      let key = flattenKey a
      case M.lookup key (tsMemo st) of
        Just _ -> return True
        Nothing -> do
          r <- compute
          if r
            then put st { tsMemo = M.insert key True (tsMemo st) }
            else return ()
          return r

-- | 通用的结构检查：a->r 标准 && a->l 标准 && (a->l 叶子 || compare(a->r, a->l->r) <= 0)
structOk :: (TonNode -> State TonState Bool) -> TonNode -> State TonState Bool
structOk stdQ a = do
  rOk <- stdQ (rightChild a)
  lOk <- stdQ (leftChild a)
  let cond = isLeaf (leftChild a)
              || compareRaise (rightChild a) (rightChild (leftChild a)) /= GT
  return (rOk && lOk && cond)

structOkN :: (TonV -> TonNode -> State TonState Bool) -> TonV -> TonNode -> State TonState Bool
structOkN stdQ sys a = do
  rOk <- stdQ sys (rightChild a)
  lOk <- stdQ sys (leftChild a)
  let cond = isLeaf (leftChild a)
              || compareRaise (rightChild a) (rightChild (leftChild a)) /= GT
  return (rOk && lOk && cond)

-- ======================================================================
-- ton-m (main)
-- ======================================================================

builtqMain :: TonV -> TonNode -> TonNode -> TonNode -> Bool
builtqMain n b a x
  | n /= 0 =
      builtqMain (n-1) b x x
      || (compareRaise x a /= GT &&
          (if isLeaf x
             then x >= zeroLeaf  -- x->v >= 0
             else builtqMain n b a (rightChild x) && builtqMain n b a (leftChild x)))
  | otherwise = compareRaise a b == LT
  where
    zeroLeaf = TLeaf 0

standardQMain :: TonV -> TonNode -> State TonState Bool
standardQMain n a = stdMemo a $ do
  ok <- structOkN standardQMain n a
  if not ok then return False
  else return (builtqMain n a (rightChild a) (rightChild a))

tonMConfig :: TonConfig
tonMConfig = TonConfig
  { tcMainStyle = True
  , tcIbpLimit = False
  , tcMark = Just markMain
  , tcStdFn = \a sys -> standardQMain sys a
  }

-- ======================================================================
-- ton-i
-- ======================================================================

builtqI :: TonNode -> TonNode -> TonV -> TonNode -> Bool
builtqI a b n x
  | n /= 0 =
      if isLeaf x
        then let v = leafV x
             in if v < 0 then builtqI x b (n-1) x
                else if v == 0 then True
                else False
        else (compareNoraise x zeroLeaf == LT && builtqI x b (n-1) x)
             || ((compareNoraise x zeroLeaf /= LT || compareNoraise x a /= GT)
                 && builtqI a b n (leftChild x) && builtqI a b n (rightChild x))
  | otherwise = compareNoraise a b == LT
  where
    leafV (TLeaf v) = v
    leafV _ = 0

standardQI :: TonNode -> State TonState Bool
standardQI a = stdMemo a $ do
  ok <- structOk standardQI a
  if not ok then return False
  else do
    let smallPaths = collectSmallindex (rightChild a)
    andM [ return (builtqI ex a (fromIntegral nv :: TonV) ex)
         | path <- smallPaths
         , let ex = extractPath (rightChild a) path
         , let a2 = getA2 (rightChild a) path
         , let (nv, _) = getN a2
         ]

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (m:ms) = do
  r <- m
  if r then andM ms else return False

tonIConfig :: TonConfig
tonIConfig = TonConfig
  { tcMainStyle = False
  , tcIbpLimit = True
  , tcMark = Nothing
  , tcStdFn = \a _ -> standardQI a
  }

-- ======================================================================
-- ton-dor
-- ======================================================================

builtqDor :: TonNode -> TonNode -> TonNode -> Bool
builtqDor a b x
  | isLeaf x =
      let v = leafV x
      in if v < 0
           then compareNoraise x b == LT || (compareNoraise x a /= GT && False)
           else v == 0
  | compareNoraise x zeroLeaf == LT =
      compareNoraise x b == LT
      || (compareNoraise x a /= GT && builtqDor a b (leftChild x) && builtqDor a b (rightChild x))
  | otherwise = builtqDor a b (leftChild x) && builtqDor a b (rightChild x)
  where
    leafV (TLeaf v) = v
    leafV _ = 0

standardQDor :: TonNode -> State TonState Bool
standardQDor a = stdMemo a $ do
  ok <- structOk standardQDor a
  if not ok then return False
  else do
    let smallTerms = collectSmallpart (rightChild a)
    andM [ return (builtqDor x a x) | x <- smallTerms ]

tonDrConfig :: TonConfig
tonDrConfig = TonConfig
  { tcMainStyle = False
  , tcIbpLimit = False
  , tcMark = Nothing
  , tcStdFn = \a _ -> standardQDor a
  }

-- ======================================================================
-- ton-drc
-- ======================================================================

builtqDrc :: TonNode -> TonNode -> TonNode -> TonNode -> TonNode -> Bool
builtqDrc a ap b x xp
  | isLeaf x =
      let v = leafV x
      in if v < 0
           then compareNoraise x b == LT || (compareNoraise x a /= GT && False)
           else v == 0
  | compareNoraise x zeroLeaf == LT =
      let rxxp = rConfig x xp
          raap = rConfig a ap
      in compareNoraise x b == LT
         || (compareNoraise rxxp raap /= GT
             && builtqDrc a ap b (leftChild x) x
             && builtqDrc a ap b (rightChild x) x)
  | otherwise = builtqDrc a ap b (leftChild x) xp && builtqDrc a ap b (rightChild x) xp
  where
    leafV (TLeaf v) = v
    leafV _ = 0

standardQDrc :: TonNode -> State TonState Bool
standardQDrc a = stdMemo a $ do
  ok <- structOk standardQDrc a
  if not ok then return False
  else do
    let smallTerms = collectSmallpart (rightChild a)
    andM [ return (builtqDrc x a a x a) | x <- smallTerms ]

tonDrcConfig :: TonConfig
tonDrcConfig = TonConfig
  { tcMainStyle = False
  , tcIbpLimit = False
  , tcMark = Nothing
  , tcStdFn = \a _ -> standardQDrc a
  }

-- ======================================================================
-- ton-drp
-- ======================================================================

builtqDrp :: TonNode -> TonNode -> TonNode -> TonNode -> TonNode -> Bool
builtqDrp a ai b a0 d
  | isLeaf a =
      let v = leafV a
      in if v == 0 then True else compareNoraise a b == LT
  | compareNoraise a b == LT = True
  | d == negOneLeaf && compareNoraise a zeroLeaf == LT && compareNoraise a ai == GT = False
  | compareNoraise a d == LT = builtqDrp a ai b a0 negOneLeaf
  | d == negOneLeaf && compareNoraise (leftChild a) zeroLeaf == LT && compareNoraise (rightChild a) a0 == LT =
      builtqDrp a ai b a0 a
  | otherwise = builtqDrp (leftChild a) ai b a0 d && builtqDrp (rightChild a) ai b a0 d
  where
    leafV (TLeaf v) = v
    leafV _ = 0

standardQDrp :: TonNode -> State TonState Bool
standardQDrp a = stdMemo a $ do
  ok <- structOk standardQDrp a
  if not ok then return False
  else do
    let smallTerms = collectSmallpart (rightChild a)
    andM [ return (builtqDrp x x a (rightChild a) negOneLeaf) | x <- smallTerms ]

tonDrpConfig :: TonConfig
tonDrpConfig = TonConfig
  { tcMainStyle = False
  , tcIbpLimit = False
  , tcMark = Nothing
  , tcStdFn = \a _ -> standardQDrp a
  }

-- ======================================================================
-- ton-drpc
-- ======================================================================

builtqDrpc :: TonNode -> TonNode -> TonNode -> TonNode -> TonNode -> TonNode -> Bool
builtqDrpc a ap ai b a0 d
  | isLeaf a =
      let v = leafV a
      in if v == 0 then True else compareNoraise a b == LT
  | compareNoraise a b == LT = True
  | d == negOneLeaf && compareNoraise a zeroLeaf == LT =
      let raap = rConfig a ap
          raib = rConfig ai b
      in if compareNoraise raap raib == GT
           then False
           else recurse
  | otherwise = recurse
  where
    leafV (TLeaf v) = v
    leafV _ = 0
    recurse =
      if compareNoraise a d == LT
        then builtqDrpc a ap ai b a0 negOneLeaf
        else
          let x2 = if compareNoraise a zeroLeaf == LT then a else ap
          in if d == negOneLeaf && compareNoraise (leftChild a) zeroLeaf == LT
               then let r1 = rConfig (rightChild a) x2
                        r2 = rConfig a0 b
                    in if compareNoraise r1 r2 == LT
                         then builtqDrpc a ap ai b a0 a
                         else builtqDrpc (leftChild a) x2 ai b a0 d
                              && builtqDrpc (rightChild a) x2 ai b a0 d
               else builtqDrpc (leftChild a) x2 ai b a0 d
                    && builtqDrpc (rightChild a) x2 ai b a0 d

standardQDrpc :: TonNode -> State TonState Bool
standardQDrpc a = stdMemo a $ do
  ok <- structOk standardQDrpc a
  if not ok then return False
  else do
    let smallTerms = collectSmallpart (rightChild a)
    andM [ return (builtqDrpc x a x a (rightChild a) negOneLeaf) | x <- smallTerms ]

tonDrpcConfig :: TonConfig
tonDrpcConfig = TonConfig
  { tcMainStyle = False
  , tcIbpLimit = False
  , tcMark = Nothing
  , tcStdFn = \a _ -> standardQDrpc a
  }

-- ======================================================================
-- ton-ibp
-- ======================================================================

builtqIbp :: TonNode -> TonNode -> [TonV] -> TonV -> Bool
builtqIbp a b c n
  | n == 0 = compareNoraise a b == LT
  | otherwise = all ok (collectSubtermIndex a)
  where
    ok x =
      let ex = extractPath a x
      in if compareNoraise ex a /= GT then True
         else if compareNoraise ex zeroLeaf == GT then True
         else if builtqIbp ex b c (n-1) then True
         else checkY x ex
    checkY x ex =
      let yindices = [0 .. length x - 1]
      in any (yCheck x ex) yindices
    yCheck x ex yindex =
      let y = take yindex x
          ey = extractPath a y
      in if compareNoraise ey zeroLeaf == GT then False
         else if builtqIbp ey b c (n-1) then True
         else if isLeaf ey then False
         else if compareNoraiseVec (rightChild ey) c /= LT then False
         else zCheck x yindex ey
    zCheck x yindex ey =
      let xLen = length x
          zPaths = [take k x | k <- [xLen, xLen-1 .. yindex]]
      in all (\z -> let ez = extractPath a z
                    in not (compareNoraise ez ey == LT)) zPaths

standardQIbp :: TonNode -> State TonState Bool
standardQIbp a = stdMemo a $ do
  ok <- structOk standardQIbp a
  if not ok then return False
  else do
    let smallPaths = collectSmallindex (rightChild a)
    andM [ let ex = extractPath (rightChild a) p
               a2 = getA2 (rightChild a) p
               (nv, _) = getN a2
           in return (builtqIbp ex a a2 (fromIntegral nv :: TonV))
         | p <- smallPaths ]

tonIbpConfig :: TonConfig
tonIbpConfig = TonConfig
  { tcMainStyle = False
  , tcIbpLimit = True
  , tcMark = Nothing
  , tcStdFn = \a _ -> standardQIbp a
  }

-- ======================================================================
-- ton-mc / ton-mpc
-- ======================================================================

extractparent :: TonNode -> TonNode -> [Int] -> TonNode
extractparent a b x =
  if null x then b else extractPath a (init x)

refreshTotest :: TonNode -> TonNode -> [Int] -> [Int] -> [[Int]]
refreshTotest a b d e =
  let ex = extractPath a d
  in if isLeaf ex || compareRaise ex (extractparent a b e) == LT
       then []
       else d : refreshTotest a b (d ++ [0]) e ++ refreshTotest a b (d ++ [1]) e

builtqMc :: TonNode -> TonNode -> TonV -> Bool
builtqMc a b n
  | not (n > 0) = compareRaise a b == LT
  | otherwise =
      let subterms = collectSubtermIndex a
      in all ok subterms
  where
    ok x =
      let ex = extractPath a x
      in if compareRaise (rConfig ex (extractparent a b x)) (rConfig a b) == GT
           then
             let some = any (\zi -> let pre = take zi x
                                        preNode = extractPath a pre
                                    in compareRaise preNode b == LT) [0 .. length x - 1]
             in if some then True
                else
                  let totest = refreshTotest a b x x
                      found = searchY x totest
                  in found
           else True
    searchY x totest =
      let yCandidates = takeWhile (not . null) (reverse (tails x))
      in any (checkY x totest) yCandidates
    checkY x totest y =
      let parent = extractparent a b y
          cond1 = and [ compareRaise (extractPath a (take (length y + dz) x)) parent /= LT
                     | dz <- [0 .. length x - length y - 1] ]
          cond2 = and [ compareRaise (extractPath a z) parent /= LT
                      | z <- totest ]
      in cond1 && cond2 && builtqMc (extractPath a y) parent (n-1)

standardQMc :: TonV -> TonNode -> State TonState Bool
standardQMc n a = stdMemo a $ do
  ok <- structOkN standardQMc n a
  if not ok then return False
  else return (builtqMc (rightChild a) a n)

tonMcConfig :: TonConfig
tonMcConfig = TonConfig
  { tcMainStyle = True
  , tcIbpLimit = False
  , tcMark = Just markMc
  , tcStdFn = \a sys -> standardQMc sys a
  }

builtqMpc :: TonNode -> TonNode -> TonNode -> TonV -> Bool
builtqMpc a b rc n
  | not (n > 0) = compareRaise a b == LT
  | otherwise =
      let subterms = collectSubtermIndex a
      in all ok subterms
  where
    ok x =
      let ex = extractPath a x
      in if compareRaise (rConfig ex (extractparent a b x)) (rConfig a b) == GT
           then
             let some = any (\zi -> let pre = take zi x
                                        preNode = extractPath a pre
                                    in compareRaise preNode b == LT) [0 .. length x - 1]
             in if some then True
                else
                  let totest = refreshTotest a b x x
                      found = searchY x totest
                  in found
           else True
    searchY x totest =
      let yCandidates = takeWhile (not . null) (reverse (tails x))
      in any (checkY x totest) yCandidates
    checkY x totest y =
      let parent = extractparent a b y
          cond1 = and [ compareRaise (extractPath a (take (length y + dz) x)) parent /= LT
                     | dz <- [0 .. length x - length y - 1] ]
          cond2 = and [ compareRaise (extractPath a z) parent /= LT
                      | z <- totest ]
      in cond1 && cond2 &&
           (compareRaise (rConfig (extractPath a y) parent) rc == LT
            || builtqMpc (extractPath a y) parent rc (n-1))

standardQMpc :: TonV -> TonNode -> State TonState Bool
standardQMpc n a = stdMemo a $ do
  let rc = rConfig (rightChild a) a
  ok <- structOkN standardQMpc n a
  if not ok then return False
  else return (builtqMpc (rightChild a) a rc n)

tonMpcConfig :: TonConfig
tonMpcConfig = TonConfig
  { tcMainStyle = True
  , tcIbpLimit = False
  , tcMark = Just markMc
  , tcStdFn = \a sys -> standardQMpc sys a
  }

-- | tails：所有后缀（包括空）。
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:rest) = xs : tails rest
