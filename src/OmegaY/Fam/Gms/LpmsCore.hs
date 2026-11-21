-- | LPMS / LPTSS 族核心。对齐 C++ @lpms.hpp@。
--
--   二者共用同一展开引擎，仅极限基本列不同：
--     * LPMS   极限 = [零列, 一列全 1]
--     * LPTSS  极限 = 列 [i,i,i] for i=0..n
module OmegaY.Fam.Gms.LpmsCore
  ( runLpms
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import OmegaY.Fam.Gms.Common
import OmegaY.Matrix (matCols)

-- ---------------------------------------------------------------------
-- 基础列工具
-- ---------------------------------------------------------------------

maxRows :: FMat -> Int
maxRows m = foldr max 0 (map length m)

standardize :: FMat -> Int -> FMat
standardize m rows = [ [ val col r | r <- [0 .. rows - 1] ] | col <- m ]

onesCol :: Int -> Col
onesCol rows = replicate rows 1

constCol :: Int -> Int -> Col
constCol a rows
  | rows > 0 = a : replicate (rows - 1) 0
  | otherwise = []

-- | 列比较，缺省元素分别取 missA/missB。
colCompareMiss :: Col -> Col -> Int -> Int -> Int -> Int
colCompareMiss a b rows missA missB = go 0
 where
  go r
    | r >= rows = 0
    | av /= bv = compareI av bv
    | otherwise = go (r + 1)
   where
    av = if r < length a then a !! r else missA
    bv = if r < length b then b !! r else missB

legal :: FMat -> Bool
legal inp
  | null inp = True
  | otherwise =
      let rows = maxRows inp
          m = standardize inp rows
       in all (\r -> val (m !! 0) r == 0) [0 .. rows - 1] &&
          all
            (\c -> all (\r -> val (m !! c) r <= val (m !! c) (r - 1)) [1 .. rows - 1])
            [0 .. length m - 1]

lastNonzeroRow :: FMat -> Int
lastNonzeroRow m
  | null m = (-1)
  | otherwise =
      let lastCol = last m
          go r
            | r < 0 = (-1)
            | lastCol !! r /= 0 = r + 1
            | otherwise = go (r - 1)
       in go (length lastCol - 1)

lastAllZero :: FMat -> Bool
lastAllZero m
  | null m = True
  | otherwise = all (== 0) (last m)

-- ---------------------------------------------------------------------
-- 祖先链 / 父列（纯函数）
-- ---------------------------------------------------------------------

aAncestors :: FMat -> Int -> Int -> Int -> Int -> (Col, Set Int)
aAncestors m rows cols col a
  | a < 0 || a > rows || col < 0 || col >= cols = ([], Set.empty)
  | otherwise =
      let (lst, _, _) = go col Set.empty 0
       in (lst, Set.fromList lst)
 where
  go current mask guard
    | current == (-1) || guard > cols + 2 = ([], mask, guard)
    | Set.member current mask = ([], mask, guard)
    | otherwise =
        let mask' = Set.insert current mask
            next =
              if a == 0
                then (if current > 0 then current - 1 else (-1))
                else bParent m rows cols current a
            (rest, mask'', g'') = go next mask' (guard + 1)
         in (current : rest, mask'', g'')

bParent :: FMat -> Int -> Int -> Int -> Int -> Int
bParent m rows cols col b
  | b < 1 || b > rows || col < 0 || col >= cols = (-1)
  | otherwise =
      let row = b - 1
          value = val (m !! col) row
          (ancList, _) = aAncestors m rows cols col (b - 1)
          loop [] = (-1)
          loop (c : cs)
            | c >= col = loop cs
            | val (m !! c) row < value = c
            | otherwise = loop cs
       in loop ancList

findRoot :: FMat -> Int -> Int -> Maybe (Int, Int)
findRoot m rows cols =
  let t = lastNonzeroRow m
   in if t < 1
        then Nothing
        else
          let root = bParent m rows cols (cols - 1) t
           in if root < 0 then Nothing else Just (root, t)

computeDelta :: FMat -> Int -> Int -> Int -> Col
computeDelta m root t rows =
  let alpha = length m - 1
   in [ if r >= t - 1 then 0 else val (m !! alpha) r - val (m !! root) r
      | r <- [0 .. rows - 1] ]

maxEntryF :: FMat -> Int -> Int
maxEntryF m rows = maximum (0 : [ val col r | col <- m, r <- [0 .. rows - 1] ])

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

-- ---------------------------------------------------------------------
-- Prep
-- ---------------------------------------------------------------------

data Prep = Prep
  { pM :: FMat
  , pRows :: !Int
  , pCols :: !Int
  , pRoot :: !Int
  , pT :: !Int
  , pAlpha :: !Int
  , pG :: FMat
  , pB :: FMat
  , pDelta :: Col
  , pMaxTwice :: !Int
  , pVr :: [[Int]]
  }

getVr :: Prep -> Int -> Int -> Int
getVr prep c r
  | c >= pRoot prep && c < pAlpha prep && r < pT prep - 1 =
      (pVr prep) !! c !! r
  | otherwise = (-1)

containsRoot :: Prep -> Int -> Int -> Bool
containsRoot prep col rowLabel =
  let (_, mask) = aAncestors (pM prep) (pRows prep) (pCols prep) col rowLabel
   in Set.member (pRoot prep) mask

buildXy :: Prep -> Int -> Int -> (FMat, FMat, Int, Bool)
buildXy prep col k =
  let rows = pRows prep
      m = pM prep
      root = pRoot prep
      alpha = pAlpha prep
      maxTwice = pMaxTwice prep
      base =
        [ if r < k
            then val (m !! col) r + 1
            else if r == k then val (m !! col) r else 0
        | r <- [0 .. rows - 1] ]
      u = headOr (alpha + 1)
            [ c | c <- [col + 1 .. alpha]
            , colCompareMiss (m !! c) base rows 0 0 < 0 ]
      uMissing = u == alpha + 1
      ayk = val (m !! root) (k - 1)
      (alphaAnc, _) = aAncestors m rows alpha alpha k
      j = headOr alpha [ c | c <- alphaAnc, val (m !! c) (k - 1) == ayk + 1 ]
      xCols =
        [ [ let out = val (m !! c) r
             in if r <= k - 2 && getVr prep c r == 1
                  then out + maxTwice - val (m !! col) r
                  else out
          | r <- [0 .. rows - 1] ]
        | c <- [col .. u - 1]
        ]
      yCols =
        [ [ let out = val (m !! c) r
             in if r <= k - 2 &&
                   (c == j ||
                      let (_, mask) = aAncestors m rows alpha c (r + 1)
                       in Set.member j mask)
                  then out + maxTwice - val (m !! j) r
                  else out
          | r <- [0 .. rows - 1] ]
        | c <- [j .. alpha]
        ]
   in (xCols, yCols, u, uMissing)

matrixLex :: FMat -> FMat -> Int -> Int
matrixLex x y rows =
  let len = max (length x) (length y)
      miss = replicate rows (-1)
      colAt xx i = if i < length xx then xx !! i else miss
      go i
        | i >= len = 0
        | otherwise =
            let cmp = colCompareMiss (colAt x i) (colAt y i) rows (-1) (-1)
             in if cmp /= 0 then cmp else go (i + 1)
   in go 0

makePrep :: FMat -> Int -> Maybe Prep
makePrep m rows = do
  let cols = length m
  if cols == 0
    then Nothing
    else case findRoot m rows cols of
      Nothing -> Nothing
      Just (root, t) ->
        let alpha = cols - 1
            g = [ m !! c | c <- [0 .. root - 1] ]
            b = [ m !! c | c <- [root .. alpha - 1] ]
            delta = computeDelta m root t rows
            maxTwice = maxEntryF m rows * 2
            vr0 = [ [ (-1) | _r <- [0 .. max 1 rows - 1] ] | _c <- [0 .. cols - 1] ]
            prep0 = Prep m rows cols root t alpha g b delta maxTwice vr0
            vrFinal = buildVr prep0
         in Just prep0 { pVr = vrFinal }

buildVr :: Prep -> [[Int]]
buildVr prep = foldl applyRow (pVr prep) [0 .. pT prep - 2]
 where
  rows = pRows prep
  t = pT prep
  root = pRoot prep
  alpha = pAlpha prep
  cols = pCols prep
  m = pM prep
  applyRow curVr row =
    let newRow = foldl' (buildCol curVr row) (replicate cols (-1)) [root .. alpha - 1]
     in [ if c < root || c >= alpha
            then curVr !! c
            else
              [ if r == row then newRow !! c else curVr !! c !! r
              | r <- [0 .. rows - 1] ]
        | c <- [0 .. cols - 1] ]
   where
    -- C++ 按列从左到右写 vr[col][row]；has0/build_xy 需读到本行已写好的列。
    buildCol curVr row newSoFar c =
      let tempVr =
            [ if cc < root || cc >= alpha
                then curVr !! cc
                else
                  [ if r == row then newSoFar !! cc else curVr !! cc !! r
                  | r <- [0 .. rows - 1] ]
            | cc <- [0 .. cols - 1] ]
          tempPrep = prep { pVr = tempVr }
          v = decide tempPrep c
       in take c newSoFar ++ [v] ++ drop (c + 1) newSoFar
    decide tempPrep c =
      if c == root || row == 0
        then 1
        else
          let (ancList, ancMask) = aAncestors m rows cols c (row + 1)
              has0 = any (\a -> getVr tempPrep a row == 0) ancList
              parent = bParent m rows cols c (row + 1)
           in if not (Set.member root ancMask) || has0 || parent < 0
            then 0
            else
              if parent /= root
                then 1
                else
                  let earlier0 = any (\r -> getVr tempPrep c r == 0) [0 .. row - 1]
                   in if earlier0
                        then 0
                        else
                          let higherEscapes =
                                any (\r -> bParent m rows cols c (r + 1) /= root)
                                    [row + 1 .. t - 1]
                           in if higherEscapes
                                then 0
                                else
                                  let (xx, yy, u, uMissing) =
                                        buildXy tempPrep c (row + 1)
                                   in if u == alpha + 1 || uMissing
                                        then 1
                                        else if matrixLex xx yy rows < 0 then 0 else 1

bhs :: Prep -> Int -> FMat
bhs prep h =
  let rows = pRows prep
      b = pB prep
      root = pRoot prep
      t = pT prep
      delta = pDelta prep
   in [ [ let original = root + local
              factor =
                if r < t - 1 && getVr prep original r == 1 then 1 else 0
           in (b !! local) !! r + h * (delta !! r) * factor
         | r <- [0 .. rows - 1] ]
      | local <- [0 .. length b - 1] ]

-- ---------------------------------------------------------------------
-- upms_single / bms_fs
-- ---------------------------------------------------------------------

upmsSingle :: FMat -> Int -> Int -> Bool -> FMat
upmsSingle matrix l rows lFinite =
  let m = standardize matrix rows
   in if null m || lastAllZero m
        then [ m !! i | i <- [0 .. length m - 2] ]
        else case makePrep m rows of
          Nothing -> []
          Just prep ->
            let b1 = bhs prep 1
                takeN = min (length b1) (if lFinite then l else length b1)
                out = pG prep ++ pB prep ++ [ b1 !! i | i <- [0 .. takeN - 1] ]
             in standardize out rows

bmsFs :: FMat -> Int -> Int -> FMat
bmsFs matrix n rows =
  let m = standardize matrix rows
   in if null m
        then []
        else
          if lastAllZero m
            then [ m !! i | i <- [0 .. length m - 2] ]
            else case findRoot m rows (length m) of
              Nothing -> []
              Just (root, t) ->
                let alpha = length m - 1
                    g = [ m !! c | c <- [0 .. root - 1] ]
                    b = [ m !! c | c <- [root .. alpha - 1] ]
                    delta = computeDelta m root t rows
                    prepish =
                      Prep m rows (length m) root t alpha g b delta 0
                        [ [ (-1) | _ <- [0 .. rows - 1] ] | _ <- [0 .. length m - 1] ]
                    result =
                      g ++ b ++
                      [ [ let base = b !! local !! r
                              factor =
                                if r < t - 1 && containsRoot prepish (root + local) (r + 1)
                                  then 1
                                  else 0
                           in base + h * (delta !! r) * factor
                        | r <- [0 .. rows - 1] ]
                      | h <- [1 .. n]
                      , local <- [0 .. length b - 1]
                      ]
                 in standardize result rows

-- ---------------------------------------------------------------------
-- 阶段列辅助
-- ---------------------------------------------------------------------

phaseColumn :: FMat -> Int -> Int -> Col
phaseColumn m index rows
  | index >= 0 && index < length m = m !! index
  | otherwise = constCol (-1) rows

phaseCompare :: FMat -> Int -> Col -> Int -> Int
phaseCompare m index col rows =
  colCompareMiss (phaseColumn m index rows) col rows (-1) 0

pEq, pLt, pLe, pGt, pGe :: FMat -> Int -> Col -> Int -> Bool
pEq m i c r = phaseCompare m i c r == 0
pLt m i c r = phaseCompare m i c r < 0
pLe m i c r = phaseCompare m i c r <= 0
pGt m i c r = phaseCompare m i c r > 0
pGe m i c r = phaseCompare m i c r >= 0

phaseColsLt :: FMat -> Int -> [Col] -> Int -> Bool
phaseColsLt m start cols rows = go 0
 where
  go i
    | i >= length cols = False
    | otherwise =
        let cmp = phaseCompare m (start + i) (cols !! i) rows
         in if cmp /= 0 then cmp < 0 else go (i + 1)

firstDiffColumn :: FMat -> FMat -> Int -> Int
firstDiffColumn x y rows =
  let len = max (length x) (length y)
      miss = constCol (-1) rows
      colAt xx i = if i < length xx then xx !! i else miss
      go i
        | i >= len = length x + 1
        | otherwise =
            let cmp = colCompareMiss (colAt x i) (colAt y i) rows (-1) (-1)
             in if cmp /= 0 then i + 1 else go (i + 1)
   in go 0

matricesEqual :: FMat -> FMat -> Int -> Bool
matricesEqual x y rows =
  firstDiffColumn x y rows == length x + 1 && length x == length y

columnCPrime :: FMat -> Int -> Int -> Int -> Col
columnCPrime m r n rows =
  [ if i < n then val (m !! r) i + 1 else 0 | i <- [0 .. rows - 1] ]

columnC :: FMat -> Int -> Int -> Int -> Col
columnC m t n rows =
  [ if i < n - 1
      then val (m !! t) i + 1
      else if i >= n then 0 else val (m !! t) i
  | i <- [0 .. rows - 1] ]

appendS :: FMat -> Prep -> Int -> [Int] -> FMat
appendS mp prep phase desc =
  let local = phase - pRoot prep
      rows = pRows prep
      b1 = bhs prep 1
   in if local < 0 || local >= length b1
        then mp
        else
          let col =
                [ if desc !! (phase * rows + r) == 1
                    then (b1 !! local) !! r - 1
                    else (b1 !! local) !! r
                | r <- [0 .. rows - 1]
                ]
           in mp ++ [col]

markSkipRange :: [Int] -> Int -> Int -> Int -> [Int]
markSkipRange skip from to alpha =
  [ if c >= from && c <= to && c < alpha && c >= 0 then 1 else skip !! c
  | c <- [0 .. length skip - 1] ]

findMinParentRoot :: FMat -> Int -> Int -> Int -> Int
findMinParentRoot m rows col root =
  headOr rows [ k | k <- [1 .. rows], bParent m rows (length m) col k == root ]

findAdjustedD :: FMat -> Int -> Int -> Int -> Int
findAdjustedD m rows t d =
  let cols = length m
      idx = t + d - 1
   in if idx < 0 || idx >= cols
        then d
        else
          let p1 = bParent m rows cols idx 1
           in if p1 <= t
                then d
                else
                  let (anc, _) = aAncestors m rows cols idx 1
                   in headOr d [ tp + 1 - t | tp <- anc, bParent m rows cols tp 1 == t ]

atOrLast :: FMat -> Int -> Col -> Col
atOrLast mp idx fallback
  | idx >= 0 && idx < length mp = mp !! idx
  | not (null mp) = last mp
  | otherwise = fallback

-- ---------------------------------------------------------------------
-- lpms_single（状态机）
-- ---------------------------------------------------------------------

data LpState = LpState
  { stMp :: FMat
  , stSkip :: [Int]
  , stDesc :: [Int]
  , stMSwitch :: !Bool
  , stBSwitch :: !Bool
  , stL :: !Int
  }

lpmsSingle :: FMat -> FMat
lpmsSingle matrix =
  let rows = maxRows matrix
      m = standardize matrix rows
   in if null m
        then []
        else
          if lastAllZero m
            then [ m !! i | i <- [0 .. length m - 2] ]
            else
              if length m == 2 && rows > 1
                then
                  let all0 = all (== 0) (m !! 0)
                      all1 = all (== 1) (m !! 1)
                   in if all0 && all1
                        then
                          let nr = rows - 1
                           in [ zeroCol nr, onesCol nr, replicate nr 2, constCol 3 nr ]
                        else runMain m rows
                else runMain m rows
 where
  runMain m rows =
    case makePrep m rows of
      Nothing -> []
      Just prep ->
        let r = pRoot prep
            n = pT prep
            alpha = pAlpha prep
            cols = pCols prep
            mp0 = pG prep ++ pB prep
            initState =
              LpState mp0 (replicate cols 0)
                (replicate (cols * max 1 rows) 0)
                True True 0
            cPrime = columnCPrime m r n rows
            last2Root = bParent m rows cols alpha 2 == r
            final =
              foldl (stepState prep cPrime last2Root n rows r alpha)
                    initState [r .. alpha - 1]
            lFinal = if stBSwitch final then cols - r - 1 else stL final
            out =
              if last2Root
                then
                  let b2 = bhs prep 2
                      takeN = min lFinal (length b2)
                      mp1 = stMp final ++ [ b2 !! i | i <- [0 .. takeN - 1] ]
                      a = if not (null b2)
                            then (b2 !! 0) !! 0
                            else (if null (stMp final) then 0 else (last (stMp final)) !! 0)
                   in mp1 ++ [constCol (a + 1) rows]
                else
                  let bLen = cols - r - 1
                   in if lFinal == bLen
                        then
                          let src = atOrLast (stMp final) (cols - 1) [0]
                           in stMp final ++ [constCol (src !! 0 + 1) rows]
                        else stMp final
         in standardize out rows

stepState :: Prep -> Col -> Bool -> Int -> Int -> Int -> Int -> LpState -> Int -> LpState
stepState prep cPrime last2Root n rows r alpha state t =
  let m = pM prep
      cols = pCols prep
      skip = stSkip state
      desc = stDesc state
   in if skip !! t == 1
        then state
        else
          if stBSwitch state
            then
              let mp1 = appendS (stMp state) prep t desc
               in if n >= rows || val (m !! r) n == 0 || pLe m (t + 1) cPrime rows
                    then state { stMp = mp1, stBSwitch = False, stL = t + 1 - r }
                    else state { stMp = mp1 }
            else
              let nMinusParent = if n > 1 then bParent m rows cols t (n - 1) else (-1)
                  nMinusVr = if n > 1 then getVr prep t (n - 2) else 0
                  c = columnC m t n rows
               in if nMinusParent /= r || nMinusVr == 0
                    then state { stMp = appendS (stMp state) prep t desc }
                    else if last2Root
                            then
                              if n < rows && val (m !! t) n > 0
                                then state { stMp = appendS (stMp state) prep t desc }
                                else
                                  if pEq m (t + 1) c rows && pLe m (t + 2) c rows
                                    then
                                      state { stMp = appendS (stMp state) prep t desc
                                            , stSkip = markSkipRange skip (t + 1) (t + 1) alpha }
                                    else
                                      let threshold = constCol ((c !! 0) + 1) rows
                                       in if pGt m (t + 1) c rows ||
                                             (pEq m (t + 1) c rows && pGe m (t + 2) threshold rows)
                                            then state { stMp = appendS (stMp state) prep t desc }
                                            else
                                              if pLt m (t + 1) c rows
                                                then
                                                  let a0 = val (m !! t) 0
                                                      mSwitch1 =
                                                        phaseColsLt m (t + 1)
                                                          [ constCol (a0 + 1) rows
                                                          , constCol (a0 + 2) rows ]
                                                          rows &&
                                                        bParent m rows cols t 2 == r
                                                      mpBase = appendS (stMp state) prep t desc
                                                      a1 = if null mpBase then 0 else (last mpBase) !! 0
                                                      mpUp = upmsSingle mpBase (stL state) rows True
                                                      mp1 = if mSwitch1 then mpUp ++ [constCol (a1 + 1) rows] else mpUp
                                                   in state { stMp = mp1, stMSwitch = False }
                                                else state { stMp = appendS (stMp state) prep t desc }
                            else
                              let k = findMinParentRoot m rows t r
                                  kp = if k == 1 then 2 else k
                                  (xx, yy, u, uMissing) = buildXy prep t kp
                                  eqXy = matricesEqual xx yy rows
                                  d0 = if eqXy then length xx + 1 else firstDiffColumn xx yy rows
                                  d1 =
                                    if not eqXy
                                      then
                                        if uMissing && t + d0 - 1 >= alpha
                                          then length xx + 1
                                          else findAdjustedD m rows t d0
                                      else d0
                               in if d1 == 1
                                    then state { stMp = appendS (stMp state) prep t desc }
                                    else
                                      if pLt m (t + d1 - 1) c rows
                                        then
                                          let skip1 = markSkipRange skip (t + 1) (t + d1 - 2) alpha
                                              a0 = val (m !! t) 0
                                              mSwitch1 =
                                                kp == 2 &&
                                                phaseColsLt m (t + d1 - 1)
                                                  [ constCol (a0 + 1) rows
                                                  , constCol (a0 + 2) rows ]
                                                  rows
                                              mpA =
                                                if t /= r + stL state
                                                  then
                                                    let app = appendS (stMp state) prep t desc
                                                        trimmed =
                                                          if null app
                                                            then app
                                                            else
                                                              let body = init app
                                                                  lastC = last app
                                                                  lastC' =
                                                                    [ if rr >= k && rr < rows
                                                                        then 0
                                                                        else lastC !! rr
                                                                    | rr <- [0 .. rows - 1] ]
                                                               in body ++ [lastC']
                                                   in upmsSingle trimmed (stL state) rows True
                                                  else stMp state
                                              a2 =
                                                if stL state > 0 && length mpA >= stL state
                                                  then (mpA !! (length mpA - stL state)) !! 0
                                                  else (if null mpA then 0 else (last mpA) !! 0)
                                              mp2 = if mSwitch1 then mpA ++ [constCol (a2 + 1) rows] else mpA
                                              matchSet rr =
                                                Set.fromList
                                                  [ c
                                                  | c <- [0 .. cols - 1]
                                                  , let (_, mask) = aAncestors m rows cols c (rr + 1)
                                                   in Set.member t mask ]
                                              desc1 =
                                                foldl
                                                  (\dsc rr ->
                                                     [ if Set.member c (matchSet rr)
                                                         then 1
                                                         else dsc !! (c * rows + r)
                                                     | c <- [0 .. cols - 1]
                                                     , r <- [0 .. rows - 1]
                                                     ])
                                                  desc
                                                  [k - 1 .. n - 2]
                                           in state { stMp = mp2, stSkip = skip1
                                                    , stMSwitch = False, stDesc = desc1 }
                                        else
                                          if n == 3 && pEq m (t + d1 - 1) c rows && pLt m (t + d1) c rows
                                            then
                                              let skip1 = markSkipRange skip (t + 1) (t + d1 - 1) alpha
                                                  a0 = val (m !! t) 0
                                                  mSwitch1 =
                                                    phaseColsLt m (t + d1)
                                                      [ constCol (a0 + 1) rows
                                                      , constCol (a0 + 2) rows ]
                                                      rows
                                                  mpBase = stMp state
                                                  a1 = if null mpBase then 0 else (last mpBase) !! 0
                                                  mpUp = upmsSingle mpBase (stL state) rows True
                                                  mp1 = if mSwitch1 then mpUp ++ [constCol (a1 + 1) rows] else mpUp
                                               in state { stMp = mp1, stSkip = skip1, stMSwitch = False }
                                            else
                                              let skip1 = markSkipRange skip (t + 1) (t + d1 - 2) alpha
                                                  skip2 =
                                                    if n == 3 && pEq m (t + d1 - 1) c rows && pEq m (t + d1) c rows
                                                      then
                                                        markSkipRange
                                                          (markSkipRange skip1 (t + d1 - 1) (t + d1 - 1) alpha)
                                                          (t + d1) (t + d1) alpha
                                                      else
                                                        if n > 3 && pEq m (t + d1 - 1) c rows &&
                                                             pLt m (t + d1) (constCol ((c !! 0) + 1) rows) rows
                                                          then markSkipRange skip1 (t + d1 - 1) (t + d1 - 1) alpha
                                                          else skip1
                                               in state { stMp = appendS (stMp state) prep t desc
                                                        , stSkip = skip2 }

-- ---------------------------------------------------------------------
-- lpms_fs / 入口
-- ---------------------------------------------------------------------

lpmsFs :: FMat -> Int -> FMat
lpmsFs expr n0 =
  let n = max 0 n0
   in if not (legal expr)
        then []
        else
          let rows = maxRows expr
              m = standardize expr rows
           in if null m
                then []
                else
                  if lastAllZero m
                    then [ m !! i | i <- [0 .. length m - 2] ]
                    else
                      if rows < 3 || val (last m) 2 == 0
                        then bmsFs m n rows
                        else lpmsFs (lpmsSingle m) n

runLpms :: Bool -> String -> Int -> String
runLpms isLptss text term =
  case parseBmExpr text of
    Nothing -> kParseError
    Just e ->
          let m = beMat e
           in if matCols m == 0
                then ""
                else
                  let inp = fromMatrix m
                      rows = maxRows inp
                      std = standardize inp rows
                   in if not (legal inp && not (lastAllZero std))
                        then
                          let dropped = [ inp !! i | i <- [0 .. length inp - 2] ]
                           in fmatToString (standardize dropped rows)
                        else fmatToString (lpmsFs inp term)
