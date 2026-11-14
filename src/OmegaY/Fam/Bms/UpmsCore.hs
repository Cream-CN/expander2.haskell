-- | UPMS 核心，逐行对齐 @Header/notation/upms.hpp@。
--
--   Cols = 列列表（每列 [Int]，长度 = 行数）。
--   AncestorCache / VrCache 用 Data.Map。
module OmegaY.Fam.Bms.UpmsCore
  ( upmsExpand
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import OmegaY.Matrix

-- ---------------------------------------------------------------------------
-- Cols 基础
-- ---------------------------------------------------------------------------

type Cols = [[Int]]

colCount :: Cols -> Int
colCount = length

rowCount :: Cols -> Int
rowCount c
  | null c = 0
  | otherwise = length (head c)

-- ---------------------------------------------------------------------------
-- Ancestor cache: (col, a) -> set of ancestor columns
-- ---------------------------------------------------------------------------

type AncestorCache = M.Map (Int, Int) (S.Set Int)

-- | get_b_parent(m, col_index, b, cache)
getBParent :: Cols -> Int -> Int -> AncestorCache -> (Int, AncestorCache)
getBParent m colIdx b cache0 =
  let nr = rowCount m
      rowIdx = b - 1
  in if rowIdx >= nr
       then (-1, cache0)
       else
         let cVal = (m !! colIdx) !! rowIdx
             (ancestors, cache1) = getAAncestors m colIdx (b - 1) cache0
             bestCol = S.foldl (\best anc ->
                       if anc >= colIdx
                          then best
                          else let v = (m !! anc) !! rowIdx
                               in if v < cVal && anc > best then anc else best)
                       (-1) ancestors
         in (bestCol, cache1)

-- | get_a_ancestors(m, col_index, a, cache)
getAAncestors :: Cols -> Int -> Int -> AncestorCache -> (S.Set Int, AncestorCache)
getAAncestors m colIdx a cache0 =
  let key = (colIdx, a)
  in case M.lookup key cache0 of
       Just anc -> (anc, cache0)
       Nothing ->
         let (ancestors, cache1) = worklist (S.singleton colIdx) cache0
         in (ancestors, M.insert key ancestors cache1)
  where
    worklist curSet cache1 =
      let current = S.toList curSet
          (finalSet, cache2) =
            foldl (\(s, c) col ->
                     let (parent, c') =
                           if a == 0
                             then (if col > 0 then col - 1 else (-1), c)
                             else getBParent m col a c
                     in if parent >= 0 && not (S.member parent s)
                          then (S.insert parent s, c')
                          else (s, c'))
                   (curSet, cache1) current
      in if finalSet == curSet
           then (finalSet, cache2)
           else worklist finalSet cache2

-- ---------------------------------------------------------------------------
-- find_bad_root
-- ---------------------------------------------------------------------------

data BadRoot = BadRoot
  { brRoot :: !Int
  , brT    :: !Int
  } deriving (Eq, Show)

findBadRoot :: Cols -> AncestorCache -> (Maybe BadRoot, AncestorCache)
findBadRoot m cache0 =
  let nc = colCount m
      nr = rowCount m
      last = m !! (nc - 1)
      t = findT (nr - 1)
      findT r
        | r < 0 = (-1)
        | last !! r /= 0 = r + 1
        | otherwise = findT (r - 1)
  in if t < 0
       then (Nothing, cache0)
       else
         let (root, cache1) = getBParent m (nc - 1) t cache0
         in if root < 0
              then (Nothing, cache1)
              else (Just (BadRoot root t), cache1)

-- ---------------------------------------------------------------------------
-- compute_delta
-- ---------------------------------------------------------------------------

computeDelta :: Cols -> Int -> Int -> [Int]
computeDelta m rootCol t =
  let nr = rowCount m
      last = m !! (length m - 1)
      root = m !! rootCol
  in [ if r + 1 < t then (last !! r) - (root !! r) else 0
     | r <- [0 .. nr - 1]
     ]

-- ---------------------------------------------------------------------------
-- VrCache: (col, row) -> 0 | 1
-- ---------------------------------------------------------------------------

type VrCache = M.Map (Int, Int) Int

vrGet :: VrCache -> Int -> Int -> Int
vrGet vr col row = M.findWithDefault (-1) (col, row) vr

vrSet :: VrCache -> Int -> Int -> Int -> VrCache
vrSet vr col row v = M.insert (col, row) v vr

-- ---------------------------------------------------------------------------
-- compute_upms_verification_roots
-- ---------------------------------------------------------------------------

computeVr :: Cols -> Int -> Int -> AncestorCache -> VrCache
computeVr m rootCol t cache0 =
  let nc = colCount m
      alpha = nc - 1
      y = rootCol
      nr = rowCount m
      isInBadPart col row = col >= y && col < alpha && row < t - 1
      getVr col row =
        if not (isInBadPart col row) then -1 else vrGet M.empty col row
  in fst $ foldl (\(vr, c) row ->
                      foldl (\(vr2, c2) col -> processCell vr2 c2 row col)
                             (vr, c) [y .. alpha - 1])
                  (M.empty, cache0) [0 .. t - 2]
  where
    processCell vr cache row col =
      let k = row + 1
      in if col == y
           then (vrSet vr col row 1, cache)
           else if row == 0
             then (vrSet vr col row 1, cache)
             else
               let (kAnc, cache1) = getAAncestors m col k cache
                   ayk = (m !! y) !! row
                   containsAyk = S.member y kAnc
                   ancestorHasVr0 =
                     any (\anc ->
                           let gv = if not (isInBadPart' anc row)
                                      then -1
                                      else vrGet vr anc row
                           in gv == 0)
                         (S.toList kAnc)
                   (bParent, cache2) = getBParent m col k cache1
                   isInBadPart' c r = c >= y && c < alpha && r < t - 1
                   getVr' c r =
                     if not (isInBadPart' c r) then -1 else vrGet vr c r
               in if not containsAyk || ancestorHasVr0 || bParent < 0
                    then (vrSet vr col row 0, cache2)
                    else if bParent /= y
                      then (vrSet vr col row 1, cache2)
                      else
                        let existsWVr0 =
                              any (\w -> getVr' col w == 0) [0 .. row - 1]
                        in if existsWVr0
                             then (vrSet vr col row 0, cache2)
                             else
                               let existsVCond =
                                     any (\vRow ->
                                           let (bp, _) = getBParent m col (vRow + 1) cache2
                                           in bp /= y)
                                         [row + 1 .. t - 2]
                               in if existsVCond
                                    then (vrSet vr col row 0, cache2)
                                    else
                                      let (vr3, cache3) =
                                            doComplex vr cache2 row col k ayk
                                      in (vr3, cache3)
      where
        alpha = colCount m - 1
        y = rootCol
        doComplex vr cache row col k ayk =
          let baseCol = [ if r < row then (m !! col !! r) + 1 else m !! col !! r
                        | r <- [0 .. row] ]
              (u, cache1) = findU row col baseCol cache
          in if u < 0
               then (vrSet vr col row 1, cache)
               else
                 let startX = col
                     endX = u - 2
                     xCols = [ m !! c | c <- [startX .. endX] ]
                     (kAncAlpha, cache2) = getAAncestors m alpha k cache1
                     j = findJ kAncAlpha ayk
                     j' = if j < 0 then alpha + 1 else j
                     startY = j' - 1
                     endY = alpha
                     yCols = [ m !! c | c <- [startY .. endY] ]
                     maxVal = maximum [ v | colV <- m, v <- colV ]
                     xVal = maxVal * 2
                     -- Xc
                     xc0 = map (take (rowCount m)) xCols
                     xc1 = transformX xc0 xVal row col startX vr [1 .. k - 1]
                     -- Yc
                     yc0 = map (take (rowCount m)) yCols
                     (yc1, cache3) = transformY yc0 xVal row startY startY [1 .. k - 1] cache2
                     cmp = compareCols xc1 yc1
                     newVal = if cmp < 0 then 0 else 1
                 in (vrSet vr col row newVal, cache3)
          where
            alpha = colCount m - 1
            findU r col bc cacheU = go (col + 1) cacheU
              where
                go c cacheU2
                  | c > alpha = (-1, cacheU2)
                  | otherwise =
                      let (less, cacheU3) = checkRow c bc cacheU2
                      in if less then (c + 1, cacheU3) else go (c + 1) cacheU3
                checkRow c bc cacheU2 = goR 0 cacheU2
                  where
                    goR rIdx cacheU3
                      | rIdx > r = (False, cacheU3)
                      | otherwise =
                          let v = m !! c !! rIdx
                          in if v < bc !! rIdx then (True, cacheU3)
                             else if v > bc !! rIdx then (False, cacheU3)
                             else goR (rIdx + 1) cacheU3
            findJ kAnc ayk = goJ (S.toList kAnc)
              where
                goJ [] = (-1)
                goJ (anc : rest) =
                  if m !! anc !! row == ayk + 1 then anc + 1 else goJ rest
            transformX xc xVal row col startX vr sList = foldl applyS xc sList
              where
                applyS xcAcc s =
                  let sRow = s - 1
                      dx = xVal - (m !! col !! sRow)
                  in [ if vrGet vr (startX + xi) sRow == 1
                         then updateAt sRow (colV !! sRow + dx) colV
                         else colV
                     | (xi, colV) <- zip [0 ..] xcAcc ]
            transformY yc xVal row startY origStartY sList cacheTY =
              foldl (\(ycAcc, cAcc) s ->
                       let sRow = s - 1
                           dy = xVal - (m !! startY !! sRow)
                           (ycNew, cNew) =
                             foldl (\(yAcc, cAcc2) yi ->
                                      let origCol = startY + yi
                                          (sAnc, cAcc3) = getAAncestors m origCol s cAcc2
                                      in if origCol == startY || S.member startY sAnc
                                           then (updateAt sRow ((yAcc !! yi) !! sRow + dy) (yAcc !! yi) : take yi yAcc ++ drop (yi+1) yAcc, cAcc3)
                                           else (yAcc, cAcc3))
                                   (ycAcc, cAcc) [0 .. length ycAcc - 1]
                       in (reverse ycNew, cNew))
                     (reverse yc, cacheTY) sList
            compareCols xc yc =
              let mc = min (length xc) (length yc)
                  goCI ci
                    | ci >= mc =
                        if length xc < length yc then (-1)
                        else if length xc > length yc then 1
                        else 0
                    | otherwise =
                        let mr = min (length (xc !! ci)) (length (yc !! ci))
                            goRI ri
                              | ri >= mr =
                                  if length (xc !! ci) < length (yc !! ci) then (-1)
                                  else if length (xc !! ci) > length (yc !! ci) then 1
                                  else goCI (ci + 1)
                              | otherwise =
                                  let xv = xc !! ci !! ri
                                      yv = yc !! ci !! ri
                                  in if xv < yv then (-1)
                                     else if xv > yv then 1
                                     else goRI (ri + 1)
                        in goRI 0
              in goCI 0
            updateAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- generate_bh_upms
-- ---------------------------------------------------------------------------

generateBh :: Cols -> [Int] -> Int -> Int -> Int -> VrCache -> Cols
generateBh bCols delta t h rootCol vr =
  [ [ let k = if r + 1 < t && vrGet vr origCol r == 1 then 1 else 0
      in colV !! r + h * (delta !! r) * k
    | r <- [0 .. nr - 1]
    ]
  | (ci, colV) <- zip [0 ..] bCols
  , let origCol = rootCol + ci
  ]
  where
    nr = rowCount bCols

-- ---------------------------------------------------------------------------
-- expand
-- ---------------------------------------------------------------------------

upmsExpand :: Matrix -> Int -> Matrix
upmsExpand m term
  | matRows m == 0 || matCols m == 0 = emptyMatrix
  | term < 1 = emptyMatrix
  | otherwise =
      let nr = matRows m
          nc = matCols m
          cols0 = [ [ matAt m r c | r <- [0 .. nr - 1] ] | c <- [0 .. nc - 1] ]
          -- 后继型：末列全零
          lastAllZero = all (== 0) (last cols0)
      in if lastAllZero
           then
             let cols1 = init cols0
             in colsToMatrix cols1
           else
             let (bad, cache1) = findBadRoot cols0 M.empty
             in case bad of
                  Nothing -> emptyMatrix
                  Just (BadRoot root t) ->
                    let gCols = take root cols0
                        bCols = take (nc - root - 1) (drop root cols0)
                        delta = computeDelta cols0 root t
                        vr = computeVr cols0 root t cache1
                        resultCols =
                          gCols
                          ++ bCols
                          ++ concat [ generateBh bCols delta t h root vr
                                   | h <- [1 .. term]
                                   ]
                    in colsToMatrix resultCols

-- | Cols → Matrix
colsToMatrix :: Cols -> Matrix
colsToMatrix cols
  | null cols = emptyMatrix
  | otherwise =
      let nr = rowCount cols
      in Matrix nr (length cols) [ take nr (c ++ repeat 0) | c <- cols ]
