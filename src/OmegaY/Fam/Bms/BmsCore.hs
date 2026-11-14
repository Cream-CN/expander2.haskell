-- | BMS 族核心（10 个版本共享骨架，按 ver code 分支）。
--
--   逐行对齐 @Header/notation/BMSFamily.hpp@：
--   getParent / getParentIB / getConcestor / getBadSequence / copyBadSequence
--   / chkStd / expand。文本路径只用 chkStd + expand。
module OmegaY.Fam.Bms.BmsCore
  ( Version (..)
  , verCode
  , bmsExpand
  , bmsIsStandard
  ) where

import Data.Array (Array, listArray, (!))
import qualified Data.Array as A
import OmegaY.Matrix

-- | 10 个 BMS 版本。
data Version
  = V1 | V2 | V21 | V22 | V23 | V3 | V31 | V32 | V33 | V4
  deriving (Eq, Show)

verCode :: Version -> Int
verCode V1  = 100
verCode V2  = 200
verCode V21 = 210
verCode V22 = 220
verCode V23 = 230
verCode V3  = 300
verCode V31 = 310
verCode V32 = 320
verCode V33 = 330
verCode V4  = 400

-- ---------------------------------------------------------------------------
-- 工具
-- ---------------------------------------------------------------------------

updateAt :: Int -> a -> [a] -> [a]
updateAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- | C 数组下标：row + col * nr
cGet :: Array Int Int -> Int -> Int -> Int -> Int
cGet c nr r col = c A.! (r + col * nr)

cSet :: Array Int Int -> Int -> Int -> Int -> Int -> Array Int Int
cSet c nr r col v = c A.// [(r + col * nr, v)]

-- ---------------------------------------------------------------------------
-- getParent：同行向左找第一个严格小于 (r,c) 的列
-- ---------------------------------------------------------------------------

getParent :: Matrix -> Int -> Int -> Int
getParent s r c = go c
  where
    go i
      | i < 0 = -1
      | matAt s r i < matAt s r c = i
      | otherwise = go (i - 1)

-- ---------------------------------------------------------------------------
-- getParentIB：IB 祖先
-- ---------------------------------------------------------------------------

getParentIB :: Matrix -> Int -> Int -> Int
getParentIB s r c = go c r
  where
    go x y
      | x <= 0 = -1
      | otherwise =
          let x' = if y == 0 then x - 1 else getParentIB s (y - 1) x
          in if x' < 0
               then -1
               else if matAt s y x' < matAt s y c
                      then x'
                      else go x' y

-- ---------------------------------------------------------------------------
-- getConcestor
-- ---------------------------------------------------------------------------

getConcestor :: Matrix -> Int -> Int -> Int
getConcestor s m c = go p0
  where
    p0 = [ getParent s r c | r <- [0 .. m - 1] ]
    go p
      | any (== (-1)) p = -1
      | all (== head p) (tail p) = head p
      | otherwise =
          let maxr = foldl (\mr i -> if p !! i > p !! mr then i else mr) (m - 1) [0 .. m - 1]
              p' = updateAt maxr (getParent s maxr (p !! maxr)) p
          in go p'

-- ---------------------------------------------------------------------------
-- getBadSequence
-- ---------------------------------------------------------------------------

getBadSequence :: Int -> Matrix -> [Int] -> Array Int Int -> Int -> Int
               -> (Int, [Int], Array Int Int)
getBadSequence ver s delta0 c0 n nr
  | matAt s 0 n == 0 = (0, delta0, c0)
  | otherwise = case ver of
      100 -> bs100 s delta0 c0 n nr
      210 -> bs210 s delta0 c0 n nr
      220 -> bs220 s delta0 c0 n nr
      _   -> bsGeneric ver s delta0 c0 n nr

-- | 找第一个 j ∈ [0,row] 使 S(j,n)==0；没有则返回 row+1。
findFirstZero :: Matrix -> Int -> Int -> Int
findFirstZero s n row = go 0
  where
    go j
      | j > row = row + 1
      | matAt s j n == 0 = j
      | otherwise = go (j + 1)

-- | ver=100
bs100 :: Matrix -> [Int] -> Array Int Int -> Int -> Int
      -> (Int, [Int], Array Int Int)
bs100 s delta0 c0 n nr = (bad, delta', c0)
  where
    row = nr - 1
    mVal = findFirstZero s n row
    (bad, delta') = goK 0
    goK k
      | k > n = (0, delta0)
      | otherwise =
          let found = all (\l -> matAt s l (n - k) < matAt s l n) [0 .. mVal - 1]
          in if found
               then ( k
                    , [ if l < mVal - 1 then matAt s l n - matAt s l (n - k) else 0
                      | l <- [0 .. row]
                      ]
                    )
               else goK (k + 1)

-- | ver=210
bs210 :: Matrix -> [Int] -> Array Int Int -> Int -> Int
      -> (Int, [Int], Array Int Int)
bs210 s delta0 c0 n nr = outer 0 0 delta0
  where
    row = nr - 1
    mVal = findFirstZero s n row
    outer k curBad curDelta
      | k > n = (curBad, curDelta, c0)
      | otherwise =
          let k1 = innerL k 1
              found = all (\l -> matAt s l (n - k1) < matAt s l n) [0 .. mVal - 1]
          in if found
               then let d = [ if l < mVal - 1
                                 then matAt s l n - matAt s l (n - k1)
                                 else 0
                            | l <- [0 .. row]
                            ]
                    in (k1, d, c0)
               else outer k1 curBad curDelta
    innerL k l
      | n - (k + l) < 0 = k
      | matAt s 0 (n - (k + l)) < matAt s 0 (n - k) = k + l
      | otherwise = innerL k (l + 1)

-- | ver=220
bs220 :: Matrix -> [Int] -> Array Int Int -> Int -> Int
      -> (Int, [Int], Array Int Int)
bs220 s delta0 c0 n nr = (bad, delta', c0)
  where
    row = nr - 1
    mVal = findFirstZero s n row
    l = getConcestor s mVal n
    bad = if l >= 0 then n - l else 0
    delta' = [ if j < mVal - 1 then matAt s j n - matAt s j (n - bad) else 0
             | j <- [0 .. row]
             ]

-- | ver ∈ {200,230,300,310,320,330,400}：先求 bad/Delta，再按 ver 算 C。
bsGeneric :: Int -> Matrix -> [Int] -> Array Int Int -> Int -> Int
          -> (Int, [Int], Array Int Int)
bsGeneric ver s delta0 c0 n nr = (bad, delta', c')
  where
    row = nr - 1
    -- Step 1: Delta 清零，找 bad
    (bad, delta1) = findBad 0 (replicate nr 0)
    findBad k d
      | k > n = (0, d)
      | otherwise =
          case scanL 0 d of
            Just d' -> (k, d')
            Nothing -> findBad (k + 1) d
      where
        scanL l d
          | l > row = Nothing
          | matAt s l (n - k) < (matAt s l n - (d !! l)) =
              if l == row || matAt s (l + 1) n == 0
                then Just d
                else let d' = updateAt l (matAt s l n - matAt s l (n - k)) d
                     in scanL (l + 1) d'
          | otherwise = Nothing
    -- Step 2: 按 ver 算 C
    c1 = postProcess ver s c0 n nr row bad
    delta' = delta1
    c' = c1

-- | C 后处理（按 ver）。
postProcess :: Int -> Matrix -> Array Int Int -> Int -> Int -> Int -> Int
            -> Array Int Int
postProcess ver s c0 n nr row bad = case ver of
  200 -> p200or310 c0
  310 -> p310
  230 -> p230or320 c0
  320 -> p320
  330 -> p330 c0
  400 -> p400 c0
  _   -> c0  -- ver=300: C 不被使用，但仍按 200 路径算过（C 保持初值）
  where
    p200or310 cIn = foldl stepK cIn [1 .. bad]
      where
        stepK c k =
          let l = findL k
              setR r cAcc = cSet cAcc nr r (k + 1)
                              (if matAt s r (n - bad) < matAt s r (n - bad + k)
                                  && cGet cAcc nr r (l + 1) == 1
                                 then 1
                                 else 0)
          in foldl (flip setR) c [0 .. row]
        findL k = goL k
          where
            goL l
              | l < 0 = 0
              | matAt s 0 (n - bad + l) < matAt s 0 (n - bad + k) = l
              | otherwise = goL (l - 1)

    p230or320 cIn =
      foldl (\cAcc j -> foldl (\cAcc2 k -> setJK j k cAcc2) cAcc [0 .. bad - 1])
            cIn [0 .. nr - 1]
      where
        setJK j k cAcc = cSet cAcc nr j (k + 1) (goP k)
          where
            goP p
              | p == 0 = 1
              | p < 0 = 0
              | otherwise = goP (getParentIB s j (p + n - bad) - (n - bad))

    p310 =
      let cAfter200 = p200or310 c0
          mTop = findMTop (nr - 1)
          findMTop r
            | r < 0 = (-1)
            | matAt s r n /= 0 = r
            | otherwise = findMTop (r - 1)
          checkK k cAcc =
            let anyZero = any (\l -> cGet cAcc nr l (k + 1) == 0) [0 .. mTop]
            in if anyZero
                 then foldl (\c' l -> cSet c' nr l (k + 1) 0) cAcc [1 .. nr - 1]
                 else cAcc
      in foldl (\cAcc k -> checkK k cAcc) cAfter200 [0 .. bad - 1]

    p320 =
      let cAfter230 = p230or320 c0
          mTop = findMTop (nr - 1)
          findMTop r
            | r < 0 = (-1)
            | matAt s r n /= 0 = r
            | otherwise = findMTop (r - 1)
          checkK k cAcc =
            let anyZero = any (\l -> cGet cAcc nr l (k + 1) == 0) [0 .. mTop]
            in if anyZero
                 then foldl (\c' l -> cSet c' nr l (k + 1) 0) cAcc [1 .. nr - 1]
                 else cAcc
      in foldl (\cAcc k -> checkK k cAcc) cAfter230 [0 .. bad - 1]

    p330 cIn =
      let c1 = foldl (\cAcc m -> foldl (\cAcc2 k -> cSet cAcc2 nr m (k + 1) 0)
                                        cAcc [0 .. bad - 1])
                      cIn [0 .. row]
          lmost = foldl (\lm m -> if matAt s m n /= 0 then m else lm) 0 [0 .. row]
          c2 = cSet c1 nr lmost 1 1
          c3 = foldl (\cAcc k ->
                        let p = getParentIB s lmost (n - k)
                        in if p >= 0
                              && cGet cAcc nr lmost (p - (n - bad) + 1) == 1
                             then cSet cAcc nr lmost (bad - k + 1) 1
                             else cAcc)
                     c2 [bad - 1, bad - 2 .. 0]
          c4 = foldl (\cAcc k ->
                        if cGet cAcc nr lmost (bad - k + 1) == 1
                          then foldl (\c' m -> cSet c' nr m (bad - k + 1) 1)
                                      cAcc [0 .. lmost]
                          else cAcc)
                     c3 [0 .. bad]
          c5 = foldl (\cAcc m ->
                        foldl (\cAcc2 k ->
                                 let p = getParentIB s m (n - k)
                                 in if p > n - bad
                                       && cGet cAcc2 nr m (p - (n - bad) + 1) == 1
                                      then cSet cAcc2 nr m (bad - k + 1) 1
                                      else cAcc2)
                               cAcc [bad, bad - 1 .. 1])
                     c4 [0 .. lmost - 1]
      in c5

    p400 cIn =
      foldl (\cAcc l -> foldl (\cAcc2 m -> processM l m cAcc2) cAcc [0 .. row])
            cIn [bad, bad - 1 .. 2]
      where
        processM l m cAcc =
          let e0 = replicate nr 0
              (pp, q) = findPPQ m l e0
              colIdx = bad - pp + 1
              newVal = if cGet cAcc nr m colIdx == 1 && q == m then 1 else 0
          in cSet cAcc nr m (bad - l + 2) newVal
        findPPQ m l e = goN2 l e 0 0
          where
            goN2 n2 eLoc ppLoc qLoc
              | n2 > bad = (ppLoc, qLoc)
              | otherwise =
                  let (eLoc', hit, pp', q') = goO 0 n2 eLoc
                  in if hit
                       then (pp', q')
                       else goN2 (n2 + 1) eLoc' ppLoc qLoc
            goO o n2 eLoc
              | o > m = (eLoc, False, 0, 0)
              | matAt s o (n - n2) < (matAt s o (n - l + 1) - (eLoc !! o)) =
                  if o == m || (o + 1 < nr && matAt s (o + 1) (n - l + 1) == 0)
                    then (eLoc, True, n2, o)
                    else let e' = updateAt o
                                   (matAt s o (n - l + 1) - matAt s o (n - n2))
                                   eLoc
                         in goO (o + 1) n2 e'
              | otherwise = (eLoc, False, 0, 0)

-- ---------------------------------------------------------------------------
-- copyBadSequence
-- ---------------------------------------------------------------------------

copyBadSequence :: Int -> Matrix -> [Int] -> Array Int Int -> Int -> Int -> Int
                -> Int -> Matrix
copyBadSequence ver s delta c nStart nn nr bad
  | bad <= 0 = s
  | otherwise = case ver of
      100 -> loopS nStart s
      210 -> loopS nStart s
      220 -> loopS nStart s
      200 -> loopM nStart 1 s
      230 -> loopM nStart 1 s
      310 -> loopM nStart 1 s
      320 -> loopM nStart 1 s
      330 -> loopM nStart 1 s
      400 -> loopM nStart 1 s
      300 -> loopK nStart 1 s
  where
    loopS n s'
      | n >= nn = s'
      | otherwise =
          let s'' = foldl (\acc l -> matSet acc l n
                                    (matAt acc l (n - bad) + (delta !! l)))
                          s' [0 .. nr - 1]
          in loopS (n + 1) s''
    loopM n m s'
      | n >= nn = s'
      | otherwise =
          let s'' = foldl (\acc l -> matSet acc l n
                                    (matAt acc l (n - bad)
                                       + (delta !! l) * cGet c nr l m))
                          s' [0 .. nr - 1]
              m' = if m + 1 > bad then 1 else m + 1
          in loopM (n + 1) m' s''
    loopK n k s'
      | n >= nn = s'
      | otherwise =
          let s'' = foldl (\acc l ->
                              let v = matAt acc l (n - bad)
                                  mM = findM acc n
                                  cond = l == 0 || k == 1
                                       || (matAt acc l (n - bad - mM) < matAt acc l (n - mM)
                                            && matAt acc l (n - mM) < matAt acc l n + (delta !! l))
                              in matSet acc l n (if cond then v + (delta !! l) else v))
                          s' [0 .. nr - 1]
              k' = if k + 1 > bad then 1 else k + 1
          in loopK (n + 1) k' s''
    findM s' n = goM 0
      where
        goM m
          | m > n = 0
          | matAt s' 0 (n - m) < matAt s' 0 n = m
          | otherwise = goM (m + 1)

-- ---------------------------------------------------------------------------
-- expand
-- ---------------------------------------------------------------------------

bmsExpand :: Version -> Matrix -> Int -> Matrix
bmsExpand ver0 m term
  | matRows m == 0 || matCols m == 0 = m
  | otherwise =
      let ver = verCode ver0
          nr = matRows m
          n = matCols m - 1
          num = if term < 1 then 1 else term
          delta0 = replicate nr 0
          capCols = num * (n + 1) + 2
          cSize = nr * capCols
          c0 = listArray (0, cSize - 1) (replicate cSize 0)
          c1 = foldl (\c r -> cSet c nr r 1 1) c0 [0 .. nr - 1]
          (bad, delta1, c2) = getBadSequence ver m delta0 c1 n nr
      in if bad <= 0
           then if matCols m > 1
                  then matResizeCols (matCols m - 1) m
                  else matResizeCols 0 m
           else let nn = n + bad * num
                    s1 = matResizeCols nn m
                    s2 = copyBadSequence ver s1 delta1 c2 n nn nr bad
                in matResizeCols nn s2

-- ---------------------------------------------------------------------------
-- chkStd：判断矩阵是否标准式。返回 0=标准，1=非标准。
-- ---------------------------------------------------------------------------

bmsIsStandard :: Version -> Matrix -> Bool
bmsIsStandard ver0 s = chkStd s == 0
  where
    ver = verCode ver0
    nc = matCols s
    nr = matRows s

    -- 第一步：求 row（最高非零行）
    row0 = foldl stepRow 0 [0 .. nc - 1]
      where
        stepRow r i
          | r + 1 == nr = r
          | otherwise = foldl (\r2 j -> if matAt s j i > 0 then j else r2)
                              r [r + 1 .. nr - 1]

    -- 第二步：找 p
    pVal = findP 0
      where
        findP i
          | i >= nc = -1
          | otherwise =
              case scanJ 0 of
                1 -> 1
                0 -> i
                _ -> findP (i + 1)
          where
            scanJ j
              | j > row0 = (-1)
              | matAt s j i > i = 1
              | matAt s j i < i = 0
              | otherwise = scanJ (j + 1)

    rRows = row0 + 1

    chkStd mat
      | pVal < 0 = 0
      | pVal == 1 = 1
      | otherwise =
          let p = pVal
              -- SA = 前 p+1 列，截断到 rRows 行
              saCols = [ [ matAt s r c | r <- [0 .. rRows - 1] ]
                       | c <- [0 .. p]
                       ]
              sa0 = Matrix rRows (p + 1) saCols
              cSize = rRows * (nc * 2 + 4)
              c0 = listArray (0, cSize - 1) (replicate cSize 0)
              c1 = foldl (\c i -> cSet c rRows i 1 1) c0 [0 .. rRows - 1]
              delta0 = replicate rRows 0
          in chkLoop sa0 delta0 c1 p

    chkLoop sa delta c p =
      let (bad, delta', c') = getBadSequence ver sa delta c p rRows
      in if bad == 0
           then 1
           else let num = (nc + 1 - p) `div` bad + 1
                    nn = p + bad * num
                    sa1 = matResizeCols (nn + 1) sa
                    sa2 = copyBadSequence ver sa1 delta' c' p nn rRows bad
                    p3 = findP3 p sa2
                in if p3 == 1
                     then 1
                     else if p3 == (-1)
                            then 0
                            else chkLoop sa2 delta' c' p3

    findP3 i sa'
      | i >= nc = (-1)
      | otherwise =
          let (smaller, larger) =
                foldl (\(sm, lg) j ->
                        let sj = matAt s j i
                            saj = matAt sa' j i
                        in (sm || sj < saj, lg || sj > saj))
                      (False, False) [0 .. rRows - 1]
          in if larger && not smaller
               then 1
               else if larger || smaller
                      then i
                      else findP3 (i + 1) sa'
