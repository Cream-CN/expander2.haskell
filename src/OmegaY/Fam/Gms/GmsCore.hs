-- | GMS —— General Matrix System 族核心。
module OmegaY.Fam.Gms.GmsCore
  ( System (..)
  , Proj (..)
  , runGms
  ) where

import OmegaY.Fam.Gms.Common
import OmegaY.Matrix (matCols)

data System = SysGBMS | SysUPMS | SysLPMS2
  deriving (Eq, Show)

data Proj = ProjOmegaP | ProjPQSS | ProjQSS | ProjFull | ProjWeirdlyFull
  deriving (Eq, Show)

-- 父项链

type Parents = [Col]  -- parents!!(level-1)!!col

computeParents :: FMat -> Int -> Parents
computeParents a rows = parents
 where
  n = length a
  parents = p1 : higher
  p1 = [ findP c | c <- [0 .. n - 1] ]
   where
    findP c = go (c - 1)
     where
      go q
        | q < 0 = (-1)
        | val (a !! q) 0 < val (a !! c) 0 = q
        | otherwise = go (q - 1)
  higher = [ buildLevel level | level <- [2 .. rows] ]
  buildLevel level =
    let prev = parents !! (level - 2)
        suffixStart = level - 1
        walk p c
          | p == (-1) = (-1)
          | suffixCompare (a !! p) (a !! c) suffixStart rows < 0 = p
          | otherwise = walk (prev !! p) c
     in [ walk (prev !! c) c | c <- [0 .. n - 1] ]

parentAt :: Parents -> Int -> Int -> Int
parentAt parents level col
  | level < 1 || level > length parents || col < 0 = (-1)
  | otherwise = (parents !! (level - 1)) !! col

isAncestor :: Parents -> Int -> Int -> Int -> Bool
isAncestor parents level ancestor = go
 where
  go p
    | p == (-1) = False
    | p == ancestor = True
    | otherwise = go (parentAt parents level p)

childAboveAncestor :: Parents -> Int -> Int -> Int -> Int
childAboveAncestor parents level descendant ancestor = go descendant
 where
  go current
    | current == (-1) = (-1)
    | otherwise =
        let p = parentAt parents level current
         in if p == ancestor
              then current
              else if p == (-1)
                then (-1)
                else go p

-- LimitCtx

data LimitCtx = LimitCtx
  { lcA :: FMat
  , lcRows :: !Int
  , lcLast :: !Int
  , lcParents :: Parents
  , lcYIndex :: !Int
  , lcY :: Col
  , lcD :: Col
  , lcK :: !Int
  , lcYPlusC :: FMat
  , lcAPrime :: FMat
  , lcFs1 :: FMat
  , lcC :: FMat
  }

prepareLimitContext :: FMat -> GmsM LimitCtx
prepareLimitContext a
  | null a = throwGms
  | otherwise = do
      let rows = rowCount a
          last = length a - 1
          x = a !! last
          m = highestNonzeroRow x rows
      if m == 0
        then throwGms
        else do
          let parents = computeParents a rows
              yIndex = parentAt parents m last
          if yIndex == (-1)
            then throwGms
            else do
              let y = a !! yIndex
                  d = [ if r < m - 1 then val x r - val y r else 0
                      | r <- [0 .. rows - 1]
                      ]
                  k = highestNonzeroRow d rows
                  slice lo hi = [ cloneCol (a !! i) rows | i <- [lo .. hi - 1] ]
                  cMat = slice (yIndex + 1) last
                  aPrime = slice 0 last
                  yPlusC = slice yIndex last
                  fs1 = clone aPrime rows ++ [ addCols y d rows ]
              Just LimitCtx
                { lcA = clone a rows
                , lcRows = rows
                , lcLast = last
                , lcParents = parents
                , lcYIndex = yIndex
                , lcY = y
                , lcD = d
                , lcK = k
                , lcYPlusC = yPlusC
                , lcAPrime = aPrime
                , lcFs1 = fs1
                , lcC = cMat
                }

buildGbmsD :: LimitCtx -> FMat
buildGbmsD ctx =
  [ [ cell local row | row <- [0 .. rows - 1] ]
  | local <- [0 .. length yPlusC - 1]
  ]
 where
  rows = lcRows ctx
  parents = lcParents ctx
  yIndex = lcYIndex ctx
  d = lcD ctx
  k = lcK ctx
  yPlusC = lcYPlusC ctx
  -- C++：for row=1..k: d[local][row-1] = d[row-1]；row 为 0 基位置。
  cell local row =
    let global = yIndex + local
        level = row + 1
     in if level >= 1 && level <= k &&
           (global == yIndex || isAncestor parents level yIndex global)
          then d !! row
          else 0

projectionRecurrence :: LimitCtx -> FMat -> Int -> FMat
projectionRecurrence ctx d index
  | index == 0 = clone (lcAPrime ctx) rows
  | index == 1 = clone (lcFs1 ctx) rows
  | otherwise =
      let base = clone (lcAPrime ctx) rows
          appended =
            concat
              [ case addMat (lcYPlusC ctx) (scaleMat d factor rows) rows of
                  Just shifted -> shifted
                  Nothing -> []
              | factor <- [1 .. index - 1]
              ]
       in base ++ appended
 where
  rows = lcRows ctx

-- UPMS：Y / v / D，以及带记忆化的 build_adjusted_suffix

type AdjCache = [((Int, Int), FMat)]

adjSuffix :: Int -> FMat -> Parents -> Int -> Int -> Int -> [Col]
          -> AdjCache -> Int -> Int -> (FMat, AdjCache)
adjSuffix rows a parents yIndex last h vArr cache zIndex level =
  case lookup (zIndex, level) cache of
    Just cached -> (clone cached rows, cache)
    Nothing ->
      let suffix = [ cloneCol (a !! i) rows | i <- [zIndex .. last] ]
          adjustment =
            [ [ cell local p | p <- [0 .. rows - 1] ]
            | local <- [0 .. length suffix - 1]
            ]
          cell local p =
            let row = p + 1
                spanPos = zIndex + local - yIndex
             in if row >= 1 && row < level &&
                   row < length vArr &&
                   spanPos >= 0 && spanPos < length (vArr !! row) &&
                   (vArr !! row) !! spanPos == 1
                  then h - val (a !! zIndex) p
                  else 0
       in case addMat suffix adjustment rows of
            Just result -> (result, ((zIndex, level), clone result rows) : cache)
            Nothing -> (suffix, cache)

buildUpms :: LimitCtx -> GmsM (UpmsResult, AdjCache)
buildUpms ctx = do
  let rows = lcRows ctx
      a = lcA ctx
      parents = lcParents ctx
      yIndex = lcYIndex ctx
      last = lcLast ctx
      k = lcK ctx
      yPlusC = lcYPlusC ctx
      h = 2 * maxEntry a rows
      spanLength = last - yIndex + 1
  -- Y[2..k]
  yLevels <- mapM (buildYLevel rows a parents yIndex last h) [2 .. k]
  let yArr = fillArr (replicate (k + 1) []) [2 .. k] yLevels
  -- v[1]
  let v1 = replicate spanLength 1
      vAcc0 = fillArr (replicate (k + 1) []) [1] [v1]
      step (vAcc, cache) level =
        let prevV = vAcc !! (level - 1)
            (cur, cache1) =
              buildVLevel rows a parents yIndex last h yArr spanLength vAcc cache level prevV
            vAcc' = fillArr vAcc [level] [cur]
         in (vAcc', cache1)
      (vArr, cache) = foldl step (vAcc0, []) [2 .. k]
  -- D
  let d = lcD ctx
      dMat =
        [ [ if p >= 0 && p < k
              then (d !! p) * ((vArr !! (p + 1)) !! pos)
              else 0
          | p <- [0 .. rows - 1] ]
        | pos <- [0 .. length yPlusC - 1]
        ]
  Just (UpmsResult h yArr vArr dMat, cache)
 where
  fillArr xs [] _ = xs
  fillArr xs (i : is) (v : vs) = fillArr (take i xs ++ [v] ++ drop (i + 1) xs) is vs
  fillArr xs _ [] = xs

data UpmsResult = UpmsResult
  { urH :: !Int
  , urY :: [FMat]
  , urV :: [Col]
  , urD :: FMat
  }

buildYLevel :: Int -> FMat -> Parents -> Int -> Int -> Int -> Int -> GmsM FMat
buildYLevel rows a parents yIndex last h level = do
  let zIndex = childAboveAncestor parents level last yIndex
  if zIndex == (-1)
    then throwGms
    else do
      let suffix = [ cloneCol (a !! i) rows | i <- [zIndex .. last] ]
          adjustment =
            [ [ cell local p | p <- [0 .. rows - 1] ]
            | local <- [0 .. length suffix - 1]
            ]
          cell local p =
            let global = zIndex + local
                row = p + 1
             in if row >= 1 && row < level &&
                   (global == zIndex || isAncestor parents row zIndex global)
                  then h - val (a !! zIndex) p
                  else 0
      case addMat suffix adjustment rows of
        Just r -> Just r
        Nothing -> throwGms

buildVLevel :: Int -> FMat -> Parents -> Int -> Int -> Int -> [FMat] -> Int
           -> [Col] -> AdjCache -> Int -> Col -> (Col, AdjCache)
buildVLevel rows a parents yIndex last h yArr spanLength vArr cache level prevV =
  go base 1 cache
 where
  base =
    let z = replicate spanLength 0
        z0 = take 0 z ++ [1] ++ drop 1 z
     in take (spanLength - 1) z0 ++ [1]
  go cur pos cache
    | pos >= spanLength - 1 = (cur, cache)
    | otherwise =
        let zIndex = yIndex + pos
         in if not (isAncestor parents level yIndex zIndex) ||
               (prevV !! pos) == 0
              then go cur (pos + 1) cache
              else
                let zPrime = childAboveAncestor parents level zIndex yIndex
                 in if zPrime == (-1)
                      then go cur (pos + 1) cache
                      else
                        let (xMat, cache1) =
                              adjSuffix rows a parents yIndex last h vArr cache zPrime level
                            target = yArr !! level
                            bit = if matCompare xMat target < 0 then 0 else 1
                            cur' = take pos cur ++ [bit] ++ drop (pos + 1) cur
                         in go cur' (pos + 1) cache1

-- LPMS2 的 X

firstDifferentColumn :: FMat -> FMat -> Int
firstDifferentColumn a b = go 0
 where
  common = min (length a) (length b)
  rows = max (rowCount a) (rowCount b)
  go i
    | i < common && colCompare (a !! i) (b !! i) rows /= 0 = i
    | i < common = go (i + 1)
    | otherwise = common

buildLpms2X :: LimitCtx -> UpmsResult -> AdjCache -> GmsM (FMat, AdjCache)
buildLpms2X ctx upms cache0 =
  let rows = lcRows ctx
      a = lcA ctx
      parents = lcParents ctx
      yIndex = lcYIndex ctx
      last = lcLast ctx
      k = lcK ctx
      cLength = length (lcC ctx)
      dMat = urD upms
      vArr = urV upms
      h = urH upms
      yArr = urY upms
      step (xs, cache, sVec) round =
        if sVec !! round == 1
          then (xs, cache, sVec)
          else
            let zIndex = yIndex + 1 + round
                z = a !! zIndex
                spanPos = round + 1
                passesK =
                  parentAt parents k zIndex == yIndex &&
                  spanPos < length (vArr !! k) &&
                  (vArr !! k) !! spanPos == 1
                (xs1, cache1, sVec1) =
                  if not passesK
                    then (xs, cache, sVec)
                    else
                      let level =
                            headOr (-1)
                              [ cand
                              | cand <- [2 .. k]
                              , parentAt parents cand zIndex == yIndex
                              ]
                       in if level == (-1)
                            then (xs, cache, sVec)
                            else
                              let (adjusted, cacheA) =
                                    adjSuffix rows a parents yIndex last h vArr cache zIndex level
                                  diff = firstDifferentColumn adjusted (yArr !! level)
                                  wIndex =
                                    if diff < length adjusted
                                      then zIndex + diff
                                      else last + 1
                               in if wIndex == zIndex
                                    then (xs, cacheA, sVec)
                                    else
                                      let jPrime =
                                            if wIndex == last + 1
                                              then cLength + 1
                                              else headOr (-1)
                                                     [ search - yIndex - 1
                                                     | search <- [wIndex, wIndex - 1 .. yIndex + 2]
                                                     , val (a !! search) 0 <= val z 0 + 1
                                                     ]
                                          sVec2 =
                                            if jPrime == (-1)
                                              then sVec
                                              else
                                                [ if q >= round + 1 && q <= jPrime - 1 && q < length sVec
                                                    then 1
                                                    else sVec !! q
                                                | q <- [0 .. length sVec - 1]
                                                ]
                                       in (xs, cacheA, sVec2)
                appended =
                  if round + 1 < length dMat
                    then [ addCols z (dMat !! (round + 1)) rows ]
                    else []
             in (xs1 ++ appended, cache1, sVec1)
      (xCols, cache1, _) =
        foldl step ([], cache0, replicate (cLength + 1) 0) [0 .. cLength - 1]
  in Just (xCols, cache1)
 where
  headOr d xs = case xs of
    (x : _) -> x
    [] -> d

-- old_finite_fs

oldFiniteFs :: FMat -> Int -> System -> GmsM FMat
oldFiniteFs a index system
  | index < 0 = throwGms
  | null a = Just []
  | otherwise = do
      let rows = rowCount a
          last = length a - 1
      if isZeroCol (a !! last) rows
        then Just (clone (take last a) rows)
        else do
          ctx <- prepareLimitContext a
          if index == 0
            then Just (clone (lcAPrime ctx) rows)
            else
              if index == 1
                then Just (clone (lcFs1 ctx) rows)
                else
                  let gbmsD = buildGbmsD ctx
                   in if system == SysGBMS || lcK ctx <= 1
                        then Just (projectionRecurrence ctx gbmsD index)
                        else do
                          (upms, _cache) <- buildUpms ctx
                          if system == SysUPMS
                            then Just (projectionRecurrence ctx (urD upms) index)
                            else do
                              (xMat, _) <- buildLpms2X ctx upms []
                              let next = clone (lcFs1 ctx) rows ++ xMat
                              oldFiniteFs next (index - 1) system

-- 五个极限构造器

limitBuilder :: Proj -> Int -> FMat
limitBuilder proj index = case proj of
  ProjOmegaP ->
    [ [0, 0, 0] ] ++ [ [ n, n `div` 2, (n + 1) `div` 2 ] | n <- [1 .. index] ]
  ProjPQSS ->
    [ [0, 0, 0, 0] ] ++ [ [ n, n `div` 2, (n - 1) `div` 2, 1 ] | n <- [1 .. index] ]
  ProjQSS ->
    [ [0, 0, 0, 0] ] ++
      [ [ n, (n + 1) `div` 3, n `div` 3, (n + 2) `div` 3 ] | n <- [1 .. index] ]
  ProjFull -> limitFull index
  ProjWeirdlyFull -> limitWeirdlyFull index

limitFull :: Int -> FMat
limitFull index
  | index == 0 = []
  | otherwise =
      let first = [ replicate index 0 ]
          rest =
            concat
              [ [ let triangular = ((stage - 2) * (stage - 1)) `div` 2
                      col0 = triangular + j
                      body =
                        [ max 1 (stage - r - (if r >= j then 1 else 0))
                        | r <- [1 .. stage - 1]
                        ]
                   in take index (col0 : body ++ repeat 0)
                | j <- [1 .. stage - 1]
                ]
              | stage <- [2 .. index]
              ]
       in first ++ rest

limitWeirdlyFull :: Int -> FMat
limitWeirdlyFull index =
  let rows = index + 2
      first = [ replicate rows 0 ]
      rest =
        [ let body = [ if r < j then 1 else 0 | r <- [1 .. rows - 2] ]
           in j : body ++ [1]
        | j <- [1 .. rows - 1]
        ]
   in first ++ rest

trimEnabled :: Proj -> Bool
trimEnabled proj = proj == ProjFull || proj == ProjWeirdlyFull

compareFor :: Proj -> FMat -> FMat -> Int
compareFor proj a b
  | proj /= ProjWeirdlyFull = matCompare a b
  | null a || null b = matCompare a b
  | otherwise =
      let ra = rowCount a
          rb = rowCount b
       in if ra /= rb then compareI ra rb else matCompare a b

rawAdjustedFs :: FMat -> Int -> System -> Proj -> GmsM FMat
rawAdjustedFs a index system proj = do
  old1 <- if index >= 2 then oldFiniteFs a 1 system else Just []
  old2 <- if index >= 2 then oldFiniteFs a 2 system else Just []
  let oldIndex = if index >= 2 && length old2 <= length old1 then index + 1 else index
  result <- oldFiniteFs a oldIndex system
  Just (if trimEnabled proj then trimZeroRows result else result)

fs :: FMat -> Int -> System -> Proj -> GmsM FMat
fs a index system proj
  | index < 0 = throwGms
  | otherwise = do
      result0 <- rawAdjustedFs a 0 system proj
      go result0 0 1
 where
  go result logical rawIndex
    | logical >= index = Just result
    | otherwise =
        if rawIndex > index + 10000
          then throwGms
          else do
            candidate <- rawAdjustedFs a rawIndex system proj
            if compareFor proj candidate result == 0
              then go result logical (rawIndex + 1)
              else go candidate (logical + 1) (rawIndex + 1)

-- 对外入口

runGms :: System -> Proj -> String -> Int -> String
runGms system proj text term =
  case parseBmExpr text of
    Nothing -> kParseError
    Just e ->
          let m = beMat e
           in if matCols m == 0
                then ""
                else
                  let a = fromMatrix m
                   in case fs a term system proj of
                        Just result -> fmatToString result
                        Nothing -> kParseError
