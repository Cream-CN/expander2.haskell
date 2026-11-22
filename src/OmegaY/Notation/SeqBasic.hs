-- | 基础数列记号：空记号 / PrSS / PPS / PPS4 家族 / Second PPS4 / 2-pps4。
--   对齐 C++ 侧 @empty.hpp@、@prss.hpp@、@PPS-family.hpp@。
module OmegaY.Notation.SeqBasic
  ( emptyNotation
  , prssNotation
  , ppsNotation
  , pps4Notation
  , wpps4Notation
  , tpps4Notation
  , ewpps4Notation
  , secondPps4Notation
  , pps2Notation
  , expandPrss
  , expandPps1
  ) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import qualified Data.IntMap.Strict as IM
import OmegaY.MVec
import OmegaY.Notation.Types

-- | 按写入序列构造长度为 @total@ 的向量；同一位置多次写入时后者生效
--   （对齐 C++ 里 @result[position] = ...@ 的覆盖语义）。
applyWrites :: Int -> [(Int, Int)] -> [Int]
applyWrites total writes =
  let m = foldl (\acc (i, v) -> IM.insert i v acc) IM.empty writes
   in [IM.findWithDefault 0 i m | i <- [0 .. total - 1]]

-- 空记号（passthrough）

emptyNotation :: Notation
emptyNotation =
  seqNotation "empty" "空记号" ["测试", "基础"] (\seq _ -> seq) "  (尚在重构中)"

-- PrSS

isSuccessor :: [Int] -> Bool
isSuccessor seq = not (null seq) && last seq == 0

prssBadRoot :: [Int] -> Int
prssBadRoot seq
  | null seq = -1
  | otherwise = go (length seq - 2)
 where
  ak = last seq
  go i
    | i < 0 = -1
    | seq !! i < ak = i
    | otherwise = go (i - 1)

expandPrss :: [Int] -> Int -> [Int]
expandPrss seq n0
  | null seq = seq
  | isSuccessor seq = init seq
  | r < 0 = init seq
  | otherwise =
      let n = max 1 n0
          badLen = length seq - 1 - r
          prefix = take r seq
          badPart = take badLen (drop r seq)
       in prefix ++ concat (replicate n badPart)
 where
  r = prssBadRoot seq

prssNotation :: Notation
prssNotation = seqNotation "prss" "PrSS" ["PrSS", "原始数列", "数列", "入门", "基础"] expandPrss ""

-- PPS（原始数列系统的第二版）

-- | @expand_pps1@：PPS 的单步展开。
expandPps1 :: [Int] -> Int -> [Int]
expandPps1 seq n0
  | null seq = seq
  | isSuccessor seq = init seq
  | x <= 0 || x > y = seq
  | ll <= 0 = seq
  | otherwise = runST $ do
      res <- mvFromList (init seq)
      mvPush res newLast
      let targetLen = max y (y + n * ll - 1)
      go res targetLen (x + 1)
      mvToList res
 where
  y = length seq
  x = last seq
  n = max 1 n0
  ll = y - x
  b = seq !! (x - 1)
  weak = any (\i -> seq !! i == b) [x .. y - 2]
  newLast = if weak then b else last seq - 1

  go res targetLen i
    | i > targetLen - ll = pure ()
    | otherwise = do
        valM <-
          if i < y
            then pure (Just (seq !! (i - 1)))
            else
              if i == y
                then Just <$> mvRead res (y - 1)
                else do
                  nres <- mvLen res
                  if i - 1 < nres then Just <$> mvRead res (i - 1) else pure Nothing
        case valM of
          Nothing -> pure ()
          Just v -> do
            let newVal = if v >= x then v + ll else v
                targetIndex = i + ll
            when (targetIndex <= targetLen) $ do
              nres <- mvLen res
              when (targetIndex - 1 >= nres) (mvResize res targetIndex)
              mvWrite res (targetIndex - 1) newVal
        go res targetLen (i + 1)

ppsNotation :: Notation
ppsNotation = seqNotation "pps" "PPS" ["PPS", "原始数列", "数列", "入门"] expandPps1 ""

-- PPS4 家族

data PPS4Variant = PPS4 | WPPS4 | TPPS4 | EWPPS4
  deriving (Eq)

-- | 对应 @expand_pps4_variant@。
expandPps4Variant :: PPS4Variant -> [Int] -> Int -> [Int]
expandPps4Variant variant seq n0
  | null seq = seq
  | isSuccessor seq = init seq
  | x <= 0 || x > y = seq
  | ll <= 0 = seq
  | strongExpand = strongResult
  | otherwise = normalResult
 where
  y = length seq
  x = last seq
  n = max 1 n0
  ll = y - x
  b = seq !! (x - 1)
  weak = any (\i -> seq !! i == b) [x .. y - 2]

  findCol
    | weak = Nothing
    | otherwise =
        case variant of
          EWPPS4 -> loopEw [x - 2, x - 3 .. b]
          WPPS4 -> loopW [x - 2, x - 3 .. b]
          _ -> loopN [x - 2, x - 3 .. b]
  inRange c = c >= 0 && c < y
  loopEw [] = Nothing
  loopEw (c : cs)
    | not (inRange c) = loopEw cs
    | seq !! c == b = Just (c + 1)
    | seq !! c < b = Nothing
    | otherwise = loopEw cs
  loopW [] = Nothing
  loopW (c : cs)
    | not (inRange c) = loopW cs
    | seq !! c == b = Just (c + 1)
    | otherwise = loopW cs
  loopN [] = Nothing
  loopN (c : cs)
    | not (inRange c) = loopN cs
    | seq !! c <= b = Just (c + 1)
    | otherwise = loopN cs

  (newLast, strongExpand) =
    case findCol of
      Just fc -> (fc, variant == TPPS4)
      Nothing -> (b, False)

  -- 强展开（TPPS4）：未走补列分支，而是按位置逐个外推。
  strongResult
    | totalLen <= y = out0
    | otherwise = take totalLen outFull
   where
    totalLen = y + n * ll
    out0 = take (y - 1) seq ++ [newLast]
    outFull = go out0 (y + 1)
    go out position
      | position > totalLen = out
      | otherwise =
          let isLastCopy = position > y && (position - y) `mod` ll == 0
           in if isLastCopy
                then
                  let copyNumber = (position - y) `div` ll
                   in go (out ++ [newLast + copyNumber * ll]) (position + 1)
                else
                  let sourcePos = position - ll
                      v = out !! (sourcePos - 1)
                   in go (out ++ [if v >= x then v + ll else v]) (position + 1)

  -- 普通分支：从 i = x+1 到 targetLen - ll 逐个补列（读回已写入的值）。
  normalResult = runST $ do
    res <- mvFromList (init seq)
    mvPush res newLast
    let targetLen = max y (y + n * ll - 1)
    goN res targetLen (x + 1)
    mvToList res
   where
    goN res targetLen i
      | i > targetLen - ll = pure ()
      | otherwise = do
          valM <-
            if i < y
              then pure (Just (seq !! (i - 1)))
              else
                if i == y
                  then Just <$> mvRead res (y - 1)
                  else do
                    nres <- mvLen res
                    if i - 1 < nres then Just <$> mvRead res (i - 1) else pure Nothing
          case valM of
            Nothing -> pure ()
            Just v -> do
              let newVal = if v >= x then v + ll else v
                  targetIndex = i + ll
              when (targetIndex <= targetLen) $ do
                nres <- mvLen res
                when (targetIndex - 1 >= nres) (mvResize res targetIndex)
                mvWrite res (targetIndex - 1) newVal
          goN res targetLen (i + 1)

pps4Notation, wpps4Notation, tpps4Notation, ewpps4Notation :: Notation
pps4Notation = seqNotation "pps4" "PPS4" ["PPS", "原始数列", "4行", "数列"] (expandPps4Variant PPS4) ""
wpps4Notation = seqNotation "wpps4" "Weak PPS4" ["PPS", "弱", "4行", "数列"] (expandPps4Variant WPPS4) ""
tpps4Notation = seqNotation "tpps4" "Third PPS4" ["PPS", "第三", "4行", "数列"] (expandPps4Variant TPPS4) ""
ewpps4Notation = seqNotation "ewpps4" "Ex. Weak PPS4" ["PPS", "扩展弱", "4行", "数列"] (expandPps4Variant EWPPS4) ""

-- Second PPS4

expandSecondPps4 :: [Int] -> Int -> [Int]
expandSecondPps4 seq count
  | null seq = seq
  | x == 0 = init seq
  | x > y = seq
  | ll <= 0 = seq
  | totalLength <= 0 = []
  | otherwise = applyWrites totalLength writes
 where
  y = length seq
  x = last seq
  b = seq !! (x - 1)
  ll = y - x
  totalLength = y + count * ll - 1

  foundLessOrEqual = any (\c -> seq !! (c - 1) <= b) [y - 1, y - 2 .. x + 1]

  (value, strongExpand)
    | foundLessOrEqual = (b, False)
    | otherwise =
        let strongStart = b + 1
            strongEnd = x - 1
            found =
              if strongStart <= strongEnd
                then [c | c <- [strongEnd, strongEnd - 1 .. strongStart], seq !! (c - 1) == b]
                else []
         in case found of
              (c : _) -> (c, True)
              [] -> (b, False)

  writes =
    [ (i, seq !! i) | i <- [0 .. x - 1] ]
      ++ [ (i, seq !! i) | i <- [x .. y - 2] ]
      ++ [(y - 1, value)]
      ++ concat
        [ [ (position, if (i == y - 1 && strongExpand) || baseValue >= x
                        then baseValue + copy * ll
                        else baseValue)
          | copy <- [1 .. shifts]
          , let position = i + copy * ll
          , position < totalLength
          ]
        | i <- [x .. y - 1]
        , let baseValue = if i == y - 1 then value else seq !! i
        , let shifts = if i == y - 1 then count - 1 else count
        ]

secondPps4Notation :: Notation
secondPps4Notation =
  seqNotation
    "spps4"
    "Second PPS4"
    ["PPS", "第二", "4行", "数列"]
    expandSecondPps4
    ""

-- 2-pps4

expand2Pps4 :: [Int] -> Int -> [Int]
expand2Pps4 seq n
  | null seq = seq
  | n == 0 = init seq
  | x == 0 = init seq
  | x > y = seq
  | y == 2 && seq !! 0 == 0 && seq !! 1 == 2 = [0 .. n]
  | ll <= 0 = seq
  | totalLen <= 0 = []
  | otherwise = applyWrites totalLen writes
 where
  y = length seq
  x = last seq
  b = seq !! (x - 1)
  ll = y - x
  totalLen = y + n * ll - 1

  equalCount = length [() | col <- [x + 1 .. y - 1], seq !! (col - 1) == b]

  v
    | equalCount >= 2 = b
    | otherwise =
        let strongStart = b + 1
            strongEnd = x - 1
            found =
              if strongStart <= strongEnd
                then [c | c <- [strongEnd, strongEnd - 1 .. strongStart], seq !! (c - 1) <= b]
                else []
         in case found of
              (c : _) -> c
              [] -> b

  writes =
    [ (i, seq !! i) | i <- [0 .. x - 1] ]
      ++ [ (i, seq !! i) | i <- [x .. y - 2] ]
      ++ [(y - 1, v)]
      ++ concat
        [ [ (position, if baseVal >= x then baseVal + k * ll else baseVal)
          | k <- [1 .. maxK]
          , let position = i + k * ll
          , position < totalLen
          ]
        | i <- [x .. y - 1]
        , let baseVal = if i == y - 1 then v else seq !! i
        , let maxK = if i == y - 1 then n - 1 else n
        ]

pps2Notation :: Notation
pps2Notation =
  seqNotation "2-pps4" "2-pps4" ["PPS", "2行", "4行", "数列"] expand2Pps4 ""
