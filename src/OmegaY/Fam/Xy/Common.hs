-- | Xy 族 v1/v2 共享底层：Entry arena（对应 C++ deque Arena）、行运算
module OmegaY.Fam.Xy.Common
  ( E (..)
  , World
  , newWorld
  , allocE
  , getE
  , modE
  , setE
  , compareRow
  , rowAddition
  , rowDifference
  , getFootRow
  , isDimensionLimitedV12
  , toSequenceV12
  , makeRootV12
  , atM
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

data E = E
  { e_value :: Int
  , e_row :: [Int]
  , e_cloumn :: Int
  , e_no :: Int
  , e_parent :: Maybe Int
  , e_head :: Maybe Int
  , e_foot :: Maybe Int
  , e_chain :: [Int]
  , e_dimseq :: [Int]
  }

-- | 安全下标（越界返回 0）。
atM :: [Int] -> Int -> Int
atM xs i = if i >= 0 && i < length xs then xs !! i else 0

makeRootV12 :: E
makeRootV12 = E 0 [1] (-1) 0 Nothing Nothing Nothing [] []

-- | Arena 世界：条目槽位数组 + 分配水位。对应 C++ deque Arena。
--   原来是 @STRef s (IntMap E)@：每分配/修改一次都要整表读取 + O(log n) 路径复制，
--   旧表版本随即变垃圾；连续 id 用槽位数组后读写都是 O(1)。
data World s = World
  { wAr :: STRef s (STArray s Int E)
  , wCap :: STRef s Int
  , wNext :: STRef s Int
  }

newWorld :: ST s (World s)
newWorld = do
  arr0 <- newArray (0, 63) makeRootV12
  arr <- newSTRef arr0
  cap <- newSTRef 64
  nx <- newSTRef 0
  pure (World arr cap nx)

ensureCapE :: World s -> Int -> ST s ()
ensureCapE w need = do
  cap <- readSTRef (wCap w)
  when (need >= cap) $ do
    let cap' = max (need + 1) (cap * 2)
    old <- readSTRef (wAr w)
    arr' <- newArray (0, cap' - 1) makeRootV12
    let copy i
          | i >= cap = pure ()
          | otherwise = readArray old i >>= writeArray arr' i >> copy (i + 1)
    copy (0 :: Int)
    writeSTRef (wAr w) arr'
    writeSTRef (wCap w) cap'

allocE :: World s -> E -> ST s Int
allocE w e = do
  i <- readSTRef (wNext w)
  ensureCapE w i
  arr <- readSTRef (wAr w)
  writeArray arr i e
  writeSTRef (wNext w) (i + 1)
  pure i

getE :: World s -> Int -> ST s E
getE w i = do
  nx <- readSTRef (wNext w)
  if i < 0 || i >= nx
    then pure makeRootV12
    else do
      arr <- readSTRef (wAr w)
      readArray arr i

modE :: World s -> Int -> (E -> E) -> ST s ()
modE w i f = do
  nx <- readSTRef (wNext w)
  when (i >= 0 && i < nx) $ do
    arr <- readSTRef (wAr w)
    e <- readArray arr i
    writeArray arr i (f e)

setE :: World s -> Int -> E -> ST s ()
setE w i e = do
  ensureCapE w i
  arr <- readSTRef (wAr w)
  writeArray arr i e
  nx <- readSTRef (wNext w)
  when (i >= nx) (writeSTRef (wNext w) (i + 1))

compareRow :: [Int] -> [Int] -> Int
compareRow r1 r2
  | length r1 < length r2 = -1
  | length r1 > length r2 = 1
  | otherwise = cmp r1 r2
 where
  cmp [] [] = 0
  cmp (a : as) (b : bs) =
    case compare a b of
      LT -> -1
      GT -> 1
      EQ -> cmp as bs
  cmp _ _ = 0

rowAddition :: [Int] -> [Int] -> [Int]
rowAddition r1 r2
  | length r1 < length r2 = r2
  | null r2 = r1
  | otherwise =
      let k = length r1 - length r2
          b0 = head r2 + atM r1 k
          b = b0 : drop 1 r2
       in take k r1 ++ b

rowDifference :: [Int] -> [Int] -> [Int]
rowDifference r1 r2
  | length r1 > length r2 = r1
  | compareRow r1 r2 <= 0 = []
  | otherwise =
      let i = length (takeWhile (uncurry (==)) (zip r1 r2))
          d = atM r1 i - atM r2 i
       in d : drop (i + 1) r1

getFootRow :: E -> Maybe [Int] -> [Int] -> [Int]
getFootRow it mparentRow d =
  let row0 = [1]
      diff = case mparentRow of
        Nothing -> 0
        Just prow -> length (rowDifference (e_row it) prow)
      zeros =
        if compareRow d [0] > 0
          then replicate diff 0
          else []
      row1 = row0 ++ zeros
   in rowAddition (e_row it) row1

isDimensionLimitedV12 :: E -> Maybe [Int] -> [Int] -> Bool
isDimensionLimitedV12 it mparentRow d =
  length d == 1
    && length (e_row it) == atM d 0
    && case mparentRow of
      Nothing -> True
      Just prow -> length prow < atM d 0 || head (e_row it) > head prow

-- | 整数序列 → Entry 序列。parent 指向左侧最近较小元素；无则指向新合成根。
--   返回 [entry id]（按原始顺序）。对应 xy-v1/v2 toSequence。
toSequenceV12 :: World s -> [Int] -> ST s [Int]
toSequenceV12 w s = go 0 []
 where
  n = length s
  go i acc
    | i >= n = pure (reverse acc)
    | otherwise = do
        let vi = s !! i
        if vi <= 1
          then do
            root <- allocE w makeRootV12
            e <- allocE w (E vi [0] i 0 (Just root) Nothing Nothing [] [])
            go (i + 1) (e : acc)
          else do
            -- 找左侧最近较小元素（最大 j < i 使 s[j] < s[i]）
            let smaller = [j | j <- [i - 1, i - 2 .. 0], atM s j < vi]
            case smaller of
              (j : _) -> do
                -- acc 是逆序：索引 i-1 在头，索引 j 在 acc !! (i-1-j)
                let parentId = acc !! (i - 1 - j)
                e <- allocE w (E vi [0] i 0 (Just parentId) Nothing Nothing [] [])
                go (i + 1) (e : acc)
              [] -> do
                root <- allocE w makeRootV12
                e <- allocE w (E vi [0] i 0 (Just root) Nothing Nothing [] [])
                go (i + 1) (e : acc)
