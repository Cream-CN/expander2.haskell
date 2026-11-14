-- | Xy 族 v1/v2 共享底层：Entry arena（对应 C++ deque Arena）、行运算
--   （compareRow 长度优先 / rowAddition / rowDifference / getFootRow）、
--   toSequence。v3 行运算不同，不复用本模块。
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

import Control.Monad.ST (ST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.STRef
  ( STRef
  , modifySTRef'
  , newSTRef
  , readSTRef
  , writeSTRef
  )

-- | Entry 节点。chain/dimseq 仅 v2 使用；v1 置空。
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

-- | 合成根：value=0, row=[1], cloumn=-1。
makeRootV12 :: E
makeRootV12 = E 0 [1] (-1) 0 Nothing Nothing Nothing [] []

-- | Arena 世界：条目表 + 下一分配 id。对应 C++ deque Arena。
data World s = World
  { wAr :: STRef s (IntMap E)
  , wNext :: STRef s Int
  }

newWorld :: ST s (World s)
newWorld = do
  ar <- newSTRef IM.empty
  nx <- newSTRef 0
  pure (World ar nx)

allocE :: World s -> E -> ST s Int
allocE w e = do
  i <- readSTRef (wNext w)
  writeSTRef (wNext w) (i + 1)
  modifySTRef' (wAr w) (IM.insert i e)
  pure i

getE :: World s -> Int -> ST s E
getE w i = do
  m <- readSTRef (wAr w)
  pure (IM.findWithDefault makeRootV12 i m)

-- | 局部修改某条目字段。
modE :: World s -> Int -> (E -> E) -> ST s ()
modE w i f = modifySTRef' (wAr w) (IM.adjust f i)

setE :: World s -> Int -> E -> ST s ()
setE w i e = modifySTRef' (wAr w) (IM.insert i e)

-- | 字典序：先比长度（短 < 长），再逐元素。对应 xy-v1/v2 compareRow。
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

-- | 行加法。对应 xy-v1/v2 rowAddition。
rowAddition :: [Int] -> [Int] -> [Int]
rowAddition r1 r2
  | length r1 < length r2 = r2
  | null r2 = r1
  | otherwise =
      let k = length r1 - length r2
          b0 = head r2 + atM r1 k
          b = b0 : drop 1 r2
       in take k r1 ++ b

-- | 行减法。对应 xy-v1/v2 rowDifference。
rowDifference :: [Int] -> [Int] -> [Int]
rowDifference r1 r2
  | length r1 > length r2 = r1
  | compareRow r1 r2 <= 0 = []
  | otherwise =
      let i = length (takeWhile (uncurry (==)) (zip r1 r2))
          d = atM r1 i - atM r2 i
       in d : drop (i + 1) r1

-- | getFootRow（v1/v2 版）。传入 parent 的 row（无父则 Nothing）。
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

-- | isDimensionLimited。d=[1,0,1] 时恒 false，保留以忠实移植。
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
