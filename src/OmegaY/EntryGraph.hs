-- | C++ core/entry.hpp 的可变节点图：用 IntMap 中的整数 id 代替裸指针。
module OmegaY.EntryGraph
  ( EntryG (..)
  , Graph
  , newGraph
  , newNode
  , gGet
  , gGetDef
  , gPut
  , gMod
  , pushUpL
  , gSize
  , verticalCompare
  , verticalIncrease
  , dimensionDifference
  ) where

import Control.Monad.ST (ST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- | 与 C++ core::Entry 同构；leg 字段存节点 id，缺省为 Nothing。
data EntryG = EntryG
  { egValue :: Int
  , egX :: Int
  , egY :: [Int]
  , egUpL :: [Int]      -- ^ leftleg_up
  , egUpR :: Maybe Int  -- ^ rightleg_up
  , egDownR :: Maybe Int -- ^ rightleg_down
  , egDownL :: Maybe Int -- ^ leftleg_down
  } deriving (Eq, Show)

newtype Graph s = Graph (STRef s (IntMap EntryG))

newGraph :: ST s (Graph s)
newGraph = Graph <$> newSTRef IM.empty

-- 节点只增不删，因此用当前最大 id + 1 分配；IntMap 的 findMax 是 O(log n)。
newNode :: Graph s -> Int -> Int -> [Int] -> ST s Int
newNode g v x y = do
  Graph ref <- pure g
  m <- readSTRef ref
  let nid = if IM.null m then 0 else fst (IM.findMax m) + 1
  writeSTRef ref (IM.insert nid (EntryG v x y [] Nothing Nothing Nothing) m)
  pure nid

gGet :: Graph s -> Int -> ST s (Maybe EntryG)
gGet (Graph ref) i = IM.lookup i <$> readSTRef ref

-- 调用点保证 id 有效；缺失时返回零值节点，避免 Maybe 扩散到所有表达式。
gGetDef :: Graph s -> Int -> ST s EntryG
gGetDef g i = do
  m <- gGet g i
  pure (maybe (EntryG 0 0 [] [] Nothing Nothing Nothing) id m)

gPut :: Graph s -> Int -> EntryG -> ST s ()
gPut (Graph ref) i e = writeSTRef ref . IM.insert i e =<< readSTRef ref

gMod :: Graph s -> Int -> (EntryG -> EntryG) -> ST s ()
gMod g i f = do
  m <- gGet g i
  case m of
    Nothing -> pure ()
    Just e -> gPut g i (f e)

-- | 追加 @leftleg_up@。
pushUpL :: Graph s -> Int -> Int -> ST s ()
pushUpL g i child = gMod g i (\e -> e { egUpL = egUpL e ++ [child] })

gSize :: Graph s -> ST s Int
gSize (Graph ref) = IM.size <$> readSTRef ref

-- y 向量的序

-- 序按 (长度, 从高维到低维的字典序) 定义，与 C++ 的 vertical 比较一致。
verticalCompare :: [Int] -> [Int] -> Int
verticalCompare a b
  | length a > length b = 1
  | length a < length b = -1
  | otherwise = cmpRev (reverse a) (reverse b)
 where
  cmpRev [] [] = 0
  cmpRev (x : xs) (y : ys)
    | x > y = 1
    | x < y = -1
    | otherwise = cmpRev xs ys

-- 第 d 维自增，低于 d 的维清零，长度不足 d+1 时先补零。
verticalIncrease :: [Int] -> Int -> [Int]
verticalIncrease y d0
  | d0 < 0 = y
  | otherwise =
      let d = d0
          -- 对应 C++: c = y; if (idx >= c.size()) c.resize(idx+1, 0);
          c0 = if d < length y then y else y ++ replicate (d + 1 - length y) 0
          cur = c0 !! d
          newV = if cur == 0 then 1 else cur + 1
          c1 = replaceAt d newV c0
       in -- 对应 C++: for (i < d) c[i] = 0；d 之后的元素原样保留
          [ if i < d then 0 else c1 !! i | i <- [0 .. length c1 - 1] ]
  where
    replaceAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- 返回最高的不同维号；长度不同视为在较短一侧已出现差异。完全相同返回 -1。
dimensionDifference :: [Int] -> [Int] -> Int
dimensionDifference c1 c2 = go (max (length c1) (length c2) - 1)
 where
  v c d = if d < length c then c !! d else 0
  has c d = d < length c
  go d
    | d < 0 = -1
    | has c1 d /= has c2 d = d
    | has c1 d && has c2 d && v c1 d /= v c2 d = d
    | otherwise = go (d - 1)
