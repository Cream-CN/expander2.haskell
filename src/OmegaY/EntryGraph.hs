-- | 「山脉」图：可变 Entry 节点图，对齐 C++ 侧 @core/entry.hpp@ 与
--   @Omega-YMagma.hpp@ / @omega_y.hpp@ 等使用指针链接的实现。
--
--   C++ 侧用 @Entry*@ 互相引用（leftleg_up / rightleg_up / rightleg_down /
--   leftleg_down）。这里用整数 id + IntMap 的 ST 图表示：读、写、改都在
--   ST 中进行，使移植后的算法与原实现逐句对应。
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

-- | 与 C++ @core::Entry@ 同构的节点。
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

-- | 新建节点，返回其 id。
newNode :: Graph s -> Int -> Int -> [Int] -> ST s Int
newNode g v x y = do
  Graph ref <- pure g
  m <- readSTRef ref
  let nid = if IM.null m then 0 else fst (IM.findMax m) + 1
  writeSTRef ref (IM.insert nid (EntryG v x y [] Nothing Nothing Nothing) m)
  pure nid

gGet :: Graph s -> Int -> ST s (Maybe EntryG)
gGet (Graph ref) i = IM.lookup i <$> readSTRef ref

-- | 读节点；缺失时返回一个「空节点」（值为 0、y 为空）。
--   用于数组中不可能为空的场合，避免把 Maybe 渗透进每个表达式。
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

-- ----------------------------------------------------------------------
-- y 向量的序
-- ----------------------------------------------------------------------

-- | 先比长度（更长 = 更大），再从高位到低位逐位比较。
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

-- | 第 d 维加一，低于 d 的维清零；不足补 0 到 d+1（d < 0 视为 0）。
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

-- | 最高一处不同的维号；完全相同返回 -1。
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
