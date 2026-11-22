-- | 可增长的整数向量（ST 中的可变数组），语义对齐 C++ @std::vector<int>@。
module OmegaY.MVec
  ( MVec
  , newMVec
  , mvFromList
  , mvLen
  , mvRead
  , mvReadDef
  , mvWrite
  , mvPush
  , mvResize
  , mvToList
  , mvLast
  , mvSetLen
  ) where

import Control.Monad.ST (ST)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- | 数组引用本身放在 STRef 中，容量不足时整体替换（对齐 vector 的扩容）。
data MVec s = MVec
  { mvArr :: STRef s (STArray s Int Int)
  , mvCap :: STRef s Int
  , mvSize :: STRef s Int
  }

newMVec :: Int -> ST s (MVec s)
newMVec cap0 = do
  let cap = max 1 cap0
  arr <- newArray (0, cap - 1) 0
  arrRef <- newSTRef arr
  c <- newSTRef cap
  n <- newSTRef 0
  pure (MVec arrRef c n)

mvFromList :: [Int] -> ST s (MVec s)
mvFromList xs = do
  v <- newMVec (max 1 (length xs))
  mapM_ (mvPush v) xs
  pure v

mvLen :: MVec s -> ST s Int
mvLen = readSTRef . mvSize

-- | 越界读到 0（对齐各记号对 vector 的防御性访问约定）。
mvRead :: MVec s -> Int -> ST s Int
mvRead v i = do
  n <- readSTRef (mvSize v)
  if i < 0 || i >= n
    then pure 0
    else readSTRef (mvArr v) >>= \arr -> readArray arr i

mvReadDef :: MVec s -> Int -> Int -> ST s Int
mvReadDef v i d = do
  n <- readSTRef (mvSize v)
  if i < 0 || i >= n
    then pure d
    else readSTRef (mvArr v) >>= \arr -> readArray arr i

mvWrite :: MVec s -> Int -> Int -> ST s ()
mvWrite v i x = do
  n <- readSTRef (mvSize v)
  if i >= 0 && i < n
    then do
      arr <- readSTRef (mvArr v)
      writeArray arr i x
    else pure ()

mvPush :: MVec s -> Int -> ST s ()
mvPush v x = do
  n <- readSTRef (mvSize v)
  ensureCap v (n + 1)
  arr <- readSTRef (mvArr v)
  writeArray arr n x
  writeSTRef (mvSize v) (n + 1)

-- | 改变逻辑长度；增长部分填 0（对齐 @vector::resize@）。
mvResize :: MVec s -> Int -> ST s ()
mvResize v newLen0 = do
  let newLen = max 0 newLen0
  n <- readSTRef (mvSize v)
  if newLen <= n
    then writeSTRef (mvSize v) newLen
    else do
      ensureCap v newLen
      arr <- readSTRef (mvArr v)
      mapM_ (\i -> writeArray arr i 0) [n .. newLen - 1]
      writeSTRef (mvSize v) newLen

-- 只改逻辑长度，不初始化新增槽位；调用方必须先写入再读，否则可能读到旧数组残留值。
mvSetLen :: MVec s -> Int -> ST s ()
mvSetLen v newLen0 = do
  let newLen = max 0 newLen0
  ensureCap v newLen
  writeSTRef (mvSize v) newLen

ensureCap :: MVec s -> Int -> ST s ()
ensureCap v need = do
  cap <- readSTRef (mvCap v)
  if need <= cap
    then pure ()
    else do
      let cap' = max need (cap * 2)
      old <- readSTRef (mvArr v)
      arr' <- newArray (0, cap' - 1) 0
      n <- readSTRef (mvSize v)
      copy 0 n old arr'
      writeSTRef (mvArr v) arr'
      writeSTRef (mvCap v) cap'
 where
  copy i hi src dst
    | i >= hi = pure ()
    | otherwise = do
        x <- readArray src i
        writeArray dst i x
        copy (i + 1) hi src dst

mvToList :: MVec s -> ST s [Int]
mvToList v = do
  n <- readSTRef (mvSize v)
  arr <- readSTRef (mvArr v)
  mapM (readArray arr) [0 .. n - 1]

mvLast :: MVec s -> ST s Int
mvLast v = do
  n <- readSTRef (mvSize v)
  if n == 0 then pure 0 else readSTRef (mvArr v) >>= \arr -> readArray arr (n - 1)
