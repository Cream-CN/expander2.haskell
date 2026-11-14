-- | DEN 族公共骨架，逐行对齐 C++ @Header/notation/denshare.hpp@。
--
--   * 'DenEntry' = 条目（值 + 标记）；
--   * 文本解析器（只跳过空格，数字按 long long 累积并截断到 INT_MAX）；
--   * 'pleasantUntil' / 'apValue' / 切片辅助 / 极限行构造；
--   * 按值降序的稳定排序（对齐 @std::stable_sort@ + greater）。
module OmegaY.Fam.Den.Share
  ( DenEntry (..)
  , maxInt32
  , skipSpaces
  , peek
  , matchLit
  , parseDigits
  , rowValuesE
  , pleasantUntil
  , apValue
  , appendSliceExceptTail
  , appendSliceFrom
  , limitRowEntries
  , sortEntriesDesc
  , atV
  , atE
  , rowAtE
  ) where

import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Ord (Down (..))

-- | 条目：整数值 + 是否带标记。
data DenEntry = DenEntry
  { deValue :: !Int
  , deMarked :: !Bool
  } deriving (Eq, Show)

-- | INT_MAX（C++ 侧 parse_digits 的截断上限）。
maxInt32 :: Int
maxInt32 = 2147483647

-- ---------------------------------------------------------------------
-- 文本解析（对齐 TextParser）
-- ---------------------------------------------------------------------

-- | 只跳过空格 @' '@（不跳 tab / 换行）。
skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

peek :: String -> Char
peek [] = '\0'
peek (c : _) = c

-- | 匹配字面量并消费；成功返回剩余串。
matchLit :: String -> String -> Maybe String
matchLit lit s =
  let n = length lit
   in if take n s == lit then Just (drop n s) else Nothing

-- | 解析一串十进制数字（先跳过空格）；越 10 位按 INT_MAX 截断。
parseDigits :: String -> Maybe (Int, String)
parseDigits s0 =
  let s = skipSpaces s0
      (ds, rest) = span isDigit s
   in if null ds
        then Nothing
        else
          let v = foldl step 0 ds
           in Just (v, rest)
 where
  step a d =
    let v = a * 10 + (fromEnum d - fromEnum '0')
     in if v > maxInt32 then maxInt32 else v

-- ---------------------------------------------------------------------
-- 工具
-- ---------------------------------------------------------------------

-- | 行的条目值列表。
rowValuesE :: [DenEntry] -> [Int]
rowValuesE = map deValue

-- | 越界读 0。
atV :: [Int] -> Int -> Int
atV xs i = if i >= 0 && i < length xs then xs !! i else 0

-- | 越界读 0。
atE :: [DenEntry] -> Int -> Int
atE es i = if i >= 0 && i < length es then deValue (es !! i) else 0

-- | 越界读空行。
rowAtE :: [[a]] -> Int -> [[a]]
rowAtE rows i = if i >= 0 && i < length rows then [rows !! i] else []

-- ---------------------------------------------------------------------
-- pleasant_until / ap
-- ---------------------------------------------------------------------

indexOfFirstLess :: [Int] -> Int -> Int
indexOfFirstLess v limit = go 0
 where
  go i
    | i >= length v = (-1)
    | v !! i < limit = i
    | otherwise = go (i + 1)

indexOfLastGreater :: [Int] -> Int -> Int
indexOfLastGreater v limit = go (length v - 1)
 where
  go i
    | i < 0 = (-1)
    | v !! i > limit = i
    | otherwise = go (i - 1)

containsAnyNotIn :: [Int] -> Int -> Int -> [Int] -> Bool
containsAnyNotIn v i1 i2 tcheck =
  any (\i -> i < length v && not (v !! i `elem` tcheck)) [i1 .. i2]

-- | 第一个"不愉快"行下标；找不到返回 -1。
pleasantUntil :: [[Int]] -> [Int] -> Int
pleasantUntil rows tcheck
  | null tcheck = (-1)
  | otherwise = go 0
 where
  tmax = head tcheck
  tmin = last tcheck
  go n
    | n >= length rows = (-1)
    | i1 >= 0 && i2 >= 0 && i1 <= i2 && containsAnyNotIn scheck i1 i2 tcheck = n
    | otherwise = go (n + 1)
   where
    scheck = rows !! n
    i1 = indexOfFirstLess scheck tmax
    i2 = indexOfLastGreater scheck tmin

lastIndexOf :: [Int] -> Int -> Maybe Int
lastIndexOf v x = go (length v - 1)
 where
  go i
    | i < 0 = Nothing
    | v !! i == x = Just i
    | otherwise = go (i - 1)

-- | ap 单值映射。tvals 为 active 行条目值（不含 step），step 为 active.step。
apValue :: Int -> [Int] -> Int -> Int
apValue x tvals step
  | null tvals = x
  | x < last tvals = x
  | inRange step && x >= tvals !! step = x - tvals !! step + head tvals
  | otherwise = case lastIndexOf tvals x of
      Nothing -> x
      Just j -> if j < step then x else tvals !! (j - step)
 where
  inRange i = i >= 0 && i < length tvals

-- ---------------------------------------------------------------------
-- 切片（对齐 TS slice(from, -tail) / slice(from)）
-- ---------------------------------------------------------------------

-- | row[from .. size-tail)；tail<=0 或 tail>=size 时为空。
appendSliceExceptTail :: [a] -> Int -> Int -> [a]
appendSliceExceptTail row from tail
  | tail <= 0 = []
  | tail >= length row = []
  | otherwise = take (length row - tail) (drop (max from 0) row)

-- | row[from..]（from<0 按 0）。
appendSliceFrom :: [a] -> Int -> [a]
appendSliceFrom row from = drop (max from 0) row

-- | DEN2 / DEN3 极限行条目：[0..n+2] 按标记规则后追加 2 再整体反转。
limitRowEntries :: Int -> [DenEntry]
limitRowEntries n =
  reverse
    ( [ DenEntry nn (3 <= nn && nn < 2 + n) | nn <- [0 .. n + 2] ]
        ++ [DenEntry 2 False]
    )

-- | 按值降序稳定排序（对齐 stable_sort + greater）。
sortEntriesDesc :: [DenEntry] -> [DenEntry]
sortEntriesDesc es = map fst (sortOn (\(e, idx) -> (Down (deValue e), idx)) (zip es [0 ..]))
