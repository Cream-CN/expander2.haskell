-- | DEN1 记号（id=den1），逐行对齐 C++ @Header/notation/den1.hpp@。
--
--   行结构：@rows[0]@ 是父标号列表（显示在 @;@ 之后），其余行 @row[0]@ 为 step。
module OmegaY.Fam.Den.Den1
  ( expandText
  , expandTextAlter
  ) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import OmegaY.Fam.Den.Share

-- ---------------------------------------------------------------------
-- 类型与显示
-- ---------------------------------------------------------------------

data Den1Expr = Den1Expr
  { d1Infinity :: !Bool
  , d1Rows :: [[Int]]
  }

-- | TS: toShort —— 主行 slice(1, -step) 再补最后一个值。
toShortRow :: [Int] -> [Int]
toShortRow row =
  appendSliceExceptTail row 1 (if null row then 0 else head row)
    ++ (if null row then [] else [last row])

toString :: Den1Expr -> String
toString e
  | d1Infinity e = "Limit"
  | otherwise =
      concatMap rowStr (drop 1 (d1Rows e))
        ++ ";"
        ++ parentStr (if null (d1Rows e) then [] else head (d1Rows e))
 where
  rowStr row =
    "("
      ++ joinComma (drop 1 row)
      ++ ")"
      ++ show (head row)
  parentStr ps = joinComma ps
  joinComma [] = ""
  joinComma [x] = show x
  joinComma (x : xs) = show x ++ "," ++ joinComma xs

-- ---------------------------------------------------------------------
-- 解析
-- ---------------------------------------------------------------------

-- | 解析一行：'(' 值列表 ')' step。
parseRow :: String -> Maybe ([Int], String)
parseRow s0 = do
  let s1 = drop 1 s0 -- '('
      s2 = skipSpaces s1
  (vals, s3) <- parseValues s2
  if peek s3 /= ')'
    then Nothing
    else do
      let s4 = drop 1 s3
          s5 = skipSpaces s4
      (step, s6) <- parseDigits s5
      Just (step : vals, s6)
 where
  parseValues s
    | eof s || peek s == ')' = Just ([], s)
    | otherwise = loop s []
  loop s acc = do
    (v, s1) <- parseDigits s
    let s2 = skipSpaces s1
    if peek s2 == ','
      then loop (skipSpaces (drop 1 s2)) (acc ++ [v])
      else Just (acc ++ [v], s2)
  eof = null

parse :: String -> Maybe Den1Expr
parse s0 = do
  let s1 = skipSpaces s0
  case matchLit "Limit" s1 of
    Just rest ->
      let s2 = skipSpaces rest
       in if null s2 then Just (Den1Expr True []) else Nothing
    Nothing -> do
      (rows, s2) <- parseBody s1 []
      let s3 = skipSpaces s2
      if null s3 || peek s3 /= ';'
        then Nothing
        else do
          let s4 = skipSpaces (drop 1 s3)
          (parents, s5) <- parseParents s4 []
          if null s5
            then Just (Den1Expr False (parents : rows))
            else Nothing
 where
  parseBody s acc
    | null s || peek s == ';' = Just (acc, s)
    | peek s /= '(' = Nothing
    | otherwise = do
        (row, s1) <- parseRow s
        parseBody (skipSpaces s1) (acc ++ [row])
  parseParents s acc
    | null s = Just (acc, s)
    | otherwise = do
        (v, s1) <- parseDigits s
        let s2 = skipSpaces s1
        if peek s2 == ','
          then
            let rest = skipSpaces (drop 1 s2)
             in if null rest then Nothing else parseParents rest (acc ++ [v])
          else Just (acc ++ [v], s2)

-- | 组装表达式：rows[0]=父标号列表。
-- ---------------------------------------------------------------------
-- 展开核心
-- ---------------------------------------------------------------------

-- | 删除最后一行，并弹出父标号列表最后一个元素。
cut :: Den1Expr -> Den1Expr
cut raw
  | null (d1Rows raw) = Den1Expr False []
  | otherwise =
      let rows = init (d1Rows raw)
          parents = if null rows then [] else initRow (head rows)
          initRow p = if null p then [] else init p
          rows' = if null rows then [] else [parents] ++ drop 1 rows
       in Den1Expr False rows'

-- | isAncestor(R, i, j)：R 为父标号列表，j 为 1 起主行下标。
isAncestor :: [Int] -> Int -> Int -> Bool
isAncestor r i j = go j
 where
  go jj
    | i == jj = True
    | not (i < jj) || jj <= 0 || jj > length r = False
    | otherwise = go (r !! (jj - 1))

-- | 为新复制行计算父标号。
computeParent :: [Int] -> Int -> Int -> Int -> Int -> Int -> Int
computeParent rOld rowIdx start end oldHeight tmin =
  let parent = if rowIdx >= 1 && rowIdx <= length rOld then rOld !! (rowIdx - 1) else 0
   in if parent /= 0 && start <= parent && parent <= end
        then parent - start + oldHeight
        else walk parent
 where
  walk ancestor
    | ancestor == 0 = 0
    | ancestor < tmin = ancestor
    | ancestor >= 1 && ancestor <= length rOld = walk (rOld !! (ancestor - 1))
    | otherwise = 0

-- | 按 active 行的映射规则复制一行。
applyRow :: [Int] -> [Int] -> [Int]
applyRow s t =
  let tvals = drop 1 t
      step = head t
   in (if null s then 0 else head s)
        : [apValue (s !! k) tvals step | k <- [1 .. length s - 1]]

-- | 复制 [begin, end) 区间行并补算父标号。
copy :: Den1Expr -> Int -> Den1Expr
copy raw flag
  | null (d1Rows raw) || null (last (d1Rows raw)) = cut raw
  | otherwise =
      let active = last (d1Rows raw)
          expr = cut raw
          begin = active !! (1 + head active)
          end = if flag >= 0 then begin + flag else length (d1Rows raw) + 1
          oldHeight = length (d1Rows raw) - 1
          copied =
            [ applyRow ((d1Rows raw) !! i) active | i <- [begin .. end - 1] ]
          parentsNew =
            [ computeParent (head (d1Rows raw)) rowIdx begin (end - 1) oldHeight (last active)
            | rowIdx <- [begin .. end - 1]
            ]
          rows' = d1Rows expr ++ copied
       in Den1Expr False (applyParents rows' parentsNew)
 where
  applyParents rows parents =
    if null rows then parents : [] else [head rows ++ parents] ++ drop 1 rows

-- | 复制 [begin, raw.length) 区间行（含 active 自身）。
extend :: Den1Expr -> Den1Expr
extend raw
  | null (d1Rows raw) || null (last (d1Rows raw)) = cut raw
  | otherwise =
      let active = last (d1Rows raw)
          expr = cut raw
          begin = active !! (1 + head active)
          oldHeight = length (d1Rows raw) - 1
          copied =
            [ applyRow ((d1Rows raw) !! i) active | i <- [begin .. length (d1Rows raw) - 1] ]
          parentsNew =
            [ computeParent (head (d1Rows raw)) rowIdx begin oldHeight oldHeight (last active)
            | rowIdx <- [begin .. length (d1Rows raw) - 1]
            ]
          rows' = d1Rows expr ++ copied
       in Den1Expr False (applyParents rows' parentsNew)
 where
  applyParents rows parents =
    if null rows then parents : [] else [head rows ++ parents] ++ drop 1 rows

-- | 对第 i 行做压缩。
comp :: Den1Expr -> Int -> [Int] -> Den1Expr
comp raw i t =
  let rows0 = take i (d1Rows raw)
      u = length t
      ri = (d1Rows raw) !! i
      shortRow = length ri < head ri * 2 + 1
      li = if shortRow then head ri else head ri + 1
      ci =
        if shortRow
          then appendSliceExceptTail ri 1 (head ri) ++ appendSliceFrom ri (1 + head ri)
          else appendSliceFrom ri 1
      -- 第一批：对 r=0..u-1 构造新行，落到 rows0 的 i+r 位置
      batch1 =
        [ ( i + r
          , (li + r)
              : ( ci
                    ++ take (1 + r) t
                    ++ [ri `atV` 1 + 1 + rr | rr <- [0 .. r - 1]]
                )
          )
        | r <- [0 .. u - 1]
        ]
      -- 后续行平移
      m x = if x < i then x else x + u
      laterRows = [ii | ii <- [i .. length (d1Rows raw) - 1]]
      buildLater ii =
        let rawRow = (d1Rows raw) !! ii
            vals = [ if x <= i then x else x + u | x <- drop 1 rawRow ]
            findIdx =
              case [ k | k <- [0 .. length vals - 1], vals !! k <= i ] of
                (k : _) -> k
                [] -> (-1)
            flag =
              isAncestor (head (d1Rows raw)) i ii
                && (findIdx == (-1) || findIdx <= head rawRow)
            vals' =
              if flag
                then vals ++ t ++ [i + 1 + uu | uu <- [0 .. u - 1]]
                else vals
            valsSorted = sortDesc vals'
         in ( ii + u
            , (head rawRow + (if flag then u else 0)) : valsSorted
            )
      batch2 = map buildLater laterRows
      -- 合成主体行：先 rows0，再把 batch1/batch2 按位置写入/追加
      placed = foldl applyBatch rows0 (batch1 ++ batch2)
      -- 父标号列表
      parentsOld = head (d1Rows raw)
      parents0 = take (min i (length parentsOld)) parentsOld
      parents1 = parents0 ++ [i + r | r <- [0 .. u - 1]]
      parentsFinal =
        foldl
          ( \acc ii ->
              if ii - 1 < length parentsOld
                then
                  let idx = m ii - 1
                   in if idx >= length acc
                        then acc ++ [m (parentsOld !! (ii - 1))]
                        else setAt idx (m (parentsOld !! (ii - 1))) acc
                else acc
          )
          parents1
          [i + 1 .. length (d1Rows raw) - 1]
      rows1 =
        if null placed
          then [parentsFinal]
          else setRow0 placed parentsFinal
   in Den1Expr False rows1
 where
  sortDesc xs = map fst (sortOn (\(x, k) -> (Down x, k)) (zip xs [0 ..]))
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]
  applyBatch rows (pos, row) =
    if pos < length rows then setAt pos row rows else rows ++ [row]
  setRow0 rows p = setAt 0 p rows

-- | 对第 i 行构造 T 后调用 comp；T 为空则保持原样。
fullcomp :: Den1Expr -> Int -> Den1Expr
fullcomp expr i =
  let row = (d1Rows expr) !! i
      t0 = [row `atV` head row]
      t1 = build t0
      t2 = if null t1 then [] else tail t1
      t3 = if null t2 then [] else init t2
   in if null t3 then expr else comp expr i t3
 where
  build t =
    let rowIdx = head t
        v = if rowIdx >= 0 && rowIdx < length (d1Rows expr)
              then ((d1Rows expr) !! rowIdx) `atV` 2
              else 0
        t' = v : t
        row = (d1Rows expr) !! i
        th = row `atV` (head row + 1)
     in if t' !! 0 > th then build t' else t'

-- | 核心展开。longer=False 对应 FS，True 对应 FS_alter。
expand :: Den1Expr -> Int -> Bool -> Den1Expr
expand raw fsTerm longer
  | null (d1Rows raw) = raw
  | otherwise =
      let active = last (d1Rows raw)
          idx = 1 + head active
       in if null active || idx >= length active || active !! idx == 0
            then cut raw
            else
              let rows =
                    [ drop 1 ((d1Rows raw) !! i)
                    | i <- [active !! idx .. length (d1Rows raw) - 2]
                    ]
                  tcheck = drop idx active
                  flag = pleasantUntil rows tcheck
                  expr0 = raw
                  expr1 =
                    if flag >= 0
                      then copy expr0 flag
                      else
                        let e = foldl (\e _ -> extend e) expr0 [1 .. fsTerm]
                         in if longer then copy e 1 else cut e
                  -- 对复制区逐行 fullcomp
                  expr2 =
                    foldl
                      ( \e i ->
                          let row = (d1Rows e) !! i
                           in if length row <= head row * 2 + 1
                                then fullcomp e i
                                else e
                      )
                      expr1
                      [length (d1Rows raw) - 1 .. length (d1Rows expr1) - 1]
               in expr2

-- ---------------------------------------------------------------------
-- 极限
-- ---------------------------------------------------------------------

limitR :: Int -> [Int]
limitR n
  | n == 0 = [0, 0]
  | otherwise = [0, 0, 0] ++ [3 + nn | nn <- [0 .. n - 2]]

limitRow :: Int -> [Int]
limitRow n = reverse ([0 .. n + 2] ++ [2])

limit :: Int -> Den1Expr
limit n =
  Den1Expr
    False
    ( [ limitR n
      , [1, 1, 0]
      , [1, 2, 1, 0]
      ]
        ++ [limitRow (1 + nn) | nn <- [0 .. n - 1]]
    )

-- ---------------------------------------------------------------------
-- 公共入口
-- ---------------------------------------------------------------------

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = max 1 term
   in case parse text of
        Nothing -> "解析失败，请确认你没有使用全角括号，逗号，分号"
        Just e
          | d1Infinity e -> toString (limit term')
          | length (d1Rows e) <= 1 -> toString (Den1Expr False [[]])
          | otherwise -> toString (expand e term' longer)

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandTextAlter :: String -> Int -> String
expandTextAlter text term = expandTextImpl text term True
