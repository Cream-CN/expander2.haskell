-- | SRD（11242 家族）：忠实移植 C++ @notation/srd11242.hpp@。
module OmegaY.Fam.G11242.Srd
  ( srdNotation
  , expandSrd
  ) where

import Data.Char (isDigit, isSpace, toLower)
import OmegaY.Fam.G11242.State
import OmegaY.Notation.Types (Notation, textNotation)

type Expr = [[Int]]

kMaxColumns, kMaxCells :: Int
kMaxColumns = 8192
kMaxCells = 250000

budget :: M Int ()
budget = do
  w <- get
  if w + 1 > kMaxCells
    then failM "计算预算不足"
    else put (w + 1)

failM :: String -> M s a
failM msg = M $ \_ -> Left msg

-- | C++ @fs@：保留末项的单步展开。
fs :: Expr -> Int -> M Int Expr
fs graph n
  | n < 0 = failM "需要非负整数"
  | null graph || n == 0 || null (last graph) =
      pure (if null graph then [] else init graph)
  | otherwise = do
      let span_ = length graph - last (last graph)
      if length graph - 1 + n * span_ > kMaxColumns
        then failM "展开列数超出预算"
        else do
          let cells0 = sum (map length graph)
          go n span_ cells0 graph
 where
  go 0 _ cells result = pure (init result)
  go blocks span_ cells result = do
    budget
    let column = last result
        parent = last column
        copies =
          [ moveCol parent span_ (result !! s)
          | s <- [parent .. length result - 1]
          ]
        pc = if parent /= 0 then result !! (parent - 1) else []
        seam = init column ++ drop (length column - 1) pc
        colSize = length seam - length column
        sizeSum = sum [ sizeOf parent span_ (result !! s) | s <- [parent .. length result - 1] ]
        cells' = cells + sizeSum + colSize
    if cells' > kMaxCells
      then failM "单元数超出预算"
      else do
        -- C++ 逐列做预算 tick 与单元计数，超限即中止；这里在累计后统一判定。
        mapM_ (\_ -> budget) [parent .. length result - 1]
        go (blocks - 1) span_ cells' (init result ++ [seam] ++ copies)

  sizeOf parent span_ source =
    length source + (if length source > parent then span_ else 0)

  moveCol parent span_ source =
    let moved = [ if p >= parent then p + span_ else p | p <- source ]
        headPart = take parent moved
        midPart = if length moved > parent then replicate span_ (moved !! parent) else []
        tailPart = drop parent moved
     in headPart ++ midPart ++ tailPart

-- | 父项列表的合法性检查（逐字对齐 C++ 的 check）。
check :: Expr -> M Int ()
check g = do
  if length g > kMaxColumns then failM "列数超出预算" else pure ()
  let go _ _ [] = pure ()
      go cells j (col : rest) = do
        let cells' = cells + length col
        if cells' > kMaxCells then failM "单元数超出预算" else pure ()
        mapM_ (checkCell j col) (zip [0 :: Int ..] col)
        go cells' (j + 1) rest
      checkCell j col (row, p)
        | p < 0 = failM "需要非负整数"
        | p >= j + 1 || p < length col - 1 || (row > 0 && col !! (row - 1) < p) =
            failM "父列表须非增，且满足 高度−1≤父列<子列"
        | otherwise = pure ()
  go 0 0 g

plain :: Expr -> String
plain g
  | null g = "0"
  | otherwise =
      concat
        [ "[" ++ joinComma col ++ "]"
        | col <- g
        ]

joinComma :: [Int] -> String
joinComma [] = ""
joinComma [x] = show x
joinComma (x : xs) = show x ++ concat ["," ++ show y | y <- xs]

-- | 去掉所有空白，与 C++ 的预处理一致。
stripAll :: String -> String
stripAll = filter (not . isSpace)

-- | 大小写不敏感的前缀匹配，返回匹配后的剩余串。
eatCI :: String -> String -> Maybe String
eatCI w s
  | length s < length w = Nothing
  | and (zipWith (\a b -> toLower a == toLower b) w (take (length w) s)) =
      Just (drop (length w) s)
  | otherwise = Nothing

readDigits :: String -> (Int, String, Bool)
readDigits s0 =
  let (ds, rest) = span isDigit s0
   in (foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0 ds, rest, not (null ds))

-- | 解析：路由 @SRD[n]...@ / @SRPD[n]...@、纯数字空列、@[..][..]@ 父项列表。
parse :: String -> M Int Expr
parse raw = case eatCI "SRD" s of
  Just rest -> route rest
  Nothing -> case eatCI "SRPD" s of
    Just rest -> route rest
    Nothing -> ordinary
 where
  s = stripAll raw

  route rest0 = go 128 [[0]] rest0
   where
    go 0 acc rest
      | null rest = pure acc
      | otherwise = failM "表达式后有未识别内容"
    go steps acc rest = case rest of
      ('[' : r1) ->
        let (v, r2, any) = readDigits r1
         in if not any
              then failM "路由展开格式错误"
              else case r2 of
                (']' : r3) -> do
                  put 0
                  acc' <- fs acc v
                  go (steps - 1) acc' r3
                _ -> failM "路由展开格式错误"
      _ ->
        if null rest
          then pure acc
          else failM "表达式后有未识别内容"

  ordinary
    | null s || s == "∅" = pure []
    | all isDigit s =
        let n = foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0 s
         in if n > kMaxColumns then failM "输入过大" else pure (replicate n [])
    | otherwise = columns 0 []

  columns i out
    | i >= length s =
        if null out
          then failM "请输入父项列表，如 [][1][2,2]"
          else check out >> pure out
    | s !! i /= '[' = failM "请输入父项列表，如 [][1][2,2]"
    | otherwise = col (i + 1) [] out
   where
    col j acc out0
      | j < length s && s !! j /= ']' =
          let (v, rest, any) = readDigits (drop j s)
              j' = j + (length (drop j s) - length rest)
           in if any
                then case rest of
                  (',' : r2) -> col (j' + 1) (acc ++ [v]) out0
                  _ -> close j' (acc ++ [v]) out0
                else case rest of
                  (',' : _) -> col (j' + 1) acc out0
                  _ -> close j' acc out0
      | otherwise = close j acc out0
    close j acc out0 =
      if j >= length s || s !! j /= ']'
        then failM "请输入父项列表，如 [][1][2,2]"
        else columns (j + 1) (out0 ++ [acc])

expandSrd :: String -> Int -> String
expandSrd text term =
  case runM (parse text >>= \g -> fs g term) 0 of
    Left e -> e
    Right out -> plain out

srdNotation :: Notation
srdNotation = textNotation "srd" "SRD" ["11242", "文本"] expandSrd ""
