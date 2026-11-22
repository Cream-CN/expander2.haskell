-- | 平面族 m1ynss（id @-1y-(n+1)ss@），逐字移植 @notation/m1ynss.hpp@。
--   表达式 Expr = 列数组；每列 = [长度 n 的父标号 arr, 末尾步长 step:Int]。
module OmegaY.Fam.M1ynss.Flat
  ( expand
  ) where

import OmegaY.Fam.M1ynss.PP

data Column = Column { cArr :: [Int], cStep :: Int }
data Expr = Expr { eCols :: [Column] }

numberCmp :: Int -> Int -> Int
numberCmp a b = if a < b then -1 else if a > b then 1 else 0

indexOfLastNonneg :: [Int] -> Int
indexOfLastNonneg v = go (length v - 1)
  where
    go i
      | i < 0 = -1
      | v !! i >= 0 = i
      | otherwise = go (i - 1)

-- | 父表：每列 arr 存父列下标（长度 n，初值 -1），step 存一个下标。
data PCol = PCol { pArr :: [Int], pStep :: Int }

parents :: Expr -> Int -> [PCol]
parents e n = reverse built
  where
    cols = eCols e
    built = foldl addCol [] (zip [0 ..] cols)
    -- prev 为“已建好的第 0..i-1 列”的逆序：prev!!0 = 第 i-1 列。
    prevCol prev p = prev !! (length prev - 1 - p)
    addCol prev (i, col) = buildCol : prev
      where
        arr0 = replicate n (-1)
        arr1 = foldl lowerPass arr0 [0 .. n - 1]
        v = cStep col
        p0 = if n == 0 then i - 1 else arr1 !! (n - 1)
        stepP = traceStep p0 v
        buildCol = PCol arr1 stepP
        lowerPass arr j =
          let vv = if j < length (cArr col) then cArr col !! j else 0
              pStart = if j == 0 then i - 1 else arr !! (j - 1)
              p = walkLow pStart j vv
           in if p < 0 then arr else replaceAt j p arr
        walkLow p jj vv
          | p < 0 = p
          | (cArr (cols !! p) !! jj) < vv = p
          | otherwise =
              let pNext =
                    if jj == 0
                      then p - 1
                      else pArr (prevCol prev p) !! (jj - 1)
               in walkLow pNext jj vv
        traceStep p vv
          | p < 0 = p
          | cStep (cols !! p) < vv = p
          | otherwise =
              let pNext =
                    if n == 0
                      then p - 1
                      else pArr (prevCol prev p) !! (n - 1)
               in traceStep pNext vv

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt k v xs = take k xs ++ [v] ++ drop (k + 1) xs

root :: [PCol] -> Int -> Maybe (Int, Int)
root pcols n
  | null pcols = Nothing
  | otherwise =
      let right = length pcols - 1
       in if pStep (pcols !! right) >= 0
            then Just (pStep (pcols !! right), n)
            else
              let b = indexOfLastNonneg (pArr (pcols !! right))
               in if b == -1 then Nothing else Just (pArr (pcols !! right) !! b, b)

ascensionVector :: Expr -> Int -> Int -> [Int]
ascensionVector e r b =
  let cols = eCols e
      tail = last cols
      base = cols !! r
   in [cArr tail !! j - cArr base !! j | j <- [0 .. b - 1]]

ascensionThresholds :: [PCol] -> Int -> Int -> [Int]
ascensionThresholds pcols r b = [aiAt i | i <- [0 .. len - 1]]
  where
    len = length pcols
    aiAt i
      | i == r = b
      | otherwise = walk 0
      where
        walk ai
          | ai >= b = ai
          | otherwise =
              let p = climb i ai
               in if p < r then ai else walk (ai + 1)
        climb p ai
          | p <= r = p
          | otherwise = climb (pArr (pcols !! p) !! ai) ai

ascend :: Column -> [Int] -> Int -> Int -> Column
ascend col delta b w =
  let arr = cArr col
      newArr = [if j < b then arr !! j + delta !! j * w else arr !! j | j <- [0 .. length arr - 1]]
   in Column newArr (cStep col)

fs :: Expr -> Int -> Int -> Expr
fs e index n
  | null (eCols e) = e
  | otherwise =
      let pcols = parents e n
       in case root pcols n of
            Nothing -> e { eCols = init (eCols e) }
            Just (r, b) ->
              let cols = eCols e
                  right = length cols - 1
                  width = right - r
                  v = ascensionVector e r b
                  a = ascensionThresholds pcols r b
                  base = init cols
                  expanded = foldl (addWave width r b v a) base [1 .. index]
               in e { eCols = expanded }
  where
    right = length (eCols e) - 1
    addWave width r b v a cols0 w =
      let mid = cols0 ++ [ascend (eCols e !! i) v (a !! i) w | i <- [r .. right - 1]]
       in if b == n
            then replaceStepAt (r + w * width) (cStep (eCols e !! right) - 1) mid
            else mid
    replaceStepAt k v xs = take k xs ++ [(xs !! k) { cStep = v }] ++ drop (k + 1) xs

columnDisplay :: Column -> String
columnDisplay c =
  let list = cArr c ++ [cStep c]
      trimmed = reverse (dropWhile (== 0) (reverse list))
   in "(" ++ uncommas trimmed ++ ")"
  where
    uncommas xs = foldr (\x acc -> show x ++ (if null acc then "" else "," ++ acc)) "" xs

display :: Expr -> String
display e = concatMap columnDisplay (eCols e)

parseExpr :: Int -> P Expr
parseExpr n = do
  sp
  do
      sp
      cols <- manyCols
      sp
      end <- atEnd
      if end then return (Expr cols) else err
  where
    manyCols = do
      sp
      mb <- peek
      case mb of
        Just '(' -> do
          c <- parseColumn n
          rest <- manyCols
          return (c : rest)
        _ -> return []

parseColumn :: Int -> P Column
parseColumn n = do
  sp
  mb <- peek
  case mb of
    Just '(' -> advance >> return ()
    _ -> err
  sp
  mb2 <- peek
  values <-
    if mb2 == Just ')'
      then return []
      else do
        v <- numTok
        vs <- moreVals
        return (v : vs)
  sp
  mb3 <- peek
  case mb3 of
    Just ')' -> advance >> return ()
    _ -> err
  let arr = [if k < length values then values !! k else 0 | k <- [0 .. n - 1]]
      step = if length values > n then values !! n else 0
  return (Column arr step)
  where
    moreVals = do
      sp
      mb <- peek
      case mb of
        Just ',' -> do
          advance
          sp
          mb2 <- peek
          if mb2 == Just ')'
            then return []
            else do
              v <- numTok
              vs <- moreVals
              return (v : vs)
        _ -> return []

expand :: String -> Int -> Int -> String
expand text term n0 =
  let n = max 0 (min 25 n0)
      term' = max 1 term
   in case runP text (parseExpr n) of
        Left _ -> kParseError
        Right (m, _) -> display (fs m term' n)
