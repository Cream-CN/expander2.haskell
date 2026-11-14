-- | 递归族 tm1（id @t--1y-(n+1)ss@）及其分支变体 abs/rel-bm1 共用的核心：
--   逐字移植 @notation/tm1ynss.hpp@ 的 @tm1_detail@（Node/Column/step 递归树）。
module OmegaY.Fam.M1ynss.Tm1
  ( Node(..)
  , Column(..)
  , makeNode
  , isInfinity
  , numberCmp
  , compareNode
  , isZeroColumn
  , isOneColumn
  , isLimit
  , infinityFs
  , PCol(..)
  , parents
  , root
  , ascensionVector
  , ascensionThresholds
  , ascend
  , popLast
  , fs
  , lnz
  , render
  , parseNode
  , omegaChar
  , expandTm1
  ) where

import OmegaY.Fam.M1ynss.PP

-- | 列：arr 长度 n；step 为递归子表达式。
data Column = Column { cArr :: [Int], cStep :: Node }
data Node = Node { nInf :: Bool, nCols :: [Column] }

makeNode :: Node
makeNode = Node False []

isInfinity :: Node -> Bool
isInfinity e = nInf e

numberCmp :: Int -> Int -> Int
numberCmp a b = if a < b then -1 else if a > b then 1 else 0

lexNum :: [Int] -> [Int] -> Int
lexNum a b = go 0
  where
    m = min (length a) (length b)
    go k
      | k >= m = numberCmp (length a) (length b)
      | otherwise = case numberCmp (a !! k) (b !! k) of
          0 -> go (k + 1)
          c -> c

compareNode :: Node -> Node -> Int
compareNode a b =
  let ai = isInfinity a
      bi = isInfinity b
   in if ai || bi
        then numberCmp (if ai then 1 else 0) (if bi then 1 else 0)
        else
          let ca = nCols a
              cb = nCols b
              m = min (length ca) (length cb)
           in case go 0 of
                0 -> numberCmp (length ca) (length cb)
                c -> c
  where
    go k
      | k >= min (length (nCols a)) (length (nCols b)) = 0
      | otherwise =
          let c = columnCmp (nCols a !! k) (nCols b !! k)
           in if c /= 0 then c else go (k + 1)

columnCmp :: Column -> Column -> Int
columnCmp a b = case lexNum (cArr a) (cArr b) of
  0 -> compareNode (cStep a) (cStep b)
  c -> c

isZeroColumn :: Column -> Bool
isZeroColumn c = all (== 0) (cArr c) && null (nCols (cStep c))

isOneColumn :: Column -> Bool
isOneColumn c =
  let arr = cArr c
      n = length arr
   in if n == 0
        then length (nCols (cStep c)) == 1
        else
          arr !! 0 == 1
            && null (nCols (cStep c))
            && all (== 0) (drop 1 arr)

isLimit :: Node -> Int -> Bool
isLimit e n
  | isInfinity e = True
  | null (nCols e) = False
  | otherwise =
      let lc = Prelude.last (nCols e)
       in if n == 0
            then not (null (nCols (cStep lc)))
            else not (null (cArr lc)) && cArr lc !! 0 > 0

infinityFs :: Int -> Int -> Node
infinityFs index n =
  let c0 = Column (replicate n 0) makeNode
   in if index > 0
        then Node False [c0, Column (replicate n 1) (infinityFs (index - 1) n)]
        else Node False [c0]

-- | 父表：每列 arr 存父列下标，step 存一个下标。
data PCol = PCol { pArr :: [Int], pStep :: Int }

indexOfLastNonneg :: [Int] -> Int
indexOfLastNonneg v = go (length v - 1)
  where
    go i
      | i < 0 = -1
      | v !! i >= 0 = i
      | otherwise = go (i - 1)

parents :: Node -> Int -> [PCol]
parents e n
  | isInfinity e = []
  | otherwise = reverse built
  where
    cols = nCols e
    built = foldl addCol [] (zip [0 ..] cols)
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
           in if p < 0 then arr else repAt j p arr
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
          | compareNode (cStep (cols !! p)) vv < 0 = p
          | otherwise =
              let pNext =
                    if n == 0
                      then p - 1
                      else pArr (prevCol prev p) !! (n - 1)
               in traceStep pNext vv

repAt :: Int -> Int -> [Int] -> [Int]
repAt k v xs = take k xs ++ [v] ++ drop (k + 1) xs

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

ascensionVector :: Node -> Int -> Int -> [Int]
ascensionVector e r b =
  let cols = nCols e
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

popLast :: Node -> Node
popLast e = Node (nInf e) (init (nCols e))

fs :: Node -> Int -> Int -> Node
fs e index n
  | isInfinity e = infinityFs index n
  | null (nCols e) = e
  | otherwise =
      let cols = nCols e
          right = length cols - 1
       in if isLimit (cStep (cols !! right)) n
            then
              Node False
                ([cols !! i | i <- [0 .. right - 1]]
                   ++ [Column (cArr (cols !! right)) (fs (cStep (cols !! right)) index n)])
            else
              let pcols = parents e n
               in case root pcols n of
                    Nothing -> popLast e
                    Just (r0, b) ->
                      let width = right - r0
                          v = ascensionVector e r0 b
                          a = ascensionThresholds pcols r0 b
                          baseCols = [cols !! i | i <- [0 .. right - 1]]
                          result0 = Node False baseCols
                          result = foldl (addWave width r0 b v a) result0 [1 .. index]
                       in result
  where
    cols = nCols e
    right = length cols - 1
    addWave width r0 b v a res w =
      let midCols =
            nCols res
              ++ [ascend (cols !! i) v (a !! i) w | i <- [r0 .. right - 1]]
          res1 = res { nCols = midCols }
       in if b == n
            then
              let k = r0 + w * width
                  old = nCols res1 !! k
                  newStep = popLast (cStep (cols !! right))
               in res1 { nCols = repColAt k old { cStep = newStep } (nCols res1) }
            else res1
    repColAt k c xs = take k xs ++ [c] ++ drop (k + 1) xs

lnz :: Node -> Int -> Int -> Node
lnz e index n
  | isInfinity e = infinityFs index n
  | not (isLimit e n) = fs e 0 n
  | otherwise = fs e index n

omegaChar :: String
omegaChar = "\x03C9"

render :: Node -> Bool -> String
render e topLevel
  | isInfinity e = "Limit"
  | not topLevel =
      let cols = nCols e
          allZero = all isZeroColumn cols
       in if allZero
            then show (length cols)
            else
              if length cols == 2
                && isZeroColumn (cols !! 0)
                && isOneColumn (cols !! 1)
                then omegaChar
                else concatMap columnDisplay cols
  | otherwise = concatMap columnDisplay (nCols e)

columnDisplay :: Column -> String
columnDisplay c =
  let list = map show (cArr c) ++ [render (cStep c) False]
      trimmed = reverse (dropWhile (== "0") (reverse list))
   in "(" ++ commas trimmed ++ ")"
  where
    commas xs = foldr (\x acc -> x ++ (if null acc then "" else "," ++ acc)) "" xs

-- | 解析：topLevel 由调用方决定（parseNode 对应 parse_expr(top_level=true)）。
parseNode :: Bool -> Int -> P Node
parseNode topLevel n = do
  sp
  isLim <- startsWith "Limit"
  if isLim
    then do
      sequence_ (replicate 5 advance)
      sp
      end <- atEnd
      if end then return (Node True []) else err
    else do
      early <-
        if not topLevel
          then bareNumber
          else return Nothing
      case early of
        Just e -> return e
        Nothing -> parseCols
  where
    -- 裸数字 / ω / w：命中则直接返回（C++ 提前 return，不再解析后续列）。
    bareNumber = do
      sp
      mb <- peek
      case mb of
        Just c | c >= '0' && c <= '9' -> do
          cnt <- numTok
          return $ Just $ Node False [Column (replicate n 0) makeNode | _ <- [1 .. cnt]]
        _ -> do
          isOmega <- startsWith "\xCF\x89"
          if isOmega
            then do
              advance >> advance
              return $ Just $ Node False [Column (replicate n 0) makeNode, oneColumn n]
            else do
              mb2 <- peek
              case mb2 of
                Just 'w' -> do
                  advance
                  return $ Just $ Node False [Column (replicate n 0) makeNode, oneColumn n]
                _ -> return Nothing
    parseCols = do
      sp
      mb <- peek
      case mb of
        Just '(' -> do
          c <- parseColumn n
          rest <- parseCols
          return (rest { nCols = c : nCols rest })
        _ -> return (Node False [])

oneColumn :: Int -> Column
oneColumn n =
  if n == 0
    then Column [] (Node False [Column [] makeNode])
    else
      let arr = replicate n 0
       in Column (repAt 0 1 arr) makeNode

parseColumn :: Int -> P Column
parseColumn n = do
  sp
  mb <- peek
  case mb of
    Just '(' -> advance >> return ()
    _ -> err
  sp
  (arr, step) <-
    if n == 0
      then do
        s <- parseNode False n
        return ([], s)
      else do
        arr1 <- foldNum (replicate n 0) [0 .. n - 1]
        sp
        mb2 <- peek
        step <-
          case mb2 of
            Just ',' -> do
              advance
              parseNode False n
            _ -> return makeNode
        return (arr1, step)
  sp
  mb3 <- peek
  case mb3 of
    Just ')' -> advance >> return ()
    _ -> err
  return (Column arr step)
  where
    foldNum arr [] = return arr
    foldNum arr (j : js) = do
      arr' <-
        if j > 0
          then do
            sp
            mb <- peek
            case mb of
              Just ',' -> advance >> return arr
              _ -> return arr
          else return arr
      sp
      mb <- peek
      arr'' <-
        case mb of
          Just c | c >= '0' && c <= '9' -> do
            v <- numTok
            return (repAt j v arr')
          _ -> return arr'
      foldNum arr'' js

-- | t--1y-(n+1)ss 记号入口：clamp n 到 [0,25]，term<1 置 1。
expandTm1 :: String -> Int -> Int -> String
expandTm1 text term n0 =
  let n = max 0 (min 25 n0)
      term' = max 1 term
      top = do
        sp
        e <- parseNode True n
        sp
        end <- atEnd
        if end then return e else err
   in case runP text top of
        Left _ -> kParseError
        Right (m, _) -> render (lnz m term' n) True
