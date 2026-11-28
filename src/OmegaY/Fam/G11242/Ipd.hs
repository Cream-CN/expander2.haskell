-- | IPD（11242 家族）：忠实移植 C++ @notation/ipd11242.hpp@。
module OmegaY.Fam.G11242.Ipd
  ( ipdNotation
  , expandIpd
  ) where

import Control.Monad (when)
import Data.Char (isDigit, isSpace, toLower)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import OmegaY.Fam.G11242.State
import OmegaY.Notation.Types (Notation, textNotation)

cap :: Int
cap = -1

kColumns, kEdges, kLevel, kArity, kDepth, kNodes, kSlots, kMaxWork :: Int
kColumns = 4096
kEdges = 100000
kLevel = 1000000
kArity = 8192
kDepth = 160
kNodes = 60000
kSlots = 800000
kMaxWork = 1500000

data Edge = Edge
  { edP :: Int
  , edD :: Int
  , edT :: Int
  }
  deriving (Eq, Show)

data Node = Node
  { ndD :: Int
  , ndH :: Int
  , ndChildren :: [Int]
  }

data Graph = Graph
  { gCols :: [[Edge]]
  , gKey :: String
  }

data Core = Core
  { coNodes :: IntMap Node
  , coIndex :: M.Map String Int
  , coCmpCache :: M.Map String Int
  , coLowerCache :: M.Map String Int
  , coTextCache :: M.Map String String
  , coMemo :: M.Map String Int
  , coWork :: Int
  , coDepth :: Int
  , coSlots :: Int
  }

emptyCore :: Core
emptyCore =
  Core
    { coNodes = IM.singleton 0 (Node 0 0 [])
    , coIndex = M.empty
    , coCmpCache = M.empty
    , coLowerCache = M.empty
    , coTextCache = M.empty
    , coMemo = M.empty
    , coWork = 0
    , coDepth = 0
    , coSlots = 0
    }

failM :: String -> M Core a
failM msg = M $ \_ -> Left msg

tickC :: M Core ()
tickC = do
  c <- get
  if coWork c + 1 > kMaxWork
    then failM "计算预算不足"
    else put c {coWork = coWork c + 1}

enterC :: M Core ()
enterC = do
  tickC
  c <- get
  if coDepth c + 1 > kDepth
    then failM "递归深度超出预算"
    else put c {coDepth = coDepth c + 1}

leaveC :: M Core ()
leaveC = modify (\c -> c {coDepth = coDepth c - 1})

nodeAt :: Core -> Int -> Node
nodeAt c i = coNodes c IM.! i

ordCmp :: Int -> Int -> Int
ordCmp x y = (if x > y then 1 else 0) - (if x < y then 1 else 0)

-- | 节点唯一化（hash-consing）。
make :: Int -> Int -> [Int] -> M Core Int
make d h children = do
  tickC
  if length children > kArity then failM "节点元数超出预算" else pure ()
  let key = show d ++ ":" ++ show h ++ ":" ++ intercalate "," (map show children)
  c <- get
  case M.lookup key (coIndex c) of
    Just i -> pure i
    Nothing ->
      if IM.size (coNodes c) >= kNodes || coSlots c + length children + 1 > kSlots
        then failM "共享树容量超出预算"
        else do
          let i = IM.size (coNodes c)
          put
            c
              { coNodes = IM.insert i (Node d h children) (coNodes c)
              , coIndex = M.insert key i (coIndex c)
              , coSlots = coSlots c + length children + 1
              }
          pure i

seed :: Int -> Int -> Int -> Int -> M Core Int
seed d n p j
  | d == 0 = pure (if n == 0 then p else j)
  | n == 0 = pure 0
  | otherwise = make d cap (replicate (n - 1) 0)

-- | 共享树比较：返回 -1 / 0 / 1。
cmp :: Int -> Int -> Int -> M Core Int
cmp d a c0 = do
  tickC
  if a == c0
    then pure 0
    else if d == 0 || a == 0 || c0 == 0
      then pure (ordCmp a c0)
      else do
        st <- get
        let key = show d ++ ":" ++ show a ++ ":" ++ show c0
        case M.lookup key (coCmpCache st) of
          Just r -> pure r
          Nothing -> do
            enterC
            st1 <- get
            let x = nodeAt st1 a
                y = nodeAt st1 c0
            ge <- anyGe d (ndChildren x) c0
            result <-
              if ge
                then pure 1
                else do
                  le <- anyGe d (ndChildren y) a
                  if le
                    then pure (-1)
                    else do
                      base <-
                        if ndH x == ndH y
                          then pure 0
                          else if ndH x == cap
                            then pure 1
                            else if ndH y == cap
                              then pure (-1)
                              else cmp (d - 1) (ndH x) (ndH y)
                      let sizeCmp = ordCmp (length (ndChildren x)) (length (ndChildren y))
                      if base /= 0
                        then pure base
                        else if sizeCmp /= 0
                          then pure sizeCmp
                          else do
                            childCmp <- firstNonZero (zip (ndChildren x) (ndChildren y))
                            case childCmp of
                              Just r -> pure r
                              Nothing -> failM "共享树规范化内部错误"
            leaveC
            st2 <- get
            if M.size (coCmpCache st2) < 180000
              then put st2 {coCmpCache = M.insert key result (coCmpCache st2)}
              else pure ()
            pure result
 where
  anyGe _ [] _ = pure False
  anyGe dd (t : ts) target = do
    r <- cmp dd t target
    if r >= 0 then pure True else anyGe dd ts target

  firstNonZero [] = pure Nothing
  firstNonZero ((x, y) : rest) = do
    r <- cmp d x y
    if r /= 0 then pure (Just r) else firstNonZero rest

profile :: Edge -> Edge -> M Core Int
profile a c
  | edD a /= edD c = pure (ordCmp (edD a) (edD c))
  | otherwise = cmp (edD a) (edT a) (edT c)

maximumD :: Int -> [Int] -> M Core Int
maximumD d = go 0
 where
  go m [] = pure m
  go m (v : vs) = do
    r <- cmp d v m
    go (if r > 0 then v else m) vs

lower :: Int -> Int -> Int -> Int -> Int -> M Core (Maybe Int)
lower d t p j n = do
  tickC
  if d == 0
    then pure (if t == 0 then Nothing else Just (if t == j then p else t - 1))
    else if t == 0
      then pure Nothing
      else do
        st <- get
        let key = show d ++ ":" ++ show t ++ ":" ++ show p ++ ":" ++ show j ++ ":" ++ show n
        case M.lookup key (coLowerCache st) of
          Just v -> pure (Just v)
          Nothing -> do
            enterC
            st1 <- get
            let nd = nodeAt st1 t
                k = length (ndChildren nd)
            smaller <-
              if ndH nd == cap
                then Just <$> seed (d - 1) n p j
                else lower (d - 1) (ndH nd) p j n
            m0 <-
              if k == 0
                then pure 0
                else do
                  args <- (:) <$> make d (ndH nd) [] <*> pure (ndChildren nd)
                  maximumD d args
            let heads =
                  (if k > 0 then [(ndH nd, k - 1)] else [])
                    ++ maybe [] (\s -> [(s, n)]) smaller
                positions =
                  (if k >= 2 then take 1 [i | i <- [k - 2, k - 3 .. 0], ndChildren nd !! i /= 0] else [])
                    ++ (if k > 0 && ndChildren nd !! (k - 1) /= 0 then [k - 1] else [])
            lowered <- mapM (\i -> do
              low <- lower d (ndChildren nd !! i) p j n
              pure (i, fromMaybe 0 low)) positions
            m <- rounds 0 m0 heads lowered
            leaveC
            st2 <- get
            if M.size (coLowerCache st2) < 40000
              then put st2 {coLowerCache = M.insert key m (coLowerCache st2)}
              else pure ()
            pure (Just m)
 where
  rounds round m heads lowered
    | round > n = pure m
    | otherwise = do
        tickC
        headVals <- mapM (\(h, arity) -> make d h (replicate arity m)) heads
        lowVals <- mapM (\(i, low) -> do
          st <- get
          let nd = nodeAt st t
                              
              k = length (ndChildren nd)
              ch = take i (ndChildren nd) ++ [low] ++ replicate (k - i - 1) m
          make d (ndH nd) ch) lowered
        let candidates = m : headVals ++ lowVals
        m' <- maximumD d candidates
        rounds (round + 1) m' heads lowered

lowerEdge :: Edge -> Int -> Int -> M Core (Maybe Edge)
lowerEdge e j n = do
  low <- lower (edD e) (edT e) (edP e) j n
  case low of
    Just t -> pure (Just (Edge (edP e) (edD e) t))
    Nothing
      | edD e /= 0 -> do
          t <- seed (edD e - 1) n (edP e) j
          pure (Just (Edge (edP e) (edD e - 1) t))
      | otherwise -> pure Nothing

normalize :: [Edge] -> M Core [Edge]
normalize col = do
  vals <- go M.empty col
  pure (reverse (M.elems vals))
 where
  go m [] = pure m
  go m (e : es) = do
    tickC
    let p = edP e
    m' <- case M.lookup p m of
      Just old -> do
        r <- profile e old
        pure (if r > 0 then M.insert p e m else m)
      Nothing -> pure (M.insert p e m)
    go m' es

edgeOrder :: Edge -> Maybe Edge -> M Core Int
edgeOrder e Nothing = pure 1
edgeOrder e (Just best) = do
  c <- profile e best
  if c /= 0 then pure c else pure (ordCmp (edP e) (edP best))

controller :: [Edge] -> M Core (Maybe Edge)
controller = go Nothing
 where
  go best [] = pure best
  go best (e : es) = do
    r <- edgeOrder e best
    go (if r > 0 then Just e else best) es

termText :: Int -> Int -> Int -> M Core String
termText d t j = do
  tickC
  if d == 0
    then pure (if t == j then "*" else show t)
    else if t == 0
      then pure "."
      else do
        st <- get
        let key = show d ++ ":" ++ show t ++ ":" ++ show j
        case M.lookup key (coTextCache st) of
          Just s -> pure s
          Nothing -> do
            enterC
            st1 <- get
            let nd = nodeAt st1 t
            h <-
              if ndH nd == cap
                then pure "^"
                else do
                  h0 <- termText (d - 1) (ndH nd) j
                  pure (if d > 1 then "{" ++ h0 ++ "}" else h0)
            chunks <- chunkLoop (ndChildren nd)
            let result = h ++ (if null chunks then "" else "(" ++ intercalate "," chunks ++ ")")
            leaveC
            st2 <- get
            put st2 {coTextCache = M.insert key result (coTextCache st2)}
            pure result
 where
  chunkLoop [] = pure []
  chunkLoop (x : xs) = do
    let (same, rest) = span (== x) xs
    chunk <- termText d x j
    let cnt = 1 + length same
        chunk' = if cnt > 1 then chunk ++ "~" ++ show cnt else chunk
    (chunk' :) <$> chunkLoop rest

blockVisit :: Int -> Int -> (Int -> Int) -> M Core Int
blockVisit d t phi = do
  tickC
  if d == 0
    then pure (phi t)
    else if t == 0
      then pure 0
      else do
        st <- get
        let key = show d ++ ":" ++ show t
        case M.lookup key (coMemo st) of
          Just v -> pure v
          Nothing -> do
            enterC
            st1 <- get
            let nd = nodeAt st1 t
            h <- if ndH nd == cap then pure cap else blockVisit (d - 1) (ndH nd) phi
            children <- mapM (\c -> blockVisit d c phi) (ndChildren nd)
            result <- make d h children
            leaveC
            st2 <- get
            put st2 {coMemo = M.insert key result (coMemo st2)}
            pure result

makeGraph :: [[Edge]] -> M Core Graph
makeGraph columns = do
  when (length columns > kColumns) (failM "列数超出预算")
  st0 <- get
  put st0 {coTextCache = M.empty}
  go 0 0 [] [] columns
 where
  go edges chars colsOut texts [] = do
    let key0 = concat texts
        key = if null key0 then "0" else key0
    pure (Graph colsOut key)
  go edges chars colsOut texts (col : rest) = do
    col' <- normalize col
    let edges' = edges + length col'
    if edges' > kEdges then failM "边数超出预算" else pure ()
    text <- columnText (length colsOut) col'
    let chars' = chars + length text
    if chars' > 1000000 then failM "表达式文本过长" else pure ()
    go edges' chars' (colsOut ++ [col']) (texts ++ [text]) rest

  columnText colIdx col = do
    parts <- mapM (render colIdx) col
    pure ("[" ++ intercalate ";" parts ++ "]")
   where
    render colIdx e = do
      tt <- termText (edD e) (edT e) colIdx
      pure (show (edP e) ++ ":" ++ show (edD e) ++ "/" ++ tt)

seedGraph :: Int -> M Core Graph
seedGraph index = do
  when (index < 0 || index > kLevel) (failM "层号超出预算")
  makeGraph [[], [Edge 0 index 0]]

expandGraph :: Graph -> Int -> M Core Graph
expandGraph g index
  | null (gCols g) = pure g
  | index == 0 || null (last (gCols g)) = makeGraph (init (gCols g))
  | otherwise = do
      ctl <- controller (last (gCols g))
      case ctl of
        Nothing -> pure g
        Just control -> do
          let x = length (gCols g) - 1
              cut = edP control
              width = x - cut
              total = x + index * width
          when (total > kColumns) (failM "展开列数超出预算")
          let seedCols = take cut (gCols g) ++ replicate (total - cut) []
          loop 0 cut width x index total seedCols
 where
  loop block cut width x index total result
    | block > index = makeGraph result
    | otherwise = do
        st <- get
        put st {coMemo = M.empty}
        let phi k = if k < cut then k else k + block * width
            cols = gCols g
        result1 <- foldCols result [cut .. x - 1] phi cols
        if block == index
          then loop (block + 1) cut width x index total result1
          else do
            result2 <- foldLast result1 phi cut block cols
            loop (block + 1) cut width x index total result2
  foldCols result [] _ _ = pure result
  foldCols result (j : js) phi cols = do
    moved <- mapM (\e -> moveEdge phi e) (cols !! j)
    foldCols (appendAt (phi j) moved result) js phi cols
  foldLast result phi cut block cols = go result (last cols)
   where
    go acc [] = pure acc
    go acc (e : es) = do
      moved <- moveEdge phi e
      moved' <-
        if edP e == cut
          then lowerEdge moved (phi (length cols - 1)) block
          else pure (Just moved)
      case moved' of
        Nothing -> go acc es
        Just m -> go (appendAt (phi (length cols - 1)) [m] acc) es

  moveEdge phi e = do
    t <- blockVisit (edD e) (edT e) phi
    pure (Edge (phi (edP e)) (edD e) t)

appendAt :: Int -> [a] -> [[a]] -> [[a]]
appendAt i xs cols =
  take i cols ++ [cols !! i ++ xs] ++ drop (i + 1) cols

-- ---------------- 解析 ----------------

parseIpd :: String -> M Core Graph
parseIpd raw = do
  tickC
  let s = filter (not . isSpace) raw
      n = length s
  when (n > 1000000) (failM "输入文本过长")
  if isAlias s
    then do
      (v, i1) <- integerAt s 1 kLevel
      g0 <- seedGraph v
      expansionLoop s n True i1 0 g0
    else if not (null s) && isDigit (head s)
      then do
        (v, i1) <- integerAt s 0 kColumns
        g0 <- makeGraph (replicate v [])
        finish s n i1 g0
      else do
        (cols, i1) <- parseColumns s n 0 []
        when (null cols) (failM "请输入 A0、A1[3] 或完整列列表")
        g0 <- makeGraph cols
        finish s n i1 g0
 where
  isAlias s0 =
    not (null s0)
      && toLower (head s0) `elem` "at"
      && length s0 > 1
      && isDigit (s0 !! 1)

  finish s0 n i g0 =
    if i == n then pure g0 else failM "表达式后有未识别内容"

  expansionLoop s0 n alias i steps g0
    | alias && i < n && s0 !! i == '[' =
        if steps + 1 > 256
          then failM "展开路径过长"
          else do
            (v, i1) <- integerAt s0 (i + 1) kLevel
            i2 <- takeAt ']' s0 i1
            g1 <- expandGraph g0 v
            expansionLoop s0 n alias i2 (steps + 1) g1
    | otherwise = finish s0 n i g0

  parseColumns s0 n i cols
    | i < n && s0 !! i == '[' = do
        when (length cols >= kColumns) (failM "列数超出预算")
        let j = length cols
        if i + 1 < n && s0 !! (i + 1) /= ']'
          then do
            (edges, i1) <- parseEdges s0 n (i + 1) j []
            i2 <- takeAt ']' s0 i1
            parseColumns s0 n i2 (cols ++ [edges])
          else do
            i2 <- takeAt ']' s0 (i + 1)
            parseColumns s0 n i2 (cols ++ [[]])
    | otherwise = pure (cols, i)

  parseEdges s0 n i j acc
    | i < n && s0 !! i /= ']' = do
        (p, i1) <- integerAt s0 i kColumns
        when (p >= j) (failM "父列必须严格早于本列")
        i2 <- takeAt ':' s0 i1
        (d, i3) <- integerAt s0 i2 kLevel
        i4 <- takeAt '/' s0 i3
        (t, i5) <- termAt s0 n i4 d p j
        let i6 = if i5 < n && s0 !! i5 == ';' then i5 + 1 else i5
        parseEdges s0 n i6 j (acc ++ [Edge p d t])
    | otherwise = pure (acc, i)

-- | 从第 i 位读十进制整数，不超过 max。
integerAt :: String -> Int -> Int -> M Core (Int, Int)
integerAt s i maxV = do
  let (ds, _) = span isDigit (drop i s)
  when (null ds) (failM "需要非负整数")
  when (length ds > 16) (failM "整数过长")
  let v = foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0 ds
      i' = i + length ds
  when (v > maxV) (failM "整数范围超出预算")
  pure (v, i')

takeAt :: Char -> String -> Int -> M Core Int
takeAt c s i =
  if i < length s && s !! i == c
    then pure (i + 1)
    else failM "格式错误"

-- | 项解析：@d@ 层、父列 @p@、列号 @j@。
termAt :: String -> Int -> Int -> Int -> Int -> Int -> M Core (Int, Int)
termAt s n i d p j = do
  enterC
  (result, i') <-
    if d == 0
      then
        if i < n && s !! i == '*'
          then pure (j, i + 1)
          else do
            (q, i1) <- integerAt s i kColumns
            if not (q <= p || q == j) then failM "引用必须是父列以前的 ROOT 或当前 SELF" else pure (q, i1)
      else
        if i < n && s !! i == '.'
          then pure (0, i + 1)
          else do
            (h, i1) <-
              if i < n && s !! i == '^'
                then pure (cap, i + 1)
                else if d > 1
                  then do
                    i2 <- takeAt '{' s i
                    (h0, i3) <- termAt s n i2 (d - 1) p j
                    i4 <- takeAt '}' s i3
                    pure (h0, i4)
                  else termAt s n i 0 p j
            (children, i2) <-
              if i1 < n && s !! i1 == '('
                then do
                  (ch, i3) <- parseChildren s n (i1 + 1) d p j []
                  i4 <- takeAt ')' s i3
                  pure (ch, i4)
                else pure ([], i1)
            r <- make d h children
            pure (r, i2)
  leaveC
  pure (result, i')

parseChildren :: String -> Int -> Int -> Int -> Int -> Int -> [Int] -> M Core ([Int], Int)
parseChildren s n i d p j acc
  | i < n && s !! i == ')' = pure (acc, i)
  | otherwise = do
      (child, i1) <- termAt s n i d p j
      (repeatN, i2) <-
        if i1 < n && s !! i1 == '~'
          then do
            (r, i3) <- integerAt s (i1 + 1) kArity
            if r == 0 then failM "重复次数不能为零" else pure (r, i3)
          else pure (1, i1)
      when (length acc + repeatN > kArity) (failM "节点元数超出预算")
      mapM_ (\_ -> tickC) [1 .. repeatN]
      let acc' = acc ++ replicate repeatN child
      if i2 < n && s !! i2 == ','
        then parseChildren s n (i2 + 1) d p j acc'
        else pure (acc', i2)

expandIpd :: String -> Int -> String
expandIpd text term =
  case runM (parseIpd text >>= \g -> expandGraph g (if term < 0 then 0 else term)) emptyCore of
    Left e -> e
    Right g -> gKey g

ipdNotation :: Notation
ipdNotation = textNotation "ipd" "IPD" ["11242", "文本"] expandIpd ""
