-- | TON 族共享核心：忠实移植 C++ @Header/notation/tonshare.hpp@。
module OmegaY.Fam.Ton.TonCore
  ( TonNode (..)
  , TonV
  , kInf
  , zeroLeaf
  , negOneLeaf
  , isLeaf
  , isInf
  , leftChild
  , rightChild
  , flatten
  , flattenKey
  , flattenSize
  , fmtVal
  , maxValue
  , structuralEqual
  , lexCompare
  , compareNoraise
  , compareNoraiseVec
  , raise
  , compareRaise
  , rConfig
  , displayNoraise
  , displayMain
  , parseImpl
  , regress
  , regressRepeated
  , markMain
  , markMc
  , markFs
  , extractPath
  , collectSubtermIndex
  , collectSmallindex
  , collectSmallpart
  , getA2
  , getN
  , stdMemoized
  , TonState (..)
  , GenState (..)
  , CacheEntry (..)
  , initialTonState
  , tonExpandText
  , StdFn
  , TonConfig (..)
  ) where

import Control.Monad.Trans.State (State, get, put, evalState)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Debug.Trace (trace)

-- 基本类型

type TonV = Double

kInf :: TonV
kInf = 1/0

data TonNode
  = TLeaf !TonV
  | TNode !TonNode !TonNode
  deriving (Eq, Ord, Show)

zeroLeaf :: TonNode
zeroLeaf = TLeaf 0

negOneLeaf :: TonNode
negOneLeaf = TLeaf (-1)

isLeaf :: TonNode -> Bool
isLeaf (TLeaf _) = True
isLeaf _ = False

isInf :: TonNode -> Bool
isInf (TLeaf v) = v == kInf
isInf _ = False

leftChild :: TonNode -> TonNode
leftChild (TNode l _) = l
leftChild t = t

rightChild :: TonNode -> TonNode
rightChild (TNode _ r) = r
rightChild t = t

-- 扁平化

flatten :: TonNode -> [TonV]
flatten (TLeaf v) = [v]
flatten (TNode l r) = flatten l ++ flatten r ++ [-2]

isFiniteV :: TonV -> Bool
isFiniteV v = not (isNaN v || isInfinite v)

fmtVal :: TonV -> String
fmtVal v
  | v == kInf = "Infinity"
  | isFiniteV v && v == fromIntegral (round v :: Integer) = show (round v :: Integer)
  | otherwise = show v

flattenKey :: TonNode -> String
flattenKey = intercalateComma . map fmtVal . flatten

flattenSize :: TonNode -> Int
flattenSize = length . flatten

intercalateComma :: [String] -> String
intercalateComma [] = ""
intercalateComma [x] = x
intercalateComma (x:xs) = x ++ "," ++ intercalateComma xs

maxValue :: TonNode -> TonV
maxValue t = maximum (0 : flatten t)

structuralEqual :: TonNode -> TonNode -> Bool
structuralEqual a b = flattenKey a == flattenKey b

-- 比较

lexCompare :: [TonV] -> [TonV] -> Ordering
lexCompare = go
  where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go (a:as) (b:bs) = case compare a b of
      EQ -> go as bs
      o -> o

compareNoraise :: TonNode -> TonNode -> Ordering
compareNoraise x y = lexCompare (flatten x) (flatten y)

compareNoraiseVec :: TonNode -> [TonV] -> Ordering
compareNoraiseVec t v = lexCompare (flatten t) v

raise :: TonNode -> TonV -> TonNode
raise (TLeaf v) sys
  | v >= 0 && v < sys = TNode negOneLeaf (raise (TLeaf (v+1)) sys)
  | otherwise = TLeaf v
raise (TNode l r) sys = TNode (raise l sys) (raise r sys)

compareRaise :: TonNode -> TonNode -> Ordering
compareRaise x y =
  let fx = flatten x
      fy = flatten y
      sysx = maximum (0 : fx)
      sysy = maximum (0 : fy)
  in if sysx < kInf && sysy < kInf && (sysx > 0 || sysy > 0)
       then let s = max sysx sysy
            in lexCompare (flatten (raise x s)) (flatten (raise y s))
       else lexCompare fx fy

rConfig :: TonNode -> TonNode -> TonNode
rConfig a b
  | isLeaf a = a
  | otherwise =
      case compareRaise a b of
        GT -> if compareRaise (leftChild a) b == GT
                then TNode (rConfig (leftChild a) b) (rConfig (rightChild a) b)
                else TNode (TLeaf (-0.5)) (rConfig (rightChild a) b)
        LT -> a
        EQ -> TLeaf (-0.5)

-- 显示

displayNoraise :: TonNode -> String
displayNoraise (TLeaf v)
  | v < 0 = "0"
  | otherwise = "Ω"
displayNoraise (TNode l r) = displayNoraise l ++ displayNoraise r ++ "C"

displayMain :: TonNode -> String
displayMain (TLeaf v)
  | v < 0 = "0"
  | otherwise = "Ω<" ++ fmtVal v ++ ">"
displayMain (TNode l r) = displayMain l ++ displayMain r ++ "C"

-- 解析

parseImpl :: String -> Bool -> Maybe TonNode
parseImpl s0 mainStyle =
  let s = trim s0
  in if null s then Nothing else go s []
  where
    trim = dropWhile isWS . reverse . dropWhile isWS . reverse
    isWS c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

    go [] st = case st of
      [t] -> Just t
      _ -> Nothing
    go s st =
      case s of
        ('0':rest) -> go rest (TLeaf (-1) : st)
        ('\x3A9':rest) ->
          if mainStyle
            then case rest of
              ('<':rest1) ->
                let (digits, rest2) = span (\c -> c >= '0' && c <= '9') rest1
                in if null digits
                     then Nothing
                     else case rest2 of
                       ('>':rest3) ->
                         let v = read digits :: TonV
                         in go rest3 (TLeaf v : st)
                       _ -> Nothing
              _ -> Nothing
            else go rest (TLeaf 0 : st)
        ('C':rest) -> case st of
          (b:a:restSt) -> go rest (TNode a b : restSt)
          _ -> Nothing
        _ -> Nothing

-- regress

regress :: TonNode -> TonNode
regress (TLeaf v) = TLeaf v
regress n@(TNode l r) =
  case (l, r) of
    (l', TLeaf rv) | l' == negOneLeaf && rv > 0 -> TLeaf (rv - 1)
    _ -> TNode (regress l) (regress r)

regressRepeated :: TonNode -> TonNode
regressRepeated x =
  let cur = regress x
  in if flattenKey x == flattenKey cur then cur else regressRepeated cur

-- mark

markMain :: TonV -> TonNode
markMain sys =
  let base = TNode (TNode negOneLeaf (TLeaf sys)) (TLeaf sys)
      foldN :: TonV -> TonNode -> TonNode
      foldN i acc | i > 0 = foldN (i-1) (TNode negOneLeaf acc)
                  | otherwise = acc
  in foldN (sys - 1) base

markMc :: TonV -> TonNode
markMc sys =
  let loop1 acc i | i > 0 = loop1 (TNode (TNode negOneLeaf (TLeaf sys)) acc) (i-1)
                  | otherwise = acc
      loop2 acc i | i > 0 = loop2 (TNode negOneLeaf acc) (i-1)
                  | otherwise = acc
  in loop2 (loop1 (TLeaf sys) sys) (sys - 1)

markFs :: TonV -> Int -> TonNode
markFs sys n =
  let base = TLeaf (sys - 1)
      loopN acc 0 = acc
      loopN acc k = loopN (TNode (TLeaf (sys-1)) acc) (k-1)
      loopS acc i | i > 0 = loopS (TNode negOneLeaf acc) (i-1)
                  | otherwise = acc
  in loopS (loopN base n) (sys - 1)

-- 索引机器

extractPath :: TonNode -> [Int] -> TonNode
extractPath t [] = t
extractPath (TNode l r) (0:xs) = extractPath l xs
extractPath (TNode l r) (1:xs) = extractPath r xs
extractPath _ _ = zeroLeaf

collectSubtermIndex :: TonNode -> [[Int]]
collectSubtermIndex a = go [] a
  where
    go path node = case node of
      TLeaf _ -> [reverse path]
      TNode _ _ -> reverse path : go (0:path) (leftChild node) ++ go (1:path) (rightChild node)

collectSmallindex :: TonNode -> [[Int]]
collectSmallindex a = go [] a
  where
    go path node = case node of
      TLeaf v
        | v == 0 -> []
        | v < 0 -> [reverse path]
        | otherwise -> []
      TNode _ _ ->
        if compareNoraise node zeroLeaf == LT
          then [reverse path]
          else go (0:path) (leftChild node) ++ go (1:path) (rightChild node)

collectSmallpart :: TonNode -> [TonNode]
collectSmallpart a = go a
  where
    go node = case node of
      TLeaf v
        | v == 0 -> []
        | v < 0 -> [node]
        | otherwise -> []
      TNode _ _ ->
        if compareNoraise node zeroLeaf == LT
          then [node]
          else go (leftChild node) ++ go (rightChild node)

updatePath :: TonNode -> [Int] -> (TonNode -> TonNode) -> TonNode
updatePath t [] f = f t
updatePath (TNode l r) (0:xs) f = TNode (updatePath l xs f) r
updatePath (TNode l r) (1:xs) f = TNode l (updatePath r xs f)
updatePath t _ _ = t

scan :: TonNode -> TonNode
scan x = case x of
  TNode l r ->
    let l' = case l of
               TNode _ lr -> if compareNoraise r lr == GT then leftChild l else l
               _ -> l
    in TNode (scan l') (scan r)
  _ -> x

getA2 :: TonNode -> [Int] -> [TonV]
getA2 term index =
  let (a1, a1index) = step0 term index []
      a = if null a1index
            then zeroLeaf
            else setLeafAt a1 (reverse a1index)
      a' = scan a
      f1 = flatten term
      f2 = flatten a'
  in commonPrefix f1 f2
  where
    step0 a [] kept = (a, reverse kept)
    -- C++：遇到 0 时，若位于路径开头则整棵树左移；否则取前缀处的子节点 sub，
    -- 令 child = sub 的对应孩子，再把**该孩子**替换成 child->l（不是把 sub 换成 sub->l）。
    step0 a (0:rest) kept =
      case reverse kept of
        [] -> step0 (leftChild a) rest kept
        path ->
          let a' = updatePath a path $ \sub ->
                let child = if last path == 0 then leftChild sub else rightChild sub
                 in if last path == 0
                      then TNode (leftChild child) (rightChild sub)
                      else TNode (leftChild sub) (leftChild child)
           in step0 a' rest kept
    step0 a (i:rest) kept = step0 a rest (i:kept)

    setLeafAt a idx =
      case lastMay idx of
        Nothing -> a
        Just 0 -> updatePath a (init idx) (\node -> TNode zeroLeaf (rightChild node))
        Just _ -> updatePath a (init idx) (\node -> TNode (leftChild node) zeroLeaf)

    lastMay [] = Nothing
    lastMay xs = Just (last xs)

    commonPrefix [] _ = []
    commonPrefix _ [] = []
    commonPrefix (x:xs) (y:ys) | x == y = x : commonPrefix xs ys
                               | otherwise = []

-- C++ get_n：从**尾部**数 "-1, -2" 交替的组数，并就地弹出。
getN :: [TonV] -> (Int, [TonV])
getN a2 =
  let a2' = dropWhileEnd (== (-2)) a2
  in case reverse a2' of
       [] -> (0, [])
       (back:restRev)
         | back == (-1) -> countPairs (reverse restRev)
         | otherwise -> (0, a2)
  where
    countPairs xs =
      case reverse xs of
        (b : a : rest)
          | b == (-2) && a == (-1) ->
              let (n, r) = countPairs (reverse rest)
              in (n + 1, r)
        _ -> (0, xs)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x r -> if null r && p x then [] else x:r) []

-- std_memoized

stdMemoized :: Map String Bool -> TonNode -> Bool -> (Map String Bool, Bool)
stdMemoized memo a compute
  | isLeaf a = (memo, True)
  | otherwise =
      let key = flattenKey a
      in case M.lookup key memo of
           Just _ -> (memo, True)
           Nothing ->
             if compute
               then (M.insert key True memo, True)
               else (memo, False)

-- 状态

type StdFn = TonNode -> TonV -> State TonState Bool

data GenState = GenState
  { gsBeta :: !TonNode
  , gsFlag :: !Bool
  , gsLen :: !Int
  }

data CacheEntry = CacheEntry
  { ceState :: !GenState
  , ceVals :: [TonNode]
  }

data TonState = TonState
  { tsMemo :: !(Map String Bool)
  , tsCache :: !(Map String CacheEntry)
  -- 当前正在生成的 entry（cache.fs 一次只生成一个）
  , tsKey :: !String
  , tsGen :: !GenState
  , tsVals :: [TonNode]
  }

initialTonState :: TonState
initialTonState = TonState M.empty M.empty "" (GenState zeroLeaf True 0) []

-- ton_step

tonStep :: TonV -> TonNode -> (TonNode, Bool)
tonStep sys beta = case beta of
  TLeaf v ->
    if v >= 0 then (TLeaf (-1), True) else (TLeaf v, False)
  TNode l r1 -> case r1 of
    TLeaf rv
      | rv == (-1) -> tonStep sys l
      | rv >= 0 -> (TNode l (TLeaf (-1)), True)
    TNode r1l r1r -> case r1r of
      TLeaf rv2
        | rv2 == (-1) -> (TNode (TNode l r1l) (TLeaf sys), True)
        | rv2 >= 0 -> (TNode l (TNode r1l (TLeaf (-1))), True)
      _ -> (walkSpine sys (TNode l r1), True)

walkSpine :: TonV -> TonNode -> TonNode
walkSpine sys beta@(TNode _ r1@(TNode _ r2@(TNode _ _))) =
  let (pathRev, c1rNeg) = loop [1,1] r2
      pathC1 = reverse pathRev
      pathC3 = init pathC1
      c1 = extractPath beta pathC1
      c3r = extractPath beta (pathC3 ++ [1])
  in if c1rNeg
       then let newC3r = TNode (TNode (leftChild c3r) (leftChild c1)) (TLeaf sys)
            in updatePath beta pathC3 (\c3 -> TNode (leftChild c3) newC3r)
       else updatePath beta pathC1 (\c1 -> TNode (leftChild c1) (TLeaf (-1)))
  where
    loop path c1@(TNode _ c1r@(TNode _ _)) = loop (1:path) c1r
    loop path c1 = (path, isLeafNegOne (rightChild c1))
    isLeafNegOne (TLeaf v) = v == (-1)
    isLeafNegOne _ = False
walkSpine _ beta = beta

-- ton_gen_next

tonGenNext :: TonV -> Int -> StdFn -> Bool -> State TonState TonNode
tonGenNext sys n stdFn mainStyle = go
  where
    go = do
      st <- get
      let gs = tsGen st
      if gsFlag gs
        then let (newBeta, ok) = tonStep sys (gsBeta gs)
             in if not ok
                  then do
                    put st { tsGen = gs { gsBeta = newBeta, gsFlag = False } }
                    return newBeta
                  else do
                    put st { tsGen = gs { gsBeta = newBeta, gsFlag = True } }
                    expandLoop
        else do
          -- C++: st.flag = true 无条件设置
          put st { tsGen = gs { gsFlag = True } }
          expandLoop

    expandLoop = do
      st <- get
      let gs = tsGen st
      if flattenSize (gsBeta gs) < gsLen gs + n * 2
        then do
          r <- stdFn (gsBeta gs) sys
          if not r
            then go
            else do
              let newBeta = expandBeta sys (gsBeta gs)
              put st { tsGen = gs { gsBeta = newBeta } }
              expandLoop
        else do
          r <- stdFn (gsBeta gs) sys
          if r
            then do
              let out = if mainStyle then regressRepeated (gsBeta gs) else gsBeta gs
              put st { tsGen = gs { gsFlag = False } }
              return out
            else go

expandBeta :: TonV -> TonNode -> TonNode
expandBeta sys beta = case beta of
  TLeaf v -> TNode (TLeaf v) (TLeaf sys)
  TNode _ _ ->
    let (pathRev, leafv) = loop [] beta
        pathC1 = reverse pathRev
    in updatePath beta pathC1 (\c1 -> TNode (leftChild c1) (TNode leafv (TLeaf sys)))
  where
    loop path c1 = case rightChild c1 of
      TNode _ _ -> loop (1:path) (rightChild c1)
      leafv -> (path, leafv)

-- cache.fs

cacheFs :: TonNode -> TonV -> Int -> Bool -> StdFn -> State TonState TonNode
cacheFs term sys n mainStyle stdFn = do
  st <- get
  let key = flattenKey term
  case M.lookup key (tsCache st) of
    Just entry -> do
      let gs = ceState entry
      put st { tsKey = key, tsGen = gs, tsVals = ceVals entry }
    Nothing -> do
      let gs = GenState term True (flattenSize term)
      put st { tsKey = key, tsGen = gs, tsVals = [] }
  loop
  where
    loop = do
      st <- get
      let vals = tsVals st
      if length vals > n
        then do
          -- 保存回 cache
          let key = tsKey st
              entry = CacheEntry (tsGen st) vals
          put st { tsCache = M.insert key entry (tsCache st) }
          return (vals !! n)
        else do
          let i = length vals
          newVal <- tonGenNext sys i stdFn mainStyle
          st2 <- get
          put st2 { tsVals = tsVals st2 ++ [newVal] }
          loop

-- 配置与入口

data TonConfig = TonConfig
  { tcMainStyle :: !Bool
  , tcIbpLimit :: !Bool
  , tcMark :: !(Maybe (TonV -> TonNode))
  , tcStdFn :: !StdFn
  }

makeLimit :: Bool -> TonNode
makeLimit ibpStyle
  | ibpStyle =
      let inner2 = TNode (TLeaf 0) (TLeaf (-1))
          inner1 = TNode (TNode (TLeaf 0) inner2) (TLeaf 0)
      in TNode negOneLeaf inner1
  | otherwise = TNode negOneLeaf (TLeaf 0)

tonExpandText :: TonConfig -> String -> Int -> String
tonExpandText cfg text term0 =
  let term = if term0 < 0 then 0 else term0
  in case parseImpl text (tcMainStyle cfg) of
       Nothing -> "解析失败，请确认输入是 TON 文本（0 / Ω / Ω<n> / C 后缀），且不是全角字符"
       Just t -> evalState (expand cfg t term) initialTonState

expand :: TonConfig -> TonNode -> Int -> State TonState String
expand cfg t n
  | tcMainStyle cfg = do
      let sys = case t of
                  TLeaf v -> v
                  _ -> maxValue t
      if sys == kInf
        then do
          let n' = fromIntegral n :: TonV
              res = foldl (\acc _ -> TNode negOneLeaf acc)
                          (TNode (TLeaf n') (TLeaf n')) [1..n]
          return (displayMain res)
        else do
          let raised = raise t sys
          case tcMark cfg of
            Just markFn | sys >= 1 && flattenKey raised == flattenKey (markFn sys) ->
              return (displayMain (markFs sys n))
            _ -> do
              out <- cacheFs raised sys n True (tcStdFn cfg)
              return (displayMain out)
  | otherwise = do
      let t' = if isInf t then makeLimit (tcIbpLimit cfg) else t
      out <- cacheFs t' 0 n False (tcStdFn cfg)
      return (displayNoraise out)
