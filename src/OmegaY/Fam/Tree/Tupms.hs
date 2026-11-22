-- | TUPMS（超限 UPMS）。复用 TBM 全部原语，仅重写提升阈值（延迟堆 UPMS 判定）
--   与 FS 包装。移植自 @Header/notation/tupms.hpp@。
module OmegaY.Fam.Tree.Tupms
  ( expandText
  ) where

import qualified OmegaY.Fam.Tree.Tbm as Tbm
import OmegaY.Fam.Tree.Tbm
  ( Expr (..)
  , Entry (..)
  , Column
  , cmpInt
  , exprCmp
  , columnCmp
  , verticalCmp
  , indexAfter
  , columnVerticals
  , parents
  , toVertical
  , columnSub
  , columnAdd
  , columnTruncate
  , ascensionThreshold
  , copyColumn
  , isOne
  , oneExpr
  , display
  , parseExprText
  , kParseError
  )

constColumn :: Int -> [Expr] -> Column
constColumn value vertical = map (\seg -> Entry value seg) vertical

-- 最小堆：按 (delayed_pos 竖直序, 列号) 排序。列数很少，线性取最小即可。
heapPop :: [[Expr]] -> [Int] -> (Int, [Int])
heapPop delayedPos heap =
  let k = foldl1
            (\a b ->
               if cmpItem a b <= 0 then a else b)
            heap
  in (k, filter (/= k) heap)
 where
  cmpItem a b =
    case verticalCmp (delayedPos !! a) (delayedPos !! b) of
      0 -> cmpInt a b
      c -> c

data St = St
  { stA :: [[Expr]]
  , stSet :: [Bool]
  , stDP :: [[Expr]]       -- delayed_pos
  , stVr :: [Bool]         -- delayed_vr
  , stPresent :: [Bool]
  , stRootCache :: [[[Expr]]]
  , stRootSet :: [Bool]
  }

tupmsAscensionThreshold :: Expr -> [[[Expr]]] -> [[(Int, Int)]] -> Int -> [Expr] -> [[Expr]]
tupmsAscensionThreshold m@(Expr mcols) v p r b = stA (loop heap0 st0)
 where
  ncols = length mcols
  right = ncols - 1

  -- 初始化
  initA = [ if k < r then []
           else if k == r then b
           else []
          | k <- [0 .. ncols - 1] ]
  initSet = [ k <= r | k <- [0 .. ncols - 1] ]
  initDP = [ [ ] | _ <- [0 .. ncols - 1] ]
  initVr = [ False | _ <- [0 .. ncols - 1] ]
  initPresent = [ k > r | k <- [0 .. ncols - 1] ]
  initRootCache = [ [] | _ <- [0 .. ncols - 1] ]
  initRootSet = [ False | _ <- [0 .. ncols - 1] ]
  st0 = St initA initSet initDP initVr initPresent initRootCache initRootSet
  heap0 = [r + 1 .. ncols - 1]

  loop heap st =
    case heap of
      [] -> st
      _ ->
        let (i, heap1) = heapPop (stDP st) heap
            prevPos = stDP st !! i
            vr = stVr st !! i
            st1 = st { stPresent = update i False (stPresent st) }
            (_, st2) = process i prevPos vr st1 heap1
            heap2 = if stPresent st2 !! i then heap1 ++ [i] else heap1
        in loop heap2 st2

  process i prevPos vr st heap
    | not vr =
      let j0 = indexAfter (v !! i) prevPos
          st' = scanJ j0 st
          st'' =
            if not (stSet st' !! i) && not (stPresent st' !! i)
              then st' { stA = update i (last (v !! i)) (stA st')
                       , stSet = update i True (stSet st') }
              else st'
      in (heap, st'')
   where
    scanJ j st
      | j >= length (v !! i) = st
      | otherwise =
        let pos = if j /= 0 then v !! i !! (j - 1) else []
            colP = fst (p !! i !! j)
        in if colP < r
             then st { stA = update i pos (stA st)
                     , stSet = update i True (stSet st) }
             else if colP == r
               then st { stDP = update i pos (stDP st)
                       , stVr = update i True (stVr st)
                       , stPresent = update i True (stPresent st) }
               else
                 let pDelayed = not (stSet st !! colP)
                     ap = if stSet st !! colP
                            then stA st !! colP
                            else stDP st !! colP
                 in if verticalCmp pos ap >= 0
                      then if pDelayed
                             then st { stDP = update i pos (stDP st)
                                    , stVr = update i False (stVr st)
                                    , stPresent = update i True (stPresent st) }
                             else st { stA = update i pos (stA st)
                                    , stSet = update i True (stSet st) }
                      else
                        let newPos = v !! i !! j
                        in if verticalCmp newPos ap >= 0
                             then if pDelayed
                                    then st { stDP = update i ap (stDP st)
                                           , stVr = update i False (stVr st)
                                           , stPresent = update i True (stPresent st) }
                                    else st { stA = update i ap (stA st)
                                            , stSet = update i True (stSet st) }
                             else scanJ (j + 1) st
  process i prevPos True st heap =
    -- vr == True：UPMS 提升判定
    let upgrading = decideUpgrade i prevPos st
        newA = if upgrading then b else prevPos
        st1 = st { stA = update i newA (stA st)
                 , stSet = update i True (stSet st) }
    in (heap, st1)

  decideUpgrade i prevPos st
    | null prevPos = True
    | otherwise =
      let ji0 = indexAfter (v !! i) prevPos
          thresholdColumn =
            columnAdd
              (columnTruncate (mcols !! i) prevPos)
              (constColumn 1 prevPos)
          e1 = Entry (eValue (mcols !! i !! ji0) + 1) oneExpr
          offset2 = if null (cols (eHeight (mcols !! i !! ji0))) then 1 else 0
          e2 = Entry (eValue (mcols !! i !! (ji0 + offset2))) oneExpr
          tc = thresholdColumn ++ [e1, e2]
          xEnd = findXEnd (i + 1) tc
      in if xEnd /= ncols
           then decideY i prevPos st xEnd
           else True

  findXEnd xEnd tc
    | xEnd < ncols && columnCmp (mcols !! xEnd) tc >= 0 = findXEnd (xEnd + 1) tc
    | otherwise = xEnd

  decideY i prevPos st xEnd =
    let jr0 = indexAfter (v !! r) prevPos
        valueR = if jr0 < length (mcols !! r)
                   then eValue (mcols !! r !! jr0)
                   else 0
        jt0 = indexAfter (v !! right) prevPos
        valueT = eValue (mcols !! right !! jt0)
        yStart0 = right
        yStart = foldl
                   (\ys _ -> fst (p !! ys !! indexAfter (v !! ys) prevPos))
                   yStart0
                   [1 .. valueT - valueR - 1]
        xInit = columnSub (columnTruncate (mcols !! i) prevPos) (mcols !! r)
        yInit = columnSub (columnTruncate (mcols !! yStart) prevPos) (mcols !! r)
        mm = max (eValue (head (mcols !! i))) (eValue (head (mcols !! yStart)))
        xInit' = map (\e -> e { eValue = eValue e + mm }) xInit
        yInit' = map (\e -> e { eValue = eValue e + mm }) yInit
        rootCache = if not (stRootSet st !! yStart)
                      then update
                             yStart
                             (ascensionThreshold v p yStart b)
                             (stRootCache st)
                      else stRootCache st
        rootSet = update yStart True (stRootSet st)
        stR = st { stRootCache = rootCache, stRootSet = rootSet }
    in compareLoop i prevPos stR yStart xEnd xInit' yInit' 0

  compareLoop i prevPos st yStart xEnd xInit yInit k
    | yStart + k > right = True
    | i + k > xEnd = False
    | otherwise =
      let iK = i + k
          yK = yStart + k
          xThreshold =
            if stSet st !! iK
              then stA st !! iK
              else prevPos
          xThreshold' =
            if not (stSet st !! iK) || verticalCmp xThreshold prevPos > 0
              then prevPos
              else xThreshold
          xTransformed =
            columnAdd (mcols !! iK) (columnTruncate yInit xThreshold')
          yThreshold = stRootCache st !! yK !! (yStart + k)
          yThreshold' =
            if verticalCmp yThreshold prevPos > 0 then prevPos else yThreshold
          yTransformed =
            columnAdd (mcols !! yK) (columnTruncate xInit yThreshold')
          c = columnCmp xTransformed yTransformed
      in if c /= 0
           then c > 0
           else compareLoop i prevPos st yStart xEnd xInit yInit (k + 1)

update :: Int -> a -> [a] -> [a]
update k x xs = take k xs ++ [x] ++ drop (k + 1) xs

-- 展开

tuExpandSuccessor :: Expr -> Int -> Expr
tuExpandSuccessor m@(Expr mcols) index = Expr resultCols
 where
  v = map columnVerticals mcols
  p = parents m v
  n = length mcols - 1
  lastCol = mcols !! n
  r = fst (p !! n !! (length lastCol - 1))
  before = take n mcols
  b = if length lastCol > 1
        then v !! n !! (length lastCol - 2)
        else []
  offset = columnSub lastCol (mcols !! r)
  a = tupmsAscensionThreshold m v p r b
  added =
    [ copyColumn (mcols !! i) offset (a !! i) w
    | w <- [1 .. index]
    , i <- [r .. n - 1] ]
  resultCols = before ++ added

tuExpandLimit :: Expr -> Int -> Expr
tuExpandLimit m@(Expr mcols) index = Expr (take right mcols ++ [newEntries])
 where
  right = length mcols - 1
  col = mcols !! right
  lastIdx = length col - 1
  vv = eValue (col !! lastIdx)
  newH = tupmsFs (eHeight (col !! lastIdx)) index
  segs = toVertical newH
  newEntries = take lastIdx col ++ map (\seg -> Entry vv seg) segs

tuExpand :: Expr -> Int -> Expr
tuExpand m@(Expr mcols) index = case mcols of
  [] -> m
  _ ->
    let n = length mcols - 1
        lastCol = mcols !! n
    in if null lastCol
         then Expr (take n mcols)
         else
           let lastHeight = eHeight (lastCol !! (length lastCol - 1))
           in if isOne lastHeight
                then tuExpandSuccessor m index
                else tuExpandLimit m index

tupmsFs :: Expr -> Int -> Expr
tupmsFs m index
  | otherwise = tuExpand m index

expandText :: String -> Int -> String
expandText text term =
  case parseExprText text of
    Just e -> display (tupmsFs e term)
    _ -> kParseError
