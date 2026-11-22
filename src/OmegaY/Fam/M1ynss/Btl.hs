-- | btlynss（BTL）：mark 字段 + type 分派（star/mark/higher/lower）。
module OmegaY.Fam.M1ynss.Btl
  ( expandBtl
  ) where

import Control.Exception (try, SomeException, evaluate)
import System.IO.Unsafe (unsafePerformIO)
import OmegaY.Fam.M1ynss.PP

data BNode = BNode
  { bCols :: [BColumn]
  } deriving (Eq, Show)

data BColumn = BColumn
  { cLower :: [Int]
  , cMark :: Int
  , cHigher :: [BNode]
  } deriving (Eq, Show)

makeNode :: BNode
makeNode = BNode []

-- 显示

topDisplay :: BNode -> Int -> String
topDisplay e u
  | null (bCols e) = show u
  | otherwise = show u ++ "^" ++ display e

columnDisplay :: BColumn -> String
columnDisplay c
  | not (null (cHigher c)) =
      "(" ++ commas (map show (cLower c)) ++ ";" ++ commas (map (\h -> topDisplay h (cMark c)) (cHigher c)) ++ ")"
  | otherwise =
      let j = lastPositive (cLower c)
       in "(" ++ commas (map show (take (j + 1) (cLower c))) ++ ")"
  where
    commas xs = foldr (\x a -> x ++ (if null a then "" else "," ++ a)) "" xs
    lastPositive xs = go (length xs - 1)
      where
        go i
          | i < 0 = (-1)
          | xs !! i > 0 = i
          | otherwise = go (i - 1)

display :: BNode -> String
display e = concatMap columnDisplay (bCols e)

-- 比较

numCmp :: Int -> Int -> Int
numCmp a b = if a == b then 0 else if a < b then (-1) else 1

compareNode :: BNode -> BNode -> Int
compareNode a b = go 0
  where
    m = min (length (bCols a)) (length (bCols b))
    go i
      | i >= m = numCmp (length (bCols a)) (length (bCols b))
      | otherwise =
          let c = colCmp (bCols a !! i) (bCols b !! i)
           in if c /= 0 then c else go (i + 1)

higherCmp :: [BNode] -> [BNode] -> Int
higherCmp a b = go 0
  where
    m = min (length a) (length b)
    go i
      | i >= m = numCmp (length a) (length b)
      | otherwise =
          let c = compareNode (a !! i) (b !! i)
           in if c /= 0 then c else go (i + 1)

colCmp :: BColumn -> BColumn -> Int
colCmp a b = goL 0
  where
    m = min (length (cLower a)) (length (cLower b))
    goL i
      | i >= m =
          let c = numCmp (length (cLower a)) (length (cLower b))
           in if c /= 0 then c
                else
                  let cm = numCmp (cMark a) (cMark b)
                   in if cm /= 0 then cm else higherCmp (cHigher a) (cHigher b)
      | otherwise =
          let c = numCmp (cLower a !! i) (cLower b !! i)
           in if c /= 0 then c else goL (i + 1)

removeBase :: BNode -> Int -> BNode
removeBase e base =
  BNode
    [ col { cLower = if null (cLower col) then [] else repIdx 0 (head (cLower col) - base) (cLower col)
          , cHigher = map (`removeBase` base) (cHigher col) }
    | col <- bCols e ]

higherRemoveBase :: BColumn -> [BNode]
higherRemoveBase c =
  let base = head (cLower c) + 1
   in map (`removeBase` base) (cHigher c)

isOneLineColumn :: BColumn -> Int -> Bool
isOneLineColumn c value =
  not (null (cLower c))
    && cLower c !! 0 == value
    && all (== 0) (drop 1 (cLower c))
    && null (cHigher c)

-- 类型与父树

data BType = Undef | Lower | Higher | Star | Mark | MarkStar
  deriving (Eq, Show)

data PCol = PCol
  { peEntry :: [Int]
  , peType :: BType
  , peTail :: Bool
  , peHigher :: [PNode]
  }
newtype PNode = PNode [PCol]

computeParents :: BNode -> Int -> PNode
computeParents e n = goNode e [] [] []
  where
    -- cs：列栈（含祖先列与本节点已处理的列）；ps：entry_parents 栈；
    -- os：outer_stack，用于 Mark / Higher 判定。
    goNode :: BNode -> [BColumn] -> [[Int]] -> [Int] -> PNode
    goNode cur cs0 ps0 os = PNode (reverse built)
      where
        lS0 = length cs0
        (built, _, _) = foldl step ([], cs0, ps0) (bCols cur)

        step (acc, cs, ps) col =
          let iS = length cs
              cs' = cs ++ [col]
              arr = lowerEntry cs' ps col
              ps' = ps ++ [arr]
              (typ, tailFlag, pLast) = typeInfo cs' ps' col iS
              arrN = repIdx n pLast arr
              subs = map (\h -> goNode h cs' ps' (os ++ [iS])) (cHigher col)
           in (PCol arrN typ tailFlag subs : acc, cs', ps')

        -- 第一段：逐行求父列（entry[0..n-1]）。p<0 时 C++ 直接 break，
        -- 后续行保持 -1，不能继续填。
        lowerEntry cs' ps col = go 0 (replicate (n + 1) (-1))
          where
            iS = length cs' - 1
            go j arr
              | j >= n = arr
              | otherwise =
                  let p = walk cs' (ps ++ [arr]) j iS
                   in if p < 0 then arr else go (j + 1) (repIdx j p arr)
            walk cs ps j p
              | p < 0 = -1
              | cLower (cs !! p) !! j < cLower col !! j = p
              | otherwise =
                  walk cs ps j (if j == 0 then p - 1 else (ps !! p) !! (j - 1))

        -- 第二段：定类型与 is_tail，并把 entry[n] 设为收尾的 p。
        typeInfo cs' ps' col iS = (typ, tailFlag, pLast)
          where
            pLast = walkType iS
            walkType p
              | p < 0 = -1
              | cMark pc < cMark col && p `elem` os = p
              | cMark pc <= cMark col && p `notElem` os
                  && higherCmp (higherRemoveBase pc) (higherRemoveBase col) < 0 = p
              | otherwise = walkType ((ps' !! p) !! (n - 1))
              where pc = cs' !! p
            typ
              | pLast < 0 =
                  if lS0 > 0 && isOneLineColumn col (cLower (cs' !! (lS0 - 1)) !! 0 + 1)
                    then Star
                    else Lower
              | cMark (cs' !! pLast) < cMark col && pLast `elem` os =
                  if length (cHigher col) == 1 && null (bCols (head (cHigher col)))
                    then MarkStar
                    else Mark
              | otherwise = Higher
            tailFlag
              | pLast < 0 = True
              | null (cHigher col) = error "illegal tail"
              | otherwise = null (bCols (last (cHigher col)))

-- 层 / 根

isZeroCol :: BColumn -> Bool
isZeroCol c = all (== 0) (cLower c) && null (cHigher c)

nextLayer :: BNode -> BNode
nextLayer cur =
  let right = length (bCols cur) - 1
      col = bCols cur !! right
      hr = length (cHigher col) - 1
   in cHigher col !! hr

skipLayer :: BNode -> Int -> BNode
skipLayer e k = iterate nextLayer e !! k

nextLayerP :: PNode -> PNode
nextLayerP (PNode cols) =
  let right = length cols - 1
      col = cols !! right
      hr = length (peHigher col) - 1
   in peHigher col !! hr

skipLayerP :: PNode -> Int -> PNode
skipLayerP p k = iterate nextLayerP p !! k

tailInfo :: PNode -> (Int, BType)
tailInfo p = go 0 p
  where
    go layer cur =
      let PNode ccols = cur
          right = length ccols - 1
       in if peTail (ccols !! right)
            then (layer, peType (ccols !! right))
            else go (layer + 1) (nextLayerP cur)

lastNonneg :: [Int] -> Int
lastNonneg v = go (length v - 1)
  where
    go i
      | i < 0 = (-1)
      | v !! i >= 0 = i
      | otherwise = go (i - 1)

root :: PNode -> Int -> (Int, Int)
root p tLayer =
  let PNode cols = skipLayerP p tLayer
      right = length cols - 1
      entry = peEntry (cols !! right)
      b = lastNonneg entry
   in if b < 0 then (-1, -1) else (entry !! b, b)

rootLayer :: BNode -> Int -> (Int, Int)
rootLayer e r = go 0 (length (bCols e)) e
  where
    go layer len cur =
      if len <= r
        then go (layer + 1) (len + length (bCols (nextLayer cur))) (nextLayer cur)
        else (layer, r - (len - length (bCols cur)))

ascensionVector :: BNode -> Int -> Int -> Int -> [Int]
ascensionVector e r b tLayer =
  let st = collect e 0
      collect cur k =
        bCols cur ++ if k >= tLayer then [] else collect (nextLayer cur) (k + 1)
      back = last st
   in [cLower back !! j - cLower (st !! r) !! j | j <- [0 .. b - 1]]

ascendVector :: [Int] -> Int -> [Int] -> Int -> [Int]
ascendVector v a vv w =
  [ if j < a && j < length vv then v !! j + vv !! j * w else v !! j | j <- [0 .. length v - 1] ]

-- 阈值

data ACol = ACol (Maybe Int) [ANode]
newtype ANode = ANode [ACol]

undefinedAt :: BNode -> ANode
undefinedAt e = ANode [ACol Nothing (map undefinedAt (cHigher col)) | col <- bCols e]

thresholds :: BNode -> PNode -> Maybe Int -> Int -> [Maybe Int] -> ANode
thresholds e (PNode pcols) ropt b stk =
  case ropt of
    Nothing -> undefinedAt e
    Just r -> ANode (snd (foldl (step r) (stk, []) (zip [0 ..] (bCols e))))
  where
    step r (stk', acc) (i, col) =
      let iS = length stk'
       in if iS < r && i + 1 /= length (bCols e)
            then (stk' ++ [Nothing], acc ++ [ACol Nothing (map undefinedAt (cHigher col))])
            else
              let ai
                    | iS == r = Just b
                    | iS > r =
                        let entry = peEntry (pcols !! i)
                         in Just (walkAi entry stk' r)
                    | otherwise = Nothing
                  subs =
                    [ thresholds (cHigher col !! ix) (peHigher (pcols !! i) !! ix) (Just r) b (stk' ++ [ai])
                    | ix <- [0 .. length (cHigher col) - 1]
                    ]
               in (stk' ++ [ai], acc ++ [ACol ai subs])
    walkAi entry stk' r =
      let go ai =
            if ai < length entry
              && entry !! ai >= r
              && entry !! ai < length stk'
              && maybe False (> ai) (stk' !! (entry !! ai))
              then go (ai + 1)
              else ai
       in go 0

-- 复制提升

cloneSlice :: BNode -> Int -> BNode
cloneSlice e from = BNode [bCols e !! i | i <- [from .. length (bCols e) - 1]]

popLast :: BNode -> BNode
popLast e = BNode (if null (bCols e) then [] else init (bCols e))

aSlice :: ANode -> Int -> ANode
aSlice (ANode cols) from = ANode [cols !! i | i <- [from .. length cols - 1]]

nextLayerA :: ANode -> ANode
nextLayerA (ANode cols) =
  let right = length cols - 1
      ac = cols !! right
      hr = length (aHigherA ac) - 1
   in aHigherA ac !! hr
  where
    aHigherA (ACol _ h) = h

skipLayerA :: ANode -> Int -> ANode
skipLayerA a k = iterate nextLayerA a !! k

ascendReplace :: BNode -> BNode -> Maybe Int -> Maybe ANode -> [Int] -> Int -> BNode
ascendReplace e tail tl aTree vv w =
  BNode (concatMap process (zip [0 ..] (bCols e)))
  where
    ncols = bCols e
    process (i, col)
      | tl == Just 0 && i + 1 == length ncols = map id (bCols tail)
      | otherwise =
          let aiVal = case aTree of
                Just (ANode acols) | i < length acols -> aTh (acols !! i)
                _ -> 0
              newLower = ascendVector (cLower col) aiVal vv w
              ntl = if i + 1 == length ncols then subtract 1 <$> tl else Nothing
              subs =
                [ let xntl = if ix == length (cHigher col) - 1 then ntl else Nothing
                      subA = case aTree of
                        Just (ANode acols) | i < length acols, ix < length (aThs (acols !! i)) -> Just (aThs (acols !! i) !! ix)
                        _ -> Nothing
                   in ascendReplace (cHigher col !! ix) tail xntl subA vv w
                | ix <- [0 .. length (cHigher col) - 1]
                ]
              mark = if null subs then 0 else cMark col
           in [BColumn newLower mark subs]
    aTh (ACol t _) = maybe 0 id t
    aThs (ACol _ h) = h

-- fs_star

fsStar :: BNode -> Int -> Int -> BNode
fsStar e tLayer index =
  let right = length (bCols e) - 1
      col = bCols e !! right
      hr = length (cHigher col) - 1
      keptCols = [bCols e !! i | i <- [0 .. right - 1]]
      keptHigher = [cHigher col !! k | k <- [0 .. hr - 1]]
      newHigher =
        if tLayer == 1
          then
            let h = cHigher col !! hr
                vertRight = length (bCols h) - 1
                newVert = BNode [bCols h !! k | k <- [0 .. vertRight - 1]]
                mark = if hr == 0 && index == 0 then 0 else cMark col
             in (mark, keptHigher ++ replicate index newVert)
          else (cMark col, keptHigher ++ [fsStar (cHigher col !! hr) (tLayer - 1) index])
      (mark, higher) = newHigher
   in BNode (keptCols ++ [BColumn (cLower col) mark higher])

isLimit :: BNode -> Bool
isLimit e = not (null (bCols e)) && not (isZeroCol (last (bCols e)))

-- fs

fs :: BNode -> Int -> Int -> BNode
fs e index n
  | null (bCols e) = e
  | not (isLimit e) = popLast e
  | otherwise =
      let pnode = computeParents e n
          (tLayer, typ) = tailInfo pnode
       in if typ == Undef
            then popLast e
            else if typ == Star
              then fsStar e tLayer index
              else
                let (r, b) = root pnode tLayer
                    (rLayer, ri) = rootLayer e r
                    vVec = ascensionVector e r b tLayer
                    stk = []
                    aTree = thresholds e pnode (Just r) b stk
                    copyPart0 = cloneSlice (skipLayer e rLayer) ri
                    copyPartA0 = aSlice (skipLayerA aTree rLayer) ri
                    (copyPart, copyPartA, diffLayer) =
                      if typ == Mark
                        then
                          let higherRight = length (cHigher (bCols copyPart0 !! 0)) - 1
                              c0 = BColumn (cLower (bCols copyPart0 !! 0)) 0 []
                              innerCols = bCols (cHigher (bCols copyPart0 !! 0) !! higherRight)
                              newCp = BNode (c0 : innerCols)
                              a0 = ACol (Just b) []
                              innerA = case copyPartA0 of ANode (ac : _) -> aHigherA ac !! higherRight
                              newCpA = ANode (a0 : case innerA of ANode acs -> acs)
                           in (newCp, newCpA, tLayer - rLayer - 1)
                        else (copyPart0, copyPartA0, tLayer - rLayer)
                    current = skipLayer e tLayer
                    currentA = skipLayerA aTree tLayer
                    right = length (bCols current) - 1
                    tailTop = [cHigher (bCols current !! right) !! k | k <- [0 .. length (cHigher (bCols current !! right)) - 2]]
                    tailTopA = let ANode acs = currentA in [aHigherA (acs !! right) !! k | k <- [0 .. length (aHigherA (acs !! right)) - 2]]
                    tailMark = if null tailTop then 0 else cMark (bCols current !! right)
                    result0 = makeNode
                    result1 = foldl (\res w ->
                      let r2 = ascendReplace copyPart res (Just diffLayer) (Just copyPartA) vVec w
                       in if typ == Higher || typ == Mark
                            then
                              let c0 = BColumn (cLower (bCols r2 !! 0)) tailMark
                                        [ascendReplace (tailTop !! ix) makeNode Nothing (Just (tailTopA !! ix)) vVec (w - 1) | ix <- [0 .. length tailTop - 1]]
                               in r2 { bCols = repIdx 0 c0 (bCols r2) }
                            else r2)
                      result0
                      [index, index - 1 .. 1]
                    result2 = ascendReplace e result1 (Just tLayer) (Just aTree) vVec 0
                 in result2
  where
    aHigherA (ACol _ h) = h
    aHigherA2 (ANode cols) = cols

lnz :: BNode -> Int -> Int -> BNode
lnz e index n
  | not (isLimit e) = fs e 0 n
  | otherwise = fs e index n

-- 入口

expandBtl :: String -> Int -> Int -> String
expandBtl text term n0 =
  let n = max 1 (min 25 n0)
      term' = max 1 term
   in case runP text (parseNode n) of
        Right (e, i) | i >= length text -> safeRun (display (lnz e term' n))
        _ -> kParseError

safeRun :: String -> String
safeRun action = unsafePerformIO $ do
  r <- try (do _ <- evaluate (length action); return action) :: IO (Either SomeException String)
  case r of
    Left _ -> return kParseError
    Right s -> return s

-- 解析

parseNode :: Int -> P BNode
parseNode n = do
  sp
  do
      e <- parseExpr n
      sp
      end <- atEnd
      if end then return e else err
  where
    advanceN k = sequence_ (replicate k advance)

parseExpr :: Int -> P BNode
parseExpr n = do
  sp
  cols <- parseCols
  return (BNode cols)
  where
    parseCols = do
      sp
      mc <- peek
      case mc of
        Just '(' -> do
          c <- parseColumn n
          rest <- parseCols
          return (c : rest)
        _ -> return []

parseColumn :: Int -> P BColumn
parseColumn n = do
  sp
  expect '('
  (numbers, mark, higher) <- goNumbers [] Nothing [] False True
  sp
  expect ')'
  let numbers' = numbers ++ replicate (n - length numbers) (maybe 0 id mark)
      finalMark = if null higher then 0 else maybe 0 id mark
  if not (null higher) && finalMark == 0 then err else return (BColumn numbers' finalMark higher)
  where
    expect ch = do
      mc <- peek
      case mc of
        Just c | c == ch -> advance
        _ -> err
    goNumbers numbers mark higher inHigher expectValue = do
      sp
      mc <- peek
      case mc of
        Nothing -> err
        Just ')' -> return (numbers, mark, higher)
        Just c | c == ',' || c == ';' -> do
          if expectValue then err else return ()
          whenSemi <-
            if c == ';'
              then if length numbers /= n then err else return True
              else return inHigher
          advance
          goNumbers numbers mark higher whenSemi True
        Just c | c >= '0' && c <= '9' -> do
          if not expectValue then err else return ()
          v <- numTok
          sp
          mc2 <- peek
          case mc2 of
            Just '^' -> do
              advance
              mark' <- case mark of
                Nothing -> return (Just v)
                Just m -> if m == v then return mark else err
              h <- parseExpr n
              goNumbers numbers mark' (higher ++ [h]) True False
            _ ->
              if inHigher || length numbers >= n
                then do
                  mark' <- case mark of
                    Nothing -> return (Just v)
                    Just m -> if m == v then return mark else err
                  goNumbers numbers mark' (higher ++ [makeNode]) True False
                else goNumbers (numbers ++ [v]) mark higher False False
        _ -> err

repIdx :: Int -> a -> [a] -> [a]
repIdx k v xs = take k xs ++ [v] ++ drop (k + 1) xs
