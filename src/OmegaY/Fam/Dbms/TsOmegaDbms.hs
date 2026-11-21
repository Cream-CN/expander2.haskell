{-# LANGUAGE OverloadedStrings #-}
-- | TSωDBMS（id: ts-omega-dbms）。忠实移植 ts_omega_dbms.hpp。
module OmegaY.Fam.Dbms.TsOmegaDbms (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common
import Data.List (sortBy)

-- Height = [HeightEntry{anchor, position}]
data HeightEntry = HE !Int Expr deriving (Show)
newtype Height = Height [HeightEntry] deriving (Show)
type E = (Int, Height)
type Col = [E]
type Expr = [Col]
data OrdO = OrEpsilon Expr | OrCnf [(OrdO, Integer)] deriving (Show)

errP = errParse
errR = errResource

-- ---------- Ordinal ----------
ordZero :: OrdO
ordZero = OrCnf []
ordOne :: OrdO
ordOne = OrCnf [(ordZero, 1)]
ordOmega :: OrdO
ordOmega = ordPowOmega ordOne

ordTerms :: OrdO -> [(OrdO, Integer)]
ordTerms (OrEpsilon e) = [(OrEpsilon e, 1)]
ordTerms (OrCnf ts) = ts

ordFromTerms :: [(OrdO, Integer)] -> OrdO
ordFromTerms [] = ordZero
ordFromTerms [t@(OrEpsilon _, 1)] = fst t
ordFromTerms ts = OrCnf ts

ordIsZero :: OrdO -> Bool
ordIsZero (OrCnf []) = True
ordIsZero _ = False

ordNatural :: Integer -> Either String OrdO
ordNatural v
  | v < 0 = Left "neg"
  | v == 0 = Right ordZero
  | otherwise = Right (ordFromTerms [(ordZero, v)])

sgnI :: Int -> Int
sgnI v = if v < 0 then -1 else if v > 0 then 1 else 0

cmpOrd :: OrdO -> OrdO -> Int
cmpOrd a b = case (a, b) of
  (OrEpsilon ea, OrEpsilon eb) -> cmpExpr ea eb
  (OrEpsilon _, _) -> negate (cmpOrd b a)
  (_, OrEpsilon eb) ->
    case a of
      OrCnf [] -> -1
      OrCnf ((tExp, tCoeff) : _) ->
        let c = cmpOrd tExp b
         in if c /= 0 then c
            else if length (ordTerms a) == 1 && tCoeff == 1 then 0 else 1
  (OrCnf ta, OrCnf tb) ->
    let n = min (length ta) (length tb)
        go k | k >= n = sgnI (length ta - length tb)
             | otherwise =
                 let (ae, ac) = ta !! k
                     (be, bc) = tb !! k
                     c = cmpOrd ae be
                  in if c /= 0 then c
                     else if ac /= bc then sgnI (fromIntegral (if ac < bc then -1 else 1) :: Int)
                     else go (k + 1)
     in go 0

ordAdd :: OrdO -> OrdO -> OrdO
ordAdd a b =
  let right = ordTerms b
   in if null right then a
      else
        let left = ordTerms a
         in case left of
              [] -> ordFromTerms right
              _ ->
                let lExp = fst (last left)
                    lCoeff = snd (last left)
                    r0 = head right
                 in if cmpOrd lExp (fst r0) < 0
                      then ordAdd (ordFromTerms (init left)) b
                      else if cmpOrd lExp (fst r0) == 0
                        then ordFromTerms (init left ++ [(lExp, lCoeff + snd r0)] ++ tail right)
                        else ordFromTerms (left ++ right)

ordPowOmega :: OrdO -> OrdO
ordPowOmega e = ordFromTerms [(e, 1)]

ordMul :: OrdO -> OrdO -> OrdO
ordMul a b
  | ordIsZero a || ordIsZero b = ordZero
  | otherwise =
      let left = ordTerms a
       in foldl ordAdd ordZero
            [ let tExp = fst t
                  tCoeff = snd t
               in if ordIsZero tExp
                    then ordFromTerms ((fst (head left), snd (head left) * tCoeff) : tail left)
                    else ordFromTerms [(ordAdd (fst (head left)) tExp, tCoeff)]
            | t <- ordTerms b
            ]

ordTailAfter :: OrdO -> OrdO -> Either String OrdO
ordTailAfter a b =
  if cmpOrd a b > 0 then Left "neg tail"
  else
    let left = ordTerms a
        right = ordTerms b
        n = min (length left) (length right)
        go k | k >= n = k
             | otherwise =
                 let (le, lc) = left !! k
                     (re, rc) = right !! k
                  in if cmpOrd le re == 0 && lc == rc then go (k + 1) else k
        i = go 0
     in if i == length left || cmpOrd (fst (left !! i)) (fst (right !! i)) < 0
          then Right (ordFromTerms (drop i right))
          else
            let (rExp, rCoeff) = right !! i
                (lExp, lCoeff) = left !! i
             in Right (ordFromTerms ((rExp, rCoeff - lCoeff) : drop (i + 1) right))

maxEpsilonAtom :: OrdO -> Maybe OrdO
maxEpsilonAtom (OrEpsilon e) = Just (OrEpsilon e)
maxEpsilonAtom (OrCnf ts) = foldl step Nothing ts
  where
    step acc t = case maxEpsilonAtom (fst t) of
      Just atom -> case acc of
        Nothing -> Just atom
        Just a -> if cmpOrd atom a > 0 then Just atom else Just a
      Nothing -> acc

-- ---------- 矩阵比较 ----------
cmpHeight :: Height -> Height -> Int
cmpHeight (Height aE) (Height bE) =
  let n = min (length aE) (length bE)
      go k | k >= n =
                if length aE == length bE then 0
                else if length aE > length bE then -1 else 1
           | otherwise =
               let (HE aa pa) = aE !! k
                   (HE bb pb) = bE !! k
                   c = sgnI (aa - bb)
                in if c /= 0 then c
                   else let c2 = cmpExpr pa pb in if c2 /= 0 then c2 else go (k + 1)
   in go 0

cmpEntry :: E -> E -> Int
cmpEntry (pa, ha) (pb, hb) =
  case sgnI (pa - pb) of
    0 -> cmpHeight ha hb
    c -> c

cmpCol :: Col -> Col -> Int
cmpCol a b =
  let n = min (length a) (length b)
      go k | k >= n = sgnI (length a - length b)
           | otherwise = let c = cmpEntry (a !! k) (b !! k) in if c /= 0 then c else go (k + 1)
   in go 0

cmpExpr :: Expr -> Expr -> Int
cmpExpr a b =
  let n = min (length a) (length b)
      go k | k >= n = sgnI (length a - length b)
           | otherwise = let c = cmpCol (a !! k) (b !! k) in if c /= 0 then c else go (k + 1)
   in go 0

isLimit :: Expr -> Bool
isLimit e = not (null e) && not (null (last e))

heightOf :: Col -> Maybe Height
heightOf [] = Nothing
heightOf col = Just (snd (last col))

mergeCol :: Col -> Col -> Col
mergeCol a b =
  let h = heightOf a
   in a ++ [e | e@(_, he) <- b, case h of Nothing -> True; Just h0 -> cmpHeight he h0 > 0]

predecessor :: Expr -> Either String Expr
predecessor position
  | null position || isLimit position = Left "not successor"
  | otherwise = Right (take (length position - 1) position)

heightFillDec :: Height -> Expr -> Int -> Height
heightFillDec (Height entries) position anchor =
  let entries1 = reverse (dropWhile (\(HE _ p) -> cmpExpr p position <= 0) (reverse entries))
      entries2 = if not (null entries1) && anchorOf (last entries1) == anchor
                   then init entries1
                   else entries1
   in Height (entries2 ++ [HE anchor position])
  where
    anchorOf (HE a _) = a

computeNewHeight :: Height -> Expr -> Int -> Either String (Maybe Height)
computeNewHeight h@(Height entries) expr root =
  if null entries then Left "empty height"
  else
    let HE anchor position = last entries
     in if not (null position)
          then do
            p <- predecessor position
            Right (Just (heightFillDec h p root))
          else
            if length entries == 1
              then Right (heightOf (expr !! anchor))
              else
                let HE boundA bound = entries !! (length entries - 2)
                    col = expr !! anchor
                    hits = [j | j <- [0 .. length col - 1], cmpHeight h (snd (col !! j)) <= 0]
                 in case hits of
                      [] -> Right (heightOf col)
                      (j : _) -> do
                        bP <- predecessor bound
                        let result0 = heightFillDec h bP (fst (col !! j))
                            result = if j > 0 && cmpHeight (snd (col !! (j - 1))) result0 > 0
                                       then snd (col !! (j - 1))
                                       else result0
                        Right (Just result)

remapCol :: Col -> (Int -> Int) -> (Int -> Expr -> Int) -> Col
remapCol col mp ma =
  [ (mp p, Height [HE (ma a lv) lv | HE a lv <- heList])
  | (p, Height heList) <- col ]

copyCol :: Col -> Int -> Int -> Col
copyCol col root off = remapCol col mp (\n _ -> mp n)
  where mp n = if n >= root then n + off else n

shiftCol :: Col -> Int -> Col
shiftCol col off = remapCol col (\n -> n + off) (\n _ -> n + off)

sliceComponent :: Expr -> Int -> Int -> Expr
sliceComponent expr start end =
  [ shiftCol (expr !! i) (-start) | i <- [start .. end - 1] ]

concatenate :: [Expr] -> Expr
concatenate = foldl step []
  where
    step result part =
      let offset = length result
       in result ++ [shiftCol col offset | col <- part]

pureLow :: Col -> Bool
pureLow col =
  length col == 1 &&
  let (_, Height [HE a p]) = head col in a == 0 && null p

lowCol :: Col -> Int -> Bool
lowCol col parent = pureLow col && fst (head col) == parent

tailTemplate :: Expr -> Int -> Either String Expr
tailTemplate expr cut =
  Right ([] : [ remapCol (expr !! i) mp ma | i <- [cut .. length expr - 1] ])
  where
    mp n = if n == 0 then 0 else n - cut + 1
    ma n _ = mp n

rootSubstitute :: Expr -> Expr -> Expr
rootSubstitute head tmpl =
  snd (foldl step ([0], []) [0 .. length tmpl - 1])
  where
    step (mapping, result) i =
      let mi = length result
       in if null (tmpl !! i)
            then
              let offset = length result
               in (mapping ++ [mi], result ++ [shiftCol col offset | col <- head])
            else
              let mp n = mapping !! n
               in (mapping ++ [mi], result ++ [remapCol (tmpl !! i) mp (\n _ -> mp n)])

lowTemplate :: Expr
lowTemplate = [[], [(0, Height [HE 0 []])]]

cleanMatrixPadding :: Expr -> Expr
cleanMatrixPadding expr =
  let result0 = [ [], [(0, Height [HE 0 []])] ] ++ [ shiftCol (expr !! i) 1 | i <- [1 .. length expr - 1] ]
   in if pureLow (last expr)
        then
          let lastCol = last result0
              (p, _) = head lastCol
           in init result0 ++ [[(p, Height [HE 0 []])]]
        else result0

convertMatrixOne :: Expr -> Bool -> Either String Expr
convertMatrixOne expr toRel
  | length expr == 1 = Right expr
  | toRel =
      if lowCol (expr !! 1) 0
        then Right expr
        else
          case [i | i <- [1 .. length expr - 1], lowCol (expr !! i) 0] of
            [] -> Right (cleanMatrixPadding expr)
            (cut : _) -> do
              let head = cleanMatrixPadding (sliceComponent expr 0 cut)
              tail <- tailTemplate expr cut
              let threshold = head ++ [[(1, Height [HE 0 []])]]
              Right (if cmpExpr tail threshold < 0 then rootSubstitute head tail else tail)
  | otherwise =
      if not (lowCol (expr !! 1) 0)
        then Left "invalid prefix"
        else
          let bound = [[], [(0, Height [HE 0 []])], [(1, Height [HE 1 []])]]
           in if cmpExpr expr bound < 0
                then Right expr
                else
                  case [i | i <- [2 .. length expr - 1], lowCol (expr !! i) 0] of
                    [] -> do
                      let cut = length expr
                          prefix = take cut expr
                      restored <- buildRestored prefix
                      if cut == length expr
                        then Right restored
                        else do
                          tail <- tailTemplate expr cut
                          Right (rootSubstitute restored tail)
                    (cut : _) -> do
                      let prefix = take cut expr
                      restored <- buildRestored prefix
                      tail <- tailTemplate expr cut
                      Right (rootSubstitute restored tail)
  where
    buildRestored prefix = do
      let restored0 = [[]]
          loop i acc
            | i >= length prefix = Right (reverse acc)
            | otherwise =
                let col = prefix !! i
                    last = i == length prefix - 1
                    allGe = all (\(p, Height entries) -> p >= 1 && let HE a _ = head entries in a >= 1) col
                 in if allGe
                      then
                        let candidate = shiftCol col (-1)
                            correctedLast = last && pureLow candidate
                         in if not (lowCol candidate 0) && not correctedLast
                              then loop (i + 1) (candidate : acc)
                              else fallback i col last acc
                      else fallback i col last acc
          fallback i col last acc =
            if last && pureLow col && fst (head col) > 1
              then loop (i + 1) ([(fst (head col) - 1, Height [HE 0 []])] : acc)
              else
                let parent = fst (head col) - 1
                    newCol = if parent > 0 then [(parent, Height [HE 0 []])] else []
                 in Right (rootSubstitute (reverse (newCol : acc)) (drop i prefix))
      loop 2 restored0

convertMatrix :: Expr -> Bool -> Either String Expr
convertMatrix expr toRel = do
  let roots = [i | i <- [0 .. length expr - 1], null (expr !! i)]
  parts <- mapM (\i ->
      let end = if i + 1 < length roots then roots !! (i + 1) else length expr
       in convertMatrixOne (sliceComponent expr (roots !! i) end) toRel)
      [0 .. length roots - 1]
  Right (concatenate (sortBy (\a b -> if cmpExpr a b > 0 then LT else if cmpExpr a b < 0 then GT else EQ) parts))

suspend :: Expr -> Expr -> Expr
suspend head tmpl =
  let offset = length head - 1
      mp p = if p == 0 then 1 else p + offset
      ma a lv = if a == 0 && null lv then 0 else a + offset
   in head ++ [ remapCol (tmpl !! i) mp ma | i <- [1 .. length tmpl - 1] ]

unsuspend :: Expr -> Int -> Either String Expr
unsuspend expr cut =
  Right ([] : [ remapCol (expr !! i) mp ma | i <- [cut .. length expr - 1] ])
  where
    mp p = if p == 1 then 0 else p - cut + 1
    ma a lv = if a == 0 && null lv then 0 else a - cut + 1

relativeToCnf :: Expr -> Either String OrdO
relativeToCnf expr
  | null expr = Right ordZero
  | length expr == 1 = Right ordOne
  | otherwise = do
      let roots = [i | i <- [0 .. length expr - 1], null (expr !! i)]
      if length roots > 1
        then do
          parts <- mapM (\j ->
              let end = if j + 1 < length roots then roots !! (j + 1) else length expr
               in relativeToCnf (sliceComponent expr (roots !! j) end))
              [0 .. length roots - 1]
          Right (foldl ordAdd ordZero parts)
        else
          if not (lowCol (expr !! 1) 0) then Left "invalid prefix"
          else
            case [i | i <- [2 .. length expr - 1], lowCol (expr !! i) 0] of
              (two : _) -> do
                let a = take two expr
                ca <- relativeToCnf a
                tE <- tailTemplate expr two
                ct <- relativeToCnf tE
                Right (ordMul ca ct)
              [] ->
                if length expr == 2 then Right ordOmega
                else
                  case [i | i <- [2 .. length expr - 1], lowCol (expr !! i) 1] of
                    [] -> Right (OrEpsilon expr)
                    (three : _) -> do
                      let head = if three == 2 then ordOne else OrEpsilon (take three expr)
                      us <- unsuspend expr three
                      ru <- relativeToCnf us
                      Right (ordPowOmega (ordMul head ru))

relativePower :: OrdO -> Either String Expr
relativePower exponent
  | isEps exponent = let OrEpsilon e = exponent in Right e
  | null (ordTerms exponent) = Right [[]]
  | length (ordTerms exponent) > 1 || snd (head (ordTerms exponent)) > 1 = do
      let init0 = [[]]
      foldl (\acc t -> do
                r <- acc
                factor <- relativePower (ordPowOmega (fst t))
                let count = snd t
                Right (foldl (\r _ -> rootSubstitute r factor) r [1 .. count]))
              (Right init0) (ordTerms exponent)
  | otherwise =
      case maxEpsilonAtom exponent of
        Nothing -> do
          r <- relativeFromCnf exponent
          Right (suspend lowTemplate r)
        Just maxAtom -> do
          let me = case maxAtom of OrEpsilon e -> e
          ta <- ordTailAfter maxAtom (fst (head (ordTerms exponent)))
          let quotient = ordPowOmega ta
          r <- relativeFromCnf quotient
          Right (suspend me r)

isEps :: OrdO -> Bool
isEps (OrEpsilon _) = True
isEps _ = False

relativeFromCnf :: OrdO -> Either String Expr
relativeFromCnf value
  | isEps value = let OrEpsilon e = value in Right e
  | otherwise = do
      let terms = ordTerms value
      blocksE <- mapM (\t -> do
          block <- relativePower (fst t)
          let count = snd t
          Right (replicate (fromIntegral count) block))
        terms
      Right (concatenate (concat blocksE))

tsOmegaToCnf :: Expr -> Either String OrdO
tsOmegaToCnf expr =
  if not (null expr) && not (null (head expr)) then Left "must start empty"
  else do
    m <- convertMatrix expr True
    relativeToCnf m

cnfToTsOmega :: OrdO -> Either String Expr
cnfToTsOmega value = do
  r <- relativeFromCnf value
  convertMatrix r False

-- ---------- root components / matrix add / tail ----------
rootComponents :: Expr -> Either String [Expr]
rootComponents expr =
  if null expr || not (null (head expr)) then Left "invalid root"
  else do
    let roots = [i | i <- [0 .. length expr - 1], null (expr !! i)]
    Right [ sliceComponent expr (roots !! j)
              (if j + 1 < length roots then roots !! (j + 1) else length expr)
          | j <- [0 .. length roots - 1] ]

matrixAdd :: Expr -> Expr -> Either String Expr
matrixAdd a b = do
  left <- rootComponents a
  right <- rootComponents b
  if null right then Right a
  else
    let left' = reverse (dropWhile (\c -> cmpExpr c (head right) < 0) (reverse left))
     in Right (concatenate (left' ++ right))

matrixTail :: Expr -> Expr -> Either String Expr
matrixTail a b =
  if cmpExpr a b > 0 then Left "neg tail"
  else do
    left <- rootComponents a
    right <- rootComponents b
    let n = min (length left) (length right)
        k = length (takeWhile (\i -> cmpExpr (left !! i) (right !! i) == 0) [0 .. n - 1])
    Right (concatenate (drop k right))

-- ---------- limit_approximation ----------
limitApproximation :: Height -> Maybe Height -> Int -> Bool -> Either String Height
limitApproximation (Height entries) lower index alter =
  if null entries then Left "empty h"
  else
    let HE anchor alpha = last entries
        prefix = init entries
        zeroCap = Height (prefix ++ [HE anchor []])
     in case lower of
          Just l | cmpHeight zeroCap l <= 0 -> do
            -- check lower prefix matches
            if length (lowerEntries l) < length entries
              then Left "invalid prefix"
              else
                let (Height lEntries) = l
                    prefixOk = all (\i ->
                        let HE aL pL = lEntries !! i
                            HE aP pP = prefix !! i
                         in aL == aP && cmpExpr pL pP == 0) [0 .. length prefix - 1]
                 in if not prefixOk then Left "invalid prefix"
                    else do
                      let HE nextA beta = lEntries !! length prefix
                      if nextA /= anchor then Left "invalid prefix"
                      else if cmpExpr beta alpha >= 0 then Left "invalid interval"
                      else do
                        difference <- if null beta then Right alpha else matrixTail beta alpha
                        approximation <- fundamentalSeq difference index alter
                        if null approximation then Left "zero approx"
                        else do
                          newLevel <- if null beta then Right approximation else matrixAdd beta approximation
                          Right (Height (prefix ++ [HE anchor newLevel]))
          _ -> do
            let beta = []
                difference = alpha
            approximation <- fundamentalSeq difference index alter
            if null approximation then Left "zero approx"
            else Right (Height (prefix ++ [HE anchor approximation]))
  where
    lowerEntries (Height es) = es

-- ---------- expand ----------
expandF :: Expr -> Int -> Bool -> Bool -> Either String Expr
expandF expr index shorter alter =
  if index < 0 then Left "bad index"
  else if null expr then Right expr
  else
    let right = length expr - 1
        col = expr !! right
     in if null col
          then Right (take (length expr - 1) expr)
          else
            let (root, h@(Height entries)) = last col
             in if isLimit (positionOf (last entries))
                  then do
                    let replacement = take (length col - 1) col
                    h' <- limitApproximation h (heightOf replacement) index alter
                    Right (take (length expr - 1) expr ++ [replacement ++ [(root, h')]])
                  else do
                    next <- computeNewHeight h expr root
                    let replacement0 = take (length col - 1) col
                        replacement1 = case next of
                          Just h0 -> mergeCol replacement0 [(root, h0)]
                          Nothing -> replacement0
                        replacement2 = mergeCol replacement1 (expr !! root)
                        result0 = take (length expr - 1) expr ++ [replacement2]
                        result1 = foldl addCopy result0 [1 .. index]
                        addCopy rs w = foldl (addOne w) rs [root + 1 .. right]
                        addOne w rs i = rs ++ [copyCol (rs !! i) root ((right - root) * w)]
                     in Right (if shorter then take (length result1 - 1) result1 else result1)
  where
    positionOf (HE _ p) = p

fundamentalSeq :: Expr -> Int -> Bool -> Either String Expr
fundamentalSeq expr index alter =
  if index < 0 then Left "bad index"
  else if not (isLimit expr)
    then Right (if null expr then expr else take (length expr - 1) expr)
    else
      let (root, h@(Height entries)) = last (last expr)
       in if isLimit (positionOf (last entries))
            then expandF expr index True alter
            else expandF expr index (not alter) alter
  where
    positionOf (HE _ p) = p

-- ---------- 显示 ----------
isSimpleExp :: String -> Bool
isSimpleExp e = e == "w" || (not (null e) && all (\c -> c >= '0' && c <= '9') e)

superscript :: String -> String -> Bool -> String
superscript base exp pair =
  if pair || isSimpleExp exp then base ++ "^" ++ exp else base ++ "^(" ++ exp ++ ")"

displayOrdinal :: OrdO -> Either String String
displayOrdinal value =
  case maxEpsilonAtom value of
    Just _ -> do
      e <- cnfToTsOmega value
      Right (displayExpr e)
    Nothing ->
      let terms = ordTerms value
       in if null terms then Right "0"
          else Right (concatTerms terms)
  where
    concatTerms terms =
      let parts = [ if ordIsZero (fst t)
                      then show (snd t)
                      else
                        let base = if cmpOrd (fst t) ordOne /= 0
                                     then superscript "w" (displayOrdinalStr (fst t)) False
                                     else "w"
                         in if snd t /= 1 then base ++ "*" ++ show (snd t) else base
                   | t <- terms ]
       in foldr (\x acc -> if null acc then x else x ++ "+" ++ acc) "" parts
    displayOrdinalStr o = case displayOrdinal o of Right s -> s; Left _ -> ""

heightDisplay :: Height -> Either String String
heightDisplay (Height entries) = do
  parts <- mapM (\(HE a p) -> do
      c <- tsOmegaToCnf p
      d <- displayOrdinal c
      Right (show (a + 1) ++ "@" ++ d)) entries
  Right ("(" ++ foldr (\x acc -> if null acc then x else x ++ "," ++ acc) "" parts ++ ")")

entryDisplay :: E -> Either String String
entryDisplay (p, h) = do
  hd <- heightDisplay h
  Right (superscript (show (p + 1)) hd True)

displayExpr :: Expr -> String
displayExpr = concatMap colD
  where
    colD [] = "(0)"
    colD cs = "(" ++ joinE cs ++ ")"
    joinE cs = foldr (\e acc ->
      let s = case entryDisplay e of Right x -> x; Left _ -> ""
       in s ++ (if null acc then "" else "," ++ acc)) "" cs

-- ---------- 解析 ----------
stripWs :: String -> String
stripWs = concatMap (\c -> if c == ' ' || c == '\t' || c == '\r' || c == '\n' then "" else [c])

splitTop :: String -> Char -> Either String [String]
splitTop text sep = go 0 [] "" []
  where
    go i closing acc out
      | i >= length text = Right (reverse (acc : out))
      | otherwise =
          let ch = text !! i
           in case ch of
             '(' -> go (i + 1) (')' : closing) (acc ++ [ch]) out
             '{' -> go (i + 1) ('}' : closing) (acc ++ [ch]) out
             ')' -> if null closing || head closing /= ch then Left "mismatch" else go (i + 1) (tail closing) (acc ++ [ch]) out
             '}' -> if null closing || head closing /= ch then Left "mismatch" else go (i + 1) (tail closing) (acc ++ [ch]) out
             _ -> if ch == sep && null closing
                    then go (i + 1) closing [] (reverse acc : out)
                    else go (i + 1) closing (acc ++ [ch]) out

closingIdx :: String -> Int -> Either String Int
closingIdx text start = go start []
  where
    go i closing
      | i >= length text = Left "unclosed"
      | otherwise =
          let ch = text !! i
           in case ch of
             '(' -> go (i + 1) (')' : closing)
             '{' -> go (i + 1) ('}' : closing)
             ')' -> if null closing || head closing /= ch then Left "mismatch"
                    else if null (tail closing) then Right i else go (i + 1) (tail closing)
             '}' -> if null closing || head closing /= ch then Left "mismatch"
                    else if null (tail closing) then Right i else go (i + 1) (tail closing)
             _ -> go (i + 1) closing

parseIndex :: String -> Either String Int
parseIndex text =
  if null text || not (all (\c -> c >= '0' && c <= '9') text) then Left "bad index"
  else let v = read text :: Integer
       in if v < 1 then Left "bad index"
          else Right (fromIntegral v - 1)

parseOrdinal :: String -> Either String OrdO
parseOrdinal raw =
  let source = stripWs raw
   in if null source then Left "bad ordinal"
      else do
        sum <- splitTop source '+'
        if length sum > 1
          then foldl (\acc t -> do
                        a <- acc
                        o <- parseOrdinal t
                        Right (ordAdd a o)) (Right ordZero) sum
          else do
            product <- splitTop source '*'
            if length product > 1
              then
                if length product /= 2 || not (all (\c -> c >= '0' && c <= '9') (product !! 1))
                  then Left "bad coefficient"
                  else do
                    o <- parseOrdinal (product !! 0)
                    n <- ordNatural (read (product !! 1))
                    Right (ordMul o n)
              else
                if all (\c -> c >= '0' && c <= '9') source
                  then ordNatural (read source)
                  else if source == "w" then Right ordOmega
                  else if length source >= 2 && head source == 'w' && (source !! 1) == '^'
                    then do
                      e <- parseOrdinal (drop 2 source)
                      Right (ordPowOmega e)
                    else case head source of
                      '(' -> do
                        end <- closingIdx source 0
                        if end == length source - 1
                          then parseOrdinal (take (end - 1) (drop 1 source))
                          else Left "bad"
                      '{' -> Left "bad"
                      _ -> Left "bad"
  where

matchEntry :: String -> Either String (Int, String)
matchEntry entry =
  let digits = takeWhile (\c -> c >= '0' && c <= '9') entry
   in if null digits then Left "bad entry"
      else
        let rest = drop (length digits) entry
            rest2 = if not (null rest) && head rest == '^' then tail rest else rest
         in if null rest2 || head rest2 /= '(' then Left "bad entry"
            else do
              end <- closingIdx rest2 0
              if end == length rest2 - 1
                then do
                  p <- parseIndex digits
                  Right (p, rest2)
                else Left "bad entry"

stripOuterParens :: String -> String
stripOuterParens inner =
  if not (null inner) && head inner == '('
    then
      case closingIdx inner 0 of
        Right end | end == length inner - 1 -> stripOuterParens (take (end - 1) (drop 1 inner))
        _ -> inner
    else inner

fromDisplay :: String -> Either String Expr
fromDisplay text =
  let source = stripWs text
   in go source []
  where
    go s result =
      if null s then Right result
      else
        if head s /= '(' then Left "bad column"
        else do
          end <- closingIdx s 0
          let inside = take (end - 1) (drop 1 s)
          marked <- splitTop inside ':'
          if length marked > 2 then Left "bad column index"
          else do
            let content = head marked
            col <- if content /= "" && content /= "0"
                     then do
                       entries <- splitTop content ','
                       colE <- foldl (parseEntry result) (Right []) entries
                       Right colE
                     else Right []
            go (drop (end + 1) s) (result ++ [col])
    parseEntry :: Expr -> Either String Col -> String -> Either String Col
    parseEntry result acc e = do
      colSoFar <- acc
      (parent, body) <- matchEntry e
      end <- closingIdx body 0
      if end /= length body - 1 then Left "bad height"
      else do
        let inner0 = take (length body - 2) (drop 1 body)
            inner = stripOuterParens inner0
        slots <- splitTop inner ','
        heightEntries <- mapM (parseSlot parent) slots
        -- validate strictly increasing
        let pairsOk = all (\k ->
              let HE aP pP = heightEntries !! (k - 1)
                  HE aC pC = heightEntries !! k
               in aP < aC && cmpExpr pP pC < 0 && not (isLimit pP)) [1 .. length heightEntries - 1]
        if not pairsOk then Left "height entries must increase"
        else
          let h = Height heightEntries
           in if not (null colSoFar) &&
                 (fst (last colSoFar) <= parent ||
                  cmpHeight (snd (last colSoFar)) h >= 0)
                then Left "entries must decrease"
                else Right (colSoFar ++ [(parent, h)])
    parseSlot parent slot = do
      parts <- splitTop slot '@'
      if length parts /= 2 then Left "bad slot"
      else do
        anchor <- parseIndex (parts !! 0)
        posO <- parseOrdinal (parts !! 1)
        pos <- cnfToTsOmega posO
        if anchor > parent then Left "anchor > parent"
        else Right (HE anchor pos)

-- ---------- 边界 ----------
expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term alter =
  let term' = if term < 1 then 1 else term
   in case fromDisplay text of
        Left _ -> errP
        Right expr ->
          if null expr then ""
          else case fundamentalSeq expr term' alter of
            Left _ -> errR
            Right r -> displayExpr r

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandAlter :: String -> Int -> String
expandAlter text term = expandTextImpl text term True
