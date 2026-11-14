{-# LANGUAGE OverloadedStrings #-}
-- | TS2DBMS（id: ts-2-dbms）。忠实移植 ts2dbms.hpp。
-- 解析失败一律返回 Left（边界映射为 errParse）；展开资源超限返回 Left（映射为 errResource）。
module OmegaY.Fam.Dbms.Ts2Dbms (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common
import Data.List (sortBy)

-- ---------- 类型 ----------
data Height = Height !Int Expr deriving (Show)  -- anchor, level
type E = (Int, Height)                           -- parent, height
type Col = [E]
type Expr = [Col]
data OrdO = OrEpsilon Expr | OrCnf [(OrdO, Integer)] deriving (Show)

errP = errParse
errR = errResource

-- ---------- Ordinal 基础 ----------
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
  | v < 0 = Left "negative"
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
        go k
          | k >= n = sgnI (length ta - length tb)
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
  if cmpOrd a b > 0 then Left "negative tail"
  else
    let left = ordTerms a
        right = ordTerms b
        n = min (length left) (length right)
        go k
          | k >= n = k
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
maxEpsilonAtom (OrCnf ts) =
  foldl step Nothing ts
  where
    step acc t = case maxEpsilonAtom (fst t) of
      Just atom -> case acc of
        Nothing -> Just atom
        Just a -> if cmpOrd atom a > 0 then Just atom else Just a
      Nothing -> acc

-- ---------- 矩阵比较 ----------
cmpHeight :: Height -> Height -> Int
cmpHeight (Height _ la) (Height _ lb) = cmpExpr la lb

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

height1 :: Col -> Expr
height1 col = case heightOf col of
  Just (Height _ lv) -> lv
  Nothing -> []

mergeCol :: Col -> Col -> Col
mergeCol a b =
  let h = heightOf a
   in a ++ [e | e@(_, he) <- b, case h of Nothing -> True; Just h0 -> cmpHeight he h0 > 0]

remapCol :: Col -> (Int -> Int) -> (Int -> Expr -> Int) -> Col
remapCol col mp ma =
  [ (mp p, Height (ma pLv lv) lv) | (p, Height pLv lv) <- col ]

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

lowCol :: Col -> Int -> Bool
lowCol col parent =
  length col == 1 &&
  let (p, Height a lv) = head col in p == parent && a == 0 && null lv

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

cleanMatrixPadding :: Expr -> Expr
cleanMatrixPadding expr =
  [ [], [(0, Height 0 [])] ] ++ [ shiftCol (expr !! i) 1 | i <- [1 .. length expr - 1] ]

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
              let threshold = head ++ [[(1, Height 0 [])]]
              Right (if cmpExpr tail threshold < 0 then rootSubstitute head tail else tail)
  | otherwise = Left "relative prefix expected"

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
                ct <- relativeToCnf' (tailTemplate expr two)
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
  where
    relativeToCnf' (Right e) = relativeToCnf e
    relativeToCnf' (Left err) = Left err

relativePower :: OrdO -> Either String Expr
relativePower exponent
  | isEps exponent = let OrEpsilon e = exponent in Right e
  | null (ordTerms exponent) = Right [[]]
  | length (ordTerms exponent) > 1 || snd (head (ordTerms exponent)) > 1 = do
      let init0 = [[]]
      foldl (\acc t -> do
                r <- acc
                let factorE = relativePower (ordPowOmega (fst t))
                factor <- factorE
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

lowTemplate :: Expr
lowTemplate = [[], [(0, Height 0 [])]]

relativeFromCnf :: OrdO -> Either String Expr
relativeFromCnf value
  | isEps value = let OrEpsilon e = value in Right e
  | otherwise = do
      let terms = ordTerms value
      blocksE <- mapM (\t -> do
          let blockE = relativePower (fst t)
          block <- blockE
          let count = snd t
          Right (concat (replicate (fromIntegral count) block)))
        terms
      Right (concatenate blocksE)

isEps :: OrdO -> Bool
isEps (OrEpsilon _) = True
isEps _ = False

ts2ToCnf :: Expr -> Either String OrdO
ts2ToCnf expr =
  if not (null expr) && not (null (head expr)) then Left "must start empty"
  else do
    m <- convertMatrix expr True
    relativeToCnf m

cnfToTs2 :: OrdO -> Either String Expr
cnfToTs2 value = do
  r <- relativeFromCnf value
  convertMatrix r False

-- ---------- compute new height ----------
data Step = StepOrdinary (Maybe Height) | StepLimit Expr

computeNewHeight :: Height -> Expr -> Int -> Step
computeNewHeight (Height anchor alpha) expr root =
  let col = expr !! anchor
      hits = [j | j <- [0 .. length col - 1],
                  cmpHeight (Height anchor alpha) (snd (col !! j)) <= 0]
   in case hits of
        (j : _) ->
          let (cjP, cjH) = col !! j
              result = Height cjP alpha
              result' = if j > 0 && cmpHeight (snd (col !! (j - 1))) result > 0
                          then snd (col !! (j - 1))
                          else result
           in StepOrdinary (Just result')
        [] ->
          if cmpExpr (height1 col) alpha < 0
            then
              if isLimit alpha
                then StepLimit alpha
                else StepOrdinary (Just (Height root (init alpha)))
            else StepOrdinary (heightOf col)

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
      e <- cnfToTs2 value
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
heightDisplay (Height anchor level) = do
  c <- ts2ToCnf level
  d <- displayOrdinal c
  Right ("(" ++ d ++ "," ++ show (anchor + 1) ++ ")")

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
             '(' -> go (i + 1) (')' : closing) acc out
             '{' -> go (i + 1) ('}' : closing) acc out
             ')' -> if null closing || head closing /= ch then Left "mismatch" else go (i + 1) (tail closing) acc out
             '}' -> if null closing || head closing /= ch then Left "mismatch" else go (i + 1) (tail closing) acc out
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
   in if null source || "Limit" `elem` splitTopPure source '+' then Left "bad ordinal"
      else do
        sum <- splitTop source '+'
        if length sum > 1
          then foldl (\acc t -> do
                        a <- acc
                        o <- parseOrdinal t
                        Right (ordAdd a o)) (Right ordZero) sum
          else do
            let prodSrc = source
            product <- splitTop prodSrc '*'
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
    -- pure splitTop for the "Limit" check (we don't care about errors there)
    splitTopPure s sep = either (const []) id (splitTop s sep)

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
              if end == length rest2 - 1 then do
                p <- parseIndex digits
                Right (p, rest2)
              else Left "bad entry"

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
      (parent, pair) <- matchEntry e
      end <- closingIdx pair 0
      if end /= length pair - 1 then Left "bad height"
      else do
        let pairInner = take (length pair - 2) (drop 1 pair)
        parts <- splitTop pairInner ','
        if length parts /= 2 then Left "bad height pair"
        else do
          anchor <- parseIndex (parts !! 1)
          levelO <- parseOrdinal (parts !! 0)
          level <- cnfToTs2 levelO
          if anchor > parent || parent >= length result
            then Left "references must point left"
            else if not (null colSoFar) &&
                    (fst (last colSoFar) <= parent ||
                     cmpHeight (snd (last colSoFar)) (Height anchor level) >= 0)
              then Left "entries must decrease"
              else Right (colSoFar ++ [(parent, Height anchor level)])

-- ---------- 展开 ----------
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
            let (root, h) = last col
                step = computeNewHeight h expr root
             in case step of
                  StepLimit alpha -> do
                    let replacement = take (length col - 1) col
                    baseLevelO <- ts2ToCnf (height1 replacement)
                    alphaO <- ts2ToCnf alpha
                    diffO <- ordTailAfter baseLevelO alphaO
                    diff <- cnfToTs2 diffO
                    approx <- fundamentalSeq diff index alter
                    newLevelO <- ts2ToCnf approx
                    newLevel <- cnfToTs2 (ordAdd baseLevelO newLevelO)
                    let replacement' = replacement ++ [(root, Height root newLevel)]
                    Right (take (length expr - 1) expr ++ [replacement'])
                  StepOrdinary mh ->
                    let replacement0 = take (length col - 1) col
                        replacement1 = case mh of
                          Just h0 -> mergeCol replacement0 [(root, h0)]
                          Nothing -> replacement0
                        replacement2 = mergeCol replacement1 (expr !! root)
                        result0 = take (length expr - 1) expr ++ [replacement2]
                        result1 = foldl addCopy result0 [1 .. index]
                        addCopy rs w = foldl (addOne w) rs [root + 1 .. right]
                        addOne w rs i = rs ++ [copyCol (rs !! i) root ((right - root) * w)]
                     in Right (if shorter then take (length result1 - 1) result1 else result1)

fundamentalSeq :: Expr -> Int -> Bool -> Either String Expr
fundamentalSeq expr index alter =
  if index < 0 then Left "bad index"
  else if not (isLimit expr)
    then Right (if null expr then expr else take (length expr - 1) expr)
    else
      let col = last expr
          (root, h) = last col
       in case computeNewHeight h expr root of
            StepLimit _ -> expandF expr index True alter
            StepOrdinary _ -> expandF expr index (not alter) alter

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
