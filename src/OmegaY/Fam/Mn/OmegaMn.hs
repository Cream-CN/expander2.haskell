-- | ωMN：基于 Mountain（value + int sep），但 expand 与 nMN 不同。
module OmegaY.Fam.Mn.OmegaMn
  ( omegaMnNotation
  ) where

import OmegaY.Notation.Types (Notation, textAlterNotation)
import OmegaY.Fam.Mn.SmnShare

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

-- | omega_mn_detail::from_display：与 smnshare 版差异在 unparenthesized '0' 分支也接受 ','。
fromDisplayOmega :: String -> Maybe Mountain
fromDisplayOmega s0 = go s0 0
  where
    go s i
      | i >= length s = Just []
      | otherwise =
          let i1 = skipSpaces s i
           in if i1 >= length s
                then Just []
                else
                  let col = if s !! i1 == '(' then parseParenCol s i1 else parseUnparenCol s i1
                   in case col of
                        Nothing -> Nothing
                        Just (c, i2) ->
                          let i3 = skipSpaces s i2
                           in (c :) <$> go s i3
    skipSpaces s i = let n = length s in if i < n && s !! i == ' ' then skipSpaces s (i + 1) else i
    skipIndex s i =
      let n = length s
       in if i < n && s !! i == ':'
            then let i1 = i + 1
                     i2 = skipSpaces s i1
                  in walk i2
            else i
      where walk i = let n = length s in if i < n && isDigitC (s !! i) then walk (i + 1) else i
    parseSep s i =
      let n = length s
          walk count k = if k < n && s !! k == ',' then walk (count + 1) (k + 1) else (count, k)
          (cnt, i1) = walk 0 i
       in (if cnt == 0 then 0 else cnt - 1, i1)
    parseNumber s i =
      let n = length s
          start = i
       in if i >= n || not (isDigitC (s !! i)) then Nothing
          else
            let j = walk i
                walk k = if k < n && isDigitC (s !! k) then walk (k + 1) else k
                digits = take (j - start) (drop start s)
                v = foldl (\a d -> a * 10 + digitC d) 0 digits
             in if v > 1000000 then Nothing else Just (v, j)
    isDigitC c = c >= '0' && c <= '9'
    digitC c = fromEnum c - fromEnum '0'
    parseParenCol s i =
      let i1 = i + 1
       in entries s i1
      where
        entries s k =
          let n = length s
              k2 = skipSpaces s k
           in if k2 >= n || s !! k2 == ')' || s !! k2 == ':'
                then
                  let k3 = skipIndex s k2
                      k4 = skipSpaces s k3
                   in if k4 >= n || s !! k4 /= ')' then Nothing
                      else Just ([], k4 + 1)
                else
                  let k5 = skipSpaces s k2
                      (sep, k6) = parseSep s k5
                      k7 = skipSpaces s k6
                   in case parseNumber s k7 of
                        Nothing -> Nothing
                        Just (v, k8) ->
                          let k9 = skipSpaces s k8
                           in case entries s k9 of
                                Nothing -> Nothing
                                Just (rest, end) -> Just (Entry v sep : rest, end)
    parseUnparenCol s i =
      let n = length s
          i1 = skipSpaces s i
       in if i1 >= n then Nothing
          else
            let c = s !! i1
             in if c == '0' &&
                   (i1 + 1 >= n || s !! (i1 + 1) == ':' || s !! (i1 + 1) == ' ' ||
                    s !! (i1 + 1) == '(' || s !! (i1 + 1) == ',')
                  then let k1 = i1 + 1
                           k2 = skipIndex s k1
                        in Just ([], k2)
                  else entries s i1
      where
        entries s k =
          let n = length s
           in if k >= n || s !! k == ' ' || s !! k == '(' || s !! k == ':'
                then let k1 = skipIndex s k in Just ([], k1)
                else
                  let c = s !! k
                   in if c /= ',' then Nothing
                      else
                        let (sep, k1) = parseSep s k
                            k2 = skipSpaces s k1
                         in case parseNumber s k2 of
                              Nothing -> Nothing
                              Just (v, k3) ->
                                case entries s k3 of
                                  Nothing -> Nothing
                                  Just (rest, end) -> Just (Entry v sep : rest, end)

isLimitMountain :: Mountain -> Bool
isLimitMountain m = not (null m) && not (null (last m))


-- | omega_mn_detail::expand。
expandOmega :: Mountain -> Int -> Bool -> Mountain
expandOmega m0 index shorter =
  let rightmost = length m0 - 1
      topmost = length (m0 !! rightmost) - 1
   in if topmost == -1
        then init m0
        else
          let trSep = eSep ((m0 !! rightmost) !! topmost)
              v0 = columnVerticals `map` m0
              brij = parent m0 v0 (Position rightmost topmost)
              width = rightmost - pI brij
              topLen = min (if pJ brij < 0 then 0 else pJ brij) (length (v0 !! pI brij))
              topVerticals = take topLen (v0 !! pI brij) ++ [(v0 !! rightmost) !! topmost]
              m1 = adjustRightmost m0 trSep v0 brij topmost rightmost
              v1 = columnVerticals `map` m1
              magmaChecks = magmaChecksList m1 v1 brij rightmost
              m2 = expandLoop m1 v1 brij width topVerticals magmaChecks index rightmost
           in if shorter then init m2 else m2

-- Step: adjust rightmost column after removing/replacing top entry.
adjustRightmost :: Mountain -> Int -> [[Vertical]] -> Position -> Int -> Int -> Mountain
adjustRightmost m0 trSep v0 brij topmost rightmost =
  let brI = pI brij; brJ = pJ brij
      -- C++ 先改 m[rightmost]，再取 br_col = m[BRij.i] 做切片拼接；
      -- 当 BRij.i == rightmost 时读到的是**改过的**末列，不是原始列。
      newRight = if trSep == 0
                   then take topmost (m0 !! rightmost)
                   else
                     let newTrSep = trSep - 1
                         vParent = if brJ - 1 < 0 || brJ - 1 >= length (v0 !! brI)
                                     then []
                                     else (v0 !! brI) !! (brJ - 1)
                         vBottom = if topmost - 1 < 0 then [] else (v0 !! rightmost) !! (topmost - 1)
                      in if verticalCompare (verticalIncrease vParent newTrSep) vBottom <= 0
                           then take topmost (m0 !! rightmost)
                           else
                             let old = m0 !! rightmost
                                 topEntry = old !! topmost
                              in take topmost old ++ [topEntry { eSep = newTrSep }] ++ drop (topmost + 1) old
      brCol = if brI == rightmost then newRight else m0 !! brI
      tailCol = if brJ >= 0 && brJ < length brCol then drop brJ brCol else []
   in take rightmost m0 ++ [newRight ++ tailCol] ++ drop (rightmost + 1) m0

-- | 计算 magma_checks_list。
magmaChecksList :: Mountain -> [[Vertical]] -> Position -> Int -> [[Int]]
magmaChecksList m v brij rightmost =
  let brI = pI brij; brJ = pJ brij
      computeJ i j =
        let (wi, wj) = walk i j
         in if wi == brI && wj <= brJ &&
               verticalCompare (if wi == -1 || wj - 1 < 0 then [] else (v !! wi) !! (wj - 1))
                                (if j == 0 then [] else (v !! i) !! (j - 1)) == 0
              then wj
              else -1
        where
          walk wi wj
            | wi <= brI = (wi, wj)
            | otherwise =
                let wj' = if length (m !! wi) <= wj then wj - 1 else wj
                 in if wj' < 0 then (-1, 0)
                    else
                      let p = parent m v (Position wi wj')
                       in walk (pI p) (pJ p)
      go i acc
        | i > rightmost = acc
        | otherwise =
            let row = [computeJ i j | j <- [0 .. length (m !! i) - 1]]
                acc' = take i acc ++ [row] ++ drop (i + 1) acc
             in go (i + 1) acc'
   in go (brI + 1) [[] | _ <- [0 .. rightmost]]

getReferences :: Mountain -> [Vertical] -> [Int]
getReferences m rTops =
  let verticals = columnVerticals (last m)
      verticals' = [] : verticals
      ref = map (const (-1)) rTops
      -- C++ 顺序执行 ref[j]=i; ++i，同一 j 后写的值覆盖先写的；
      -- 必须前向累积，不能在递归返回途中回填（那样先写的会覆盖后写的）。
      go i j rs
        | i >= length verticals' || j >= length rTops = rs
        | otherwise =
            if verticalCompare (verticals' !! i) (rTops !! j) < 0
              then go (i + 1) j (updateRef j i rs)
              else go i (j + 1) rs
      updateRef idx val rs = take idx rs ++ [val] ++ drop (idx + 1) rs
   in go 0 0 ref

expandLoop :: Mountain -> [[Vertical]] -> Position -> Int -> [Vertical] -> [[Int]] -> Int -> Int -> Mountain
expandLoop m0 v brij width topVerticals magmaChecks index rightmost =
  let brI = pI brij; brJ = pJ brij
      goN n m
        | n > index = m
        | otherwise =
            let refs = getReferences m topVerticals
                refAt k = if k == -1 then -1 else refs !! k
                targetEnd = brI + width * n + width + 1
                m1 = if length m < targetEnd then m ++ [[] | _ <- [length m .. targetEnd - 1]] else m
                goDx dx m'
                  | dx > width = m'
                  | otherwise =
                      let x = brI + dx
                          sourceMagmas = magmaChecks !! x
                          targetCol = buildColumn x n m' sourceMagmas refAt
                          m'' = take (x + width * n) m' ++ [targetCol] ++ drop (x + width * n + 1) m'
                       in goDx (dx + 1) m''
                m2 = goDx 1 m1
             in goN (n + 1) m2
      buildColumn x n m' sourceMagmas refAt =
        let col = m' !! x
         in concatMap (\(y, entry) ->
                          let value = eValue entry
                              sm = if y < length sourceMagmas then sourceMagmas !! y else -1
                           in if sm >= 0
                                then
                                  let brIndex = sm
                                      lo = refAt (brIndex - 1) + 1
                                      hi = refAt brIndex
                                   in [ if j == hi
                                          then Entry (value + width * n) (eSep entry)
                                          else Entry (value + width * n)
                                                      (eSep ((m' !! (brI + width * n)) !! j))
                                      | j <- [lo .. hi] ]
                                else
                                  [Entry (value + (if value > brI then width * n else 0)) (eSep entry)]
                      ) (zip [0..] col)
   in goN 1 m0

expandImpl :: Bool -> String -> Int -> String
expandImpl longer text term0 =
  let term = max 1 term0
      raw = trimWS text
   in if null raw || raw == "0" || raw == "∅"
          then ""
          else case fromDisplayOmega raw of
                 Nothing -> errText
                 Just m
                   | null m -> ""
                   | not (isLimitMountain m) -> mountainDisplay (init m) False
                   | otherwise -> mountainDisplay (expandOmega m term (not longer)) False

trimWS :: String -> String
trimWS = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

omegaMnNotation :: Notation
omegaMnNotation =
  textAlterNotation "omega-mn" "ωMN" []
    (expandImpl False)
    (expandImpl True)
    ""
