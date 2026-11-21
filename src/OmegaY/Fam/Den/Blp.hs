-- | blp 记号（id=blp），逐行对齐 C++ @Header/notation/blp.hpp@。
module OmegaY.Fam.Den.Blp
  ( expandText
  , expandTextAlter
  ) where

import OmegaY.Fam.Den.Share

-- ---------------------------------------------------------------------
-- 类型与显示
-- ---------------------------------------------------------------------

data DenRow = DenRow
  { drStep :: !Int
  , drEntries :: [DenEntry]
  }

data DenExpr = DenExpr
  { deRows :: [DenRow]
  }

entryStr :: DenEntry -> String
entryStr e = (if deMarked e then "*" else "") ++ show (deValue e)

toString :: DenExpr -> String
toString e = concatMap rowStr (deRows e)
 where
  rowStr row =
    "(" ++ joinComma (map entryStr (drEntries row)) ++ ")" ++ show (drStep row)
  joinComma [] = ""
  joinComma [x] = x
  joinComma (x : xs) = x ++ "," ++ joinComma xs

-- ---------------------------------------------------------------------
-- 解析
-- ---------------------------------------------------------------------

parse :: String -> Maybe DenExpr
parse s0 = do
  let s1 = skipSpaces s0
  loop s1 []
 where
  loop s acc =
    let s1 = skipSpaces s
     in if null s1
          then Just (DenExpr acc)
          else
            if peek s1 /= '('
              then Nothing
              else do
                let s2 = skipSpaces (drop 1 s1)
                if null s2 || peek s2 == ')'
                  then Nothing -- 括号内不允许为空
                  else do
                    (row, s3) <- parseRow s2
                    loop s3 (acc ++ [row])
  parseRow s = do
    (entries, s1) <- parseEntries s []
    if null s1 || peek s1 /= ')'
      then Nothing
      else do
        let s2 = skipSpaces (drop 1 s1)
        (step, s3) <- parseDigits s2
        Just (DenRow step entries, s3)
  parseEntries s acc = do
    let marked = peek s == '*'
        s1 = if marked then drop 1 s else s
    (v, s2) <- parseDigits s1
    let s3 = skipSpaces s2
    if peek s3 == ','
      then parseEntries (skipSpaces (drop 1 s3)) (acc ++ [DenEntry v marked])
      else Just (acc ++ [DenEntry v marked], s3)

-- ---------------------------------------------------------------------
-- 展开核心
-- ---------------------------------------------------------------------

-- | 删除最后一行。
cut :: DenExpr -> DenExpr
cut raw
  | null (deRows raw) = raw
  | otherwise = DenExpr (init (deRows raw))

data SeqItem = SeqItem
  { siRow :: !Int
  , siIdx :: !Int
  }

-- | TS: seqFrom —— 从 (i,j) 沿 parent 链收集。
seqFrom :: DenExpr -> Int -> Int -> [SeqItem]
seqFrom expr i j =
  let row = (deRows expr) !! i
      es = drEntries row
      val0 = deValue (es !! j)
      threshold =
        if j + drStep row < length es then deValue (es !! (j + drStep row)) else 0
      start = [SeqItem (i + 1) j, SeqItem val0 0]
      rec = walk start threshold
   in init rec
 where
  walk rec thr =
    let v = siRow (last rec)
     in if v > thr
          then
            if v - 1 < 0 || v - 1 >= length (deRows expr)
              then rec
              else
                let r2 = (deRows expr) !! (v - 1)
                    idx = drStep r2
                    rec1 = init rec ++ [SeqItem (siRow (last rec)) idx]
                    v' =
                      if idx >= 0 && idx < length (drEntries r2)
                        then deValue ((drEntries r2) !! idx)
                        else 0
                 in walk (rec1 ++ [SeqItem v' 0]) thr
          else rec

-- | 按 active 行的映射规则复制一行。
applyRow :: DenRow -> DenRow -> DenRow
applyRow s t =
  let sVals = rowValuesE (drEntries s)
      tVals = rowValuesE (drEntries t)
   in DenRow (drStep s)
        [ DenEntry (apValue x tVals (drStep t)) False | x <- sVals ]

-- | 复制 [begin-1, end-1) 区间行并迁移标记。
copy :: DenExpr -> Int -> DenExpr
copy raw flag =
  let active = last (deRows raw)
      expr = cut raw
      begin = atE (drEntries active) (drStep active)
      end = if flag >= 0 then begin + flag else length (deRows raw) + 1
      offset = length (deRows raw) - begin
      lastActive = deValue (last (drEntries active))
      copied =
        [ applyRow ((deRows raw) !! i) active | i <- [begin - 1 .. end - 2] ]
      rows1 = deRows expr ++ copied
      -- 逐行迁移标记
      rows2 =
        foldl
          ( \rows i ->
              let row = (deRows raw) !! i
                  targetIdx = i + offset
                  target = rows !! targetIdx
                  newEntries =
                    migrate rows begin offset lastActive
                      (drEntries row) (drEntries target) (drStep target) i
               in setAt targetIdx target { drEntries = newEntries } rows
          )
          rows1
          [begin - 1 .. end - 2]
   in DenExpr rows2
 where
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]
  migrate newRows begin offset lastActive es targetEs targetStep i =
    foldl
      ( \acc (j, e) ->
          if not (deMarked e)
            then acc
            else
              let seq = seqFrom raw i j
                  noMove =
                    case [ k | k <- [0 .. length seq - 1], siRow (seq !! k) < begin ] of
                      (k : _) -> k
                      [] -> (-1)
               in if noMove == (-1)
                    then setMarked acc j
                    else
                      if siRow (seq !! noMove) < lastActive
                        then setMarked acc j
                        else
                          let c = siRow (seq !! (noMove - 1)) + offset
                           in if c - 1 < 0 || c - 1 >= length newRows
                                then acc
                                else
                                  let rowC = newRows !! (c - 1)
                                      bIdx = siIdx (seq !! (noMove - 1))
                                      b =
                                        if bIdx >= 0 && bIdx < length (drEntries rowC)
                                          then deValue ((drEntries rowC) !! bIdx)
                                          else 0
                                      tj = j + targetStep - 1
                                      lhs =
                                        tj >= 0 && tj < length targetEs
                                          && deValue (targetEs !! tj) <= lastActive
                                      rhs = any (\e -> deValue e == b && deMarked e) activeEs
                                   in if lhs && rhs then setMarked acc j else acc
      )
      targetEs
      (zip [0 ..] es)
   where
    activeEs = drEntries (last (deRows raw))
  setMarked es j = [ if k == j then e { deMarked = True } else e | (k, e) <- zip [0 ..] es ]

-- | compTo：对第 r 行做上向压缩。
compTo :: DenExpr -> Int -> [Maybe [Int]] -> DenExpr
compTo raw r already =
  let row = (deRows raw) !! r
      es = drEntries row
      newEs =
        foldl
          ( \acc j ->
              if not (deMarked (es !! j))
                then acc
                else
                  let n = deValue (es !! j)
                      seq = seqFrom raw r j
                      t = siRow (last seq)
                   in if t - 1 < 0 || t - 1 >= length already || already !! (t - 1) == Nothing
                        then acc
                        else
                          let tVec = maybe [] id (already !! (t - 1))
                              q = length tVec
                              acc1 = acc ++ [DenEntry x False | x <- tVec]
                              acc2 = acc1 ++ [DenEntry (n + 1 + k) True | k <- [0 .. q - 1]]
                           in sortEntriesDesc acc2
          )
          es
          [length es - 1, length es - 2 .. 0]
      added = (length newEs - length es) `div` 1
   in setRow r row { drStep = drStep row + added, drEntries = newEs } raw
 where
  setRow idx row expr =
    DenExpr
      [ if k == idx then row else r | (k, r) <- zip [0 ..] (deRows expr) ]

-- | compFrom：以第 r 行为模板、T 为系数构造 q+1 行并平移后续行。
compFrom :: DenExpr -> Int -> [Int] -> DenExpr
compFrom raw r t =
  let q = length t
      rr = (deRows raw) !! r
      es = drEntries rr
      firstVal = if null es then 0 else deValue (head es)
      shortRow = length es < drStep rr * 2
      lr = if shortRow then drStep rr else drStep rr + 1
      cr =
        if shortRow
          then appendSliceExceptTail es 0 (drStep rr) ++ appendSliceFrom es (drStep rr)
          else es
      makeRow step entries = DenRow step (sortEntriesDesc entries)
      newRows =
        [ makeRow (lr + l)
            ( cr
                ++ [DenEntry (t !! k) False | k <- [0 .. l]]
                ++ [DenEntry (firstVal + 1 + k) False | k <- [0 .. l - 1]]
            )
        | l <- [0 .. q - 1]
        ]
      origRow =
        makeRow (drStep rr + q)
          ( es
              ++ [DenEntry x False | x <- t]
              ++ [DenEntry (firstVal + 1 + k) False | k <- [0 .. q - 1]]
          )
      rows1 = take r (deRows raw) ++ newRows ++ [origRow]
      rows2 =
        foldl
          ( \rows l ->
              let idx = r + l
                  row = rows !! idx
                  es1 = markUpTo (drEntries row) 1 l
               in setAt idx row { drEntries = es1 } rows
          )
          rows1
          [1 .. q]
      rows3 =
        rows2
          ++ [ nrow
             | ii <- [r + 1 .. length (deRows raw) - 1]
             , let src = (deRows raw) !! ii
             , let nrow =
                     DenRow (drStep src)
                       [ if deValue e > firstVal then e { deValue = deValue e + q } else e
                       | e <- drEntries src
                       ]
             ]
   in DenExpr rows3
 where
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]
  markUpTo es from upTo =
    [ if k >= from && k <= upTo then e { deMarked = True } else e
    | (k, e) <- zip [0 ..] es
    ]

-- | 核心展开。shorter=False 对应 FS，True 对应 FS_alter。
expand :: DenExpr -> Int -> Bool -> DenExpr
expand raw index shorter
  | null (deRows raw) = raw
  | otherwise =
      let active = last (deRows raw)
          es = drEntries active
       in if drStep active >= length es || deValue (es !! drStep active) == 0
            then cut raw
            else
              let begin = deValue (es !! drStep active)
                  rows =
                    [ rowValuesE (drEntries ((deRows raw) !! i))
                    | i <- [begin - 1 .. length (deRows raw) - 2]
                    ]
                  tcheck = map deValue (drop (drStep active) es)
                  flag = pleasantUntil rows tcheck
                  expr0 = raw
                  expr1 =
                    if flag >= 0
                      then copy expr0 flag
                      else
                        let e = foldl (\e _ -> copy e (-1)) expr0 [1 .. index]
                         in if shorter then cut e else copy e 1
                  already0 = replicate (length (deRows expr1) + 8) Nothing
                  (expr2, _) =
                    foldl
                      ( \st r ->
                          let (expr, already) = st
                              e1 = compTo expr r already
                              row = (deRows e1) !! r
                           in if not (length (drEntries row) <= drStep row * 2)
                                then (e1, already)
                                else
                                  let step = drStep row
                                      es1 = drEntries row
                                      t0 =
                                        [ if step - 1 >= 0 && step - 1 < length es1
                                            then deValue (es1 !! (step - 1))
                                            else 0
                                        ]
                                      t1 = buildT e1 step es1 t0
                                      t2 = if null t1 then [] else tail t1
                                      t3 = if null t2 then [] else init t2
                                   in if null t3
                                        then (e1, already)
                                        else
                                          let e2 = compFrom e1 r t3
                                              already1 =
                                                if r >= length already
                                                  then already ++ replicate (r + 1 - length already) Nothing
                                                  else already
                                              already2 = setAt r (Just t3) already1
                                           in (e2, already2)
                      )
                      (expr1, already0)
                      [length (deRows raw) - 1 .. length (deRows expr1) - 1]
               in expr2
 where
  buildT e step es t =
    let rowIdx = head t - 1
        v =
          if rowIdx >= 0 && rowIdx < length (deRows e)
            then
              let r2 = (deRows e) !! rowIdx
                  es2 = drEntries r2
               in if 1 < length es2 then deValue (es2 !! 1) else 0
            else 0
        t' = v : t
        th =
          if step < length es then deValue (es !! step) else 0
     in if head t' > th then buildT e step es t' else t'
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]

-- ---------------------------------------------------------------------
-- 极限
-- ---------------------------------------------------------------------

limitRow :: Int -> DenRow
limitRow n =
  DenRow 2
    [ DenEntry nn (3 <= nn && nn < 2 + n) | nn <- [2 + n, 2 + n - 1 .. 0] ]


-- | 极限判定。
isLimit :: DenExpr -> Bool
isLimit e
  | null (deRows e) = False
  | otherwise =
      let active = last (deRows e)
          es = drEntries active
       in if drStep active >= length es || deValue (es !! drStep active) == 0
            then False
            else
              let begin = deValue (es !! drStep active)
                  rows =
                    [ rowValuesE (drEntries ((deRows e) !! i))
                    | i <- [begin - 1 .. length (deRows e) - 2]
                    ]
                  tcheck = map deValue (drop (drStep active) es)
               in pleasantUntil rows tcheck == (-1)

-- ---------------------------------------------------------------------
-- 公共入口
-- ---------------------------------------------------------------------

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term shorter =
  let term' = max 1 term
   in case parse text of
        Nothing -> "解析失败，请确认你没有使用全角括号，逗号"
        Just e
          | null (deRows e) -> toString (DenExpr [])
          | not (isLimit e) -> toString (cut e)
          | otherwise -> toString (expand e term' shorter)

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term True

expandTextAlter :: String -> Int -> String
expandTextAlter text term = expandTextImpl text term False
