-- | iblp 记号（id=iblp），逐行对齐 C++ @Header/notation/iblp.hpp@。
module OmegaY.Fam.Den.Iblp
  ( expandText
  , expandTextAlter
  ) where

import OmegaY.Fam.Den.Share

-- 类型与显示

type Den3Row = [DenEntry]

data Den3Expr = Den3Expr
  { d3Rows :: [Den3Row]
  }

entryStr :: DenEntry -> String
entryStr e = (if deMarked e then "*" else "") ++ show (deValue e)

toString :: Den3Expr -> String
toString e = concatMap rowStr (d3Rows e)
 where
  rowStr row =
    "(" ++ joinComma (map entryStr (drop 1 row)) ++ ")" ++ show (deValue (head row))
  joinComma [] = ""
  joinComma [x] = x
  joinComma (x : xs) = x ++ "," ++ joinComma xs

-- 解析

parse :: String -> Maybe Den3Expr
parse s0 = do
  let s1 = skipSpaces s0
  loop s1 []
 where
  loop s acc =
    let s1 = skipSpaces s
     in if null s1
          then Just (Den3Expr acc)
          else
            if peek s1 /= '('
              then Nothing
              else do
                let s2 = skipSpaces (drop 1 s1)
                (entries, s3) <- parseEntries s2 []
                if null s3 || peek s3 /= ')'
                  then Nothing
                  else do
                    let s4 = skipSpaces (drop 1 s3)
                    (step, s5) <- parseDigits s4
                    loop s5 (acc ++ [DenEntry step False : entries])
  -- 括号内允许为空。
  parseEntries s acc
    | null s || peek s == ')' = Just (acc, s)
    | otherwise = do
        let marked = peek s == '*'
            s1 = if marked then drop 1 s else s
        (v, s2) <- parseDigits s1
        let s3 = skipSpaces s2
        if peek s3 == ','
          then parseEntries (skipSpaces (drop 1 s3)) (acc ++ [DenEntry v marked])
          else Just (acc ++ [DenEntry v marked], s3)

-- 展开核心

cut :: Den3Expr -> Den3Expr
cut raw
  | null (d3Rows raw) = raw
  | otherwise = Den3Expr (init (d3Rows raw))

data SeqItem = SeqItem
  { siRow :: !Int
  , siIdx :: !Int
  }

-- | TS: seqFrom —— 返回 Nothing 表示无定义。
seqFrom :: Den3Expr -> Int -> Int -> Maybe [SeqItem]
seqFrom expr i j =
  let row = (d3Rows expr) !! i
   in if j < 0 || j >= length row
        then Nothing
        else
          let val0 = deValue (row !! j)
              step = deValue (head row)
              threshold =
                if j + step >= 0 && j + step < length row
                  then deValue (row !! (j + step))
                  else 0
           in if threshold == 0
                then Nothing
                else
                  let start = [SeqItem (i + 1) j, SeqItem val0 0]
                      rec = walk start threshold
                      v = siRow (last rec)
                   in if v /= threshold
                        then Nothing
                        else Just (init (tail rec))
 where
  walk rec thr =
    let v = siRow (last rec)
     in if v > thr
          then
            if v - 1 < 0 || v - 1 >= length (d3Rows expr)
              then rec
              else
                let r2 = (d3Rows expr) !! (v - 1)
                    idx = 1 + deValue (head r2)
                    rec1 = init rec ++ [SeqItem (siRow (last rec)) idx]
                    v' =
                      if idx >= 0 && idx < length r2
                        then deValue (r2 !! idx)
                        else 0
                 in walk (rec1 ++ [SeqItem v' 0]) thr
          else rec

applyRow :: Den3Row -> Den3Row -> Den3Row
applyRow s t =
  let sVals = map deValue (drop 1 s)
      tVals = map deValue (drop 1 t)
      step = deValue (head t)
   in DenEntry (deValue (head s)) False
        : [DenEntry (apValue x tVals step) False | x <- sVals]

copy :: Den3Expr -> Int -> Den3Expr
copy raw flag =
  let active = last (d3Rows raw)
      expr = cut raw
      begin = deValue (active !! (1 + deValue (head active)))
      end = if flag >= 0 then begin + flag else length (d3Rows raw) + 1
      offset = length (d3Rows raw) - begin
      a1 = deValue (last active)
      copied =
        [ applyRow ((d3Rows raw) !! i) active | i <- [begin - 1 .. end - 2] ]
      rows1 = d3Rows expr ++ copied
      rows2 =
        foldl
          ( \rows i ->
              let row = (d3Rows raw) !! i
                  targetIdx = i + offset
                  target = rows !! targetIdx
                  newTarget = migrate rows active row target i offset begin a1
               in setAt targetIdx newTarget rows
          )
          rows1
          [begin - 1 .. end - 2]
   in Den3Expr rows2
 where
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]
  setMarked row j =
    [ if k == j then e { deMarked = True } else e | (k, e) <- zip [0 ..] row ]
  -- 在新表达式 rows 上处理目标行 target（目标行下标 = i+offset）
  migrate rows active row target i offset begin a1 =
    let iTarget = i + offset
     in foldl (go iTarget) target (zip [0 ..] row)
   where
    go iTarget acc (j, e) =
      if not (deMarked e)
        then acc
        else
          case seqFrom (Den3Expr rows) iTarget j of
            Nothing -> acc
            Just seq ->
              let nomove =
                    case
                      [ k | k <- [0 .. length seq - 1], siRow (seq !! k) < begin ]
                    of
                      (k : _) -> k
                      [] -> (-1)
               in if nomove == (-1)
                    then setMarked acc j
                    else
                      let y0 = siRow (seq !! nomove)
                       in if y0 < a1
                            then setMarked acc j
                            else
                              let k =
                                    case
                                      [ e
                                      | e <- [1 .. length active - 1]
                                      , deValue (active !! e) == y0
                                      ]
                                    of
                                      (e : _) -> e
                                      [] -> 0
                                  kk = k - deValue (head active)
                                  activeMarked =
                                    kk >= 1 && kk < length active
                                      && deMarked (active !! kk)
                                  tj = j + deValue (head target) - 1
                                  lhs =
                                    tj < 0 || tj >= length target
                                      || deValue (target !! tj) <= a1
                               in if activeMarked && lhs then setMarked acc j else acc

compTo :: Den3Expr -> Int -> [Maybe [Int]] -> Den3Expr
compTo raw r rec =
  let row = (d3Rows raw) !! r
      entries = drop 1 row
      newEntries =
        foldl
          ( \acc i ->
              if not (deMarked (row !! i))
                then acc
                else
                  let bi = deValue (row !! i)
                   in case seqFrom raw r i of
                        Nothing -> acc
                        Just seq ->
                          let t = siRow (last seq)
                           in if t - 1 < 0 || t - 1 >= length rec || rec !! (t - 1) == Nothing
                                then acc
                                else
                                  let tVec = maybe [] id (rec !! (t - 1))
                                      q = length tVec
                                      acc1 = acc ++ [DenEntry x False | x <- tVec]
                                      acc2 =
                                        acc1 ++ [DenEntry (bi + 1 + uu) True | uu <- [0 .. q - 1]]
                                   in sortEntriesDesc acc2
          )
          entries
          [length row - 1, length row - 2 .. 1]
      q = length newEntries - length entries
      nrow = DenEntry (deValue (head row) + q) False : newEntries
   in setRow r nrow raw
 where
  setRow idx nrow expr =
    Den3Expr
      [ if k == idx then nrow else r | (k, r) <- zip [0 ..] (d3Rows expr) ]

compFrom :: Den3Expr -> Int -> [Int] -> Den3Expr
compFrom raw r t =
  let q = length t
      rr = (d3Rows raw) !! r
      firstVal = if length rr > 1 then deValue (rr !! 1) else 0
      shortRow = length rr < deValue (head rr) * 2 + 1
      lr = if shortRow then deValue (head rr) else deValue (head rr) + 1
      cr =
        if shortRow
          then appendSliceExceptTail rr 1 (deValue (head rr))
                 ++ appendSliceFrom rr (1 + deValue (head rr))
          else appendSliceFrom rr 1
      makeRow step entries = DenEntry step False : sortEntriesDesc entries
      newRows =
        [ makeRow (lr + qq)
            ( cr
                ++ [DenEntry (t !! k) False | k <- [0 .. qq]]
                ++ [DenEntry (firstVal + 1 + uu) False | uu <- [0 .. qq - 1]]
            )
        | qq <- [0 .. q - 1]
        ]
      origRow =
        makeRow (deValue (head rr) + q)
          ( drop 1 rr
              ++ [DenEntry x False | x <- t]
              ++ [DenEntry (firstVal + 1 + uu) False | uu <- [0 .. q - 1]]
          )
      rows1 = take r (d3Rows raw) ++ newRows ++ [origRow]
      rows2 =
        foldl
          ( \rows qq ->
              let idx = r + qq
                  row = rows !! idx
                  row1 = markUpTo row 2 (1 + qq)
               in setAt idx row1 rows
          )
          rows1
          [1 .. q]
      rows3 =
        rows2
          ++ [ nrow
             | ii <- [r + 1 .. length (d3Rows raw) - 1]
             , let src = (d3Rows raw) !! ii
             , let nrow =
                       head src
                         : [ if deValue e > firstVal then e { deValue = deValue e + q } else e
                           | e <- drop 1 src
                           ]
             ]
   in Den3Expr rows3
 where
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]
  markUpTo row from upTo =
    [ if k >= from && k <= upTo then e { deMarked = True } else e
    | (k, e) <- zip [0 ..] row
    ]

expand :: Den3Expr -> Int -> Bool -> Den3Expr
expand raw fsTerm longer
  | null (d3Rows raw) = raw
  | otherwise =
      let active = last (d3Rows raw)
          idx = 1 + deValue (head active)
       in if idx >= length active || deValue (active !! idx) == 0
            then cut raw
            else
              let begin = deValue (active !! idx)
                  rows =
                    [ map deValue (drop 1 ((d3Rows raw) !! i))
                    | i <- [begin - 1 .. length (d3Rows raw) - 2]
                    ]
                  tcheck = map deValue (drop idx active)
                  flag = pleasantUntil rows tcheck
                  expr0 = raw
                  (expr1, len0) =
                    if flag >= 0
                      then (copy expr0 flag, -1)
                      else
                        let e0 = foldl (\e _ -> copy e (-1)) expr0 [1 .. fsTerm]
                         in if longer
                              then (copy e0 1, length (d3Rows e0))
                              else (cut e0, -1)
                  -- while 循环（C++ 手动 r += |T|）
                  loop (expr, rec) r
                    | r >= length (d3Rows expr) = (expr, rec)
                    | otherwise =
                        let e1 = compTo expr r rec
                            row = (d3Rows e1) !! r
                         in if not (length row <= deValue (head row) * 2 + 1)
                              then loop (e1, rec) (r + 1)
                              else
                                let step = deValue (head row)
                                    pr =
                                      if 1 + step < length row
                                        then deValue (row !! (1 + step))
                                        else 0
                                    t0 =
                                      [ if step < length row
                                          then deValue (row !! step)
                                          else 0
                                      ]
                                    t1 = buildT e1 row t0 pr
                                    t2 = if null t1 then [] else tail t1
                                    t3 = if null t2 then [] else init t2
                                 in if null t3
                                      then loop (e1, rec) (r + 1)
                                      else
                                        let rec1 =
                                              if r >= length rec
                                                then rec ++ replicate (r + 1 - length rec) Nothing
                                                else rec
                                            rec2 = setAt r (Just t3) rec1
                                            e2 = compFrom e1 r t3
                                         in loop (e2, rec2) (r + length t3)
                  (expr2, _) = loop (expr1, []) (length (d3Rows raw) - 1)
                  final =
                    if longer && len0 >= 0
                      then Den3Expr (take len0 (d3Rows expr2))
                      else expr2
               in final
 where
  buildT e row t pr =
    let rowIdx = head t - 1
        v =
          if rowIdx >= 0 && rowIdx < length (d3Rows e) && length ((d3Rows e) !! rowIdx) > 2
            then deValue (((d3Rows e) !! rowIdx) !! 2)
            else 0
        t' = v : t
     in if head t' > pr then buildT e row t' pr else t'
  setAt idx v xs = [ if k == idx then v else x | (k, x) <- zip [0 ..] xs ]

-- 极限

-- 公共入口

expandTextImpl :: String -> Int -> Bool -> String
expandTextImpl text term longer =
  let term' = max 1 term
   in case parse text of
        Nothing -> "解析失败，请确认你没有使用全角括号，逗号"
        Just e
          | null (d3Rows e) -> toString (Den3Expr [])
          | otherwise -> toString (expand e term' longer)

expandText :: String -> Int -> String
expandText text term = expandTextImpl text term False

expandTextAlter :: String -> Int -> String
expandTextAlter text term = expandTextImpl text term True
