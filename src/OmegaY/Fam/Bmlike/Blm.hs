-- | BLM —— Bashicu Large Matrix。对齐 @Header/notation/blm.hpp@。
module OmegaY.Fam.Bmlike.Blm
  ( expandText
  ) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.STRef
import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share

expandText :: String -> Int -> String
expandText text term =
  case parseBm text of
    Nothing -> kParseError
    Just e
      | matCols (beMat e) == 0 -> ""
      | otherwise ->
          colsToString (normalizeCols (blmExpand (standardizeMatrix 0 (beMat e)) term))

type Cols = [[Int]]

normalizeCols :: Cols -> Cols
normalizeCols = map trim
 where
  trim col = take (go (length col)) col
   where
    go h = if h > 0 && col !! (h - 1) == 0 then go (h - 1) else h

colsToString :: Cols -> String
colsToString = concatMap fmt
 where
  fmt col =
    let h = go (length col)
     in '(' : (if h == 0 then "0" else joinComma [ show (col !! y) | y <- [0 .. h - 1] ])
          ++ ")"
   where
    go h = if h > 0 && col !! (h - 1) == 0 then go (h - 1) else h
    joinComma [] = []
    joinComma [x] = x
    joinComma (x : xs) = x ++ "," ++ joinComma xs

blmExpand :: Matrix -> Int -> Cols
blmExpand input a = runST $ do
  let raw = matData input
      rows = matRows input
      b = [ take rows (col ++ repeat 0) | col <- raw ]
      d3 = length b - 1
      d2 = length (b !! 0) - 1
      gv cc rr
        | cc < 0 || cc > d3 = 0
        | rr < 0 || rr >= length (b !! cc) = 0
        | otherwise = (b !! cc) !! rr

  sharedArr <- newArray (0, d2) 0 :: ST s (STUArray s Int Int)
  -- b2 各列共享同一个 STUArray：b2Write 写的是同一块存储，读取结果取决于
  -- C++ 原实现的覆盖顺序。不要改成每列独立数组，否则与参考实现不一致。
  b2Ref <- newSTRef (replicate (d3 + 1) sharedArr)
  cRef <- newSTRef (replicate (d2 + 2) 0)
  c2Ref <- newSTRef (replicate (d3 + 2) 0)
  c3Ref <- newSTRef (replicate (d2 + 2) 0)
  d7Ref <- newSTRef (0 :: Int)
  d8Ref <- newSTRef (0 :: Int)
  d9Ref <- newSTRef (0 :: Int)
  d18Ref <- newSTRef (0 :: Int)
  d19Ref <- newSTRef (0 :: Int)

  let b2Read cc rr = do
        arr <- readSTRef b2Ref
        readArray (arr !! cc) rr
      b2Write cc rr v = do
        arr <- readSTRef b2Ref
        writeArray (arr !! cc) rr v

  let loopD4 d4
        | d4 > d2 = pure ()
        | otherwise =
            if b !! d3 !! d4 > 0 && gv d3 (d4 + 1) == 0
              then do
                loopD5 d4 0
                pure ()
              else loopD4 (d4 + 1)

      loopD5 d4 d5
        | d5 > d3 = pure ()
        | otherwise = do
            loopD6 d4 d5 0
            loopD5 d4 (d5 + 1)

      loopD6 d4 d5 d6
        | d6 > d4 = pure ()
        | otherwise = do
            cArr <- readSTRef cRef
            let cD6 = if d6 < length cArr then cArr !! d6 else 0
                lhs = gv (d3 - d5) d6
                rhs = b !! d3 !! d6 - cD6
            if lhs < rhs
              then do
                if d6 < d4
                  then do
                    let v = b !! d3 !! d6 - gv (d3 - d5) d6
                    writeSTRef cRef (take d6 cArr ++ [v] ++ drop (d6 + 1) cArr)
                  else do
                    d7 <- readSTRef d7Ref
                    if d7 == 0 then writeSTRef d8Ref d5 else pure ()
                    modifySTRef' d9Ref (+ 1)
                    cArr2 <- readSTRef cRef
                    let cD4 = cArr2 !! d4
                    if cD4 + 1 < rhs
                      then writeSTRef cRef
                             (take d4 cArr2 ++ [cD4 + 1] ++ drop (d4 + 1) cArr2)
                      else pure ()
                    d9 <- readSTRef d9Ref
                    c2Arr <- readSTRef c2Ref
                    let c2Arr' = if d9 >= length c2Arr then c2Arr ++ [0] else c2Arr
                        c2Arr'' = take d9 c2Arr' ++ [d5] ++ drop (d9 + 1) c2Arr'
                    writeSTRef c2Ref c2Arr''
                    mapM_ (\d10 -> b2Write (d3 - d5) d10 d9) [0 .. d4]
                    loopD11 d4 d5 0
                    loopD16 d4 d5
                    d19 <- readSTRef d19Ref
                    if d19 == 0
                      then writeSTRef d7Ref d5
                      else writeSTRef d19Ref 0
                loopD6 d4 d5 (d6 + 1)
              else
                -- C++ else: d6 = d4; 随后 for 递增退出 d6 循环
                pure ()

      loopD11 d4 d5 d11
        | d11 > d4 = pure ()
        | otherwise = do
            loopD12 d4 d5 d11 (d3 - d5 + 1)
            loopD11 d4 d5 (d11 + 1)

      loopD12 d4 d5 d11 d12
        | d12 > d3 = pure ()
        | otherwise = do
            loopD13 d4 d5 d11 d12 d12
            loopD12 d4 d5 d11 (d12 + 1)

      loopD13 d4 d5 d11 d12 d13
        | d13 < d3 - d5 = pure ()
        | otherwise = do
            _ <- loopD14 d4 d5 d11 d12 d13 0
            loopD13 d4 d5 d11 d12 (d13 - 1)

      loopD14 d4 d5 d11 d12 d13 d14
        | d14 > d11 = pure True
        | otherwise = do
            c3Arr <- readSTRef c3Ref
            let c3D14 = if d14 < length c3Arr then c3Arr !! d14 else 0
                lhs = gv d13 d14
                rhs = b !! d12 !! d14 - c3D14
            if lhs < rhs
              then do
                if d11 == d14
                  then do
                    v13 <- b2Read d13 d11
                    v12 <- b2Read d12 d11
                    d9 <- readSTRef d9Ref
                    if v13 > 0 && v12 == 0
                      then b2Write d12 d11 d9
                      else pure ()
                    pure True
                  else do
                    let v = b !! d12 !! d14 - gv d13 d14
                    writeSTRef c3Ref (take d14 c3Arr
                                        ++ [v]
                                        ++ drop (d14 + 1) c3Arr)
                    loopD14 d4 d5 d11 d12 d13 (d14 + 1)
              else pure True

      loopD16 d4 d5 = do
        d8 <- readSTRef d8Ref
        let go d16
              | d16 > d8 = pure ()
              | otherwise = do
                  broke <- loopD17 d4 d5 d16 0
                  d7 <- readSTRef d7Ref
                  if broke then go (d7 + 1) else go (d16 + 1)
        go 0

      loopD17 d4 d5 d16 d17
        | d17 > d2 = pure False
        | otherwise = do
            d8 <- readSTRef d8Ref
            writeSTRef d18Ref 0
            v1 <- b2Read (d3 - d8 + d16) d17
            d18 <-
              if v1 > 0
                then
                  if d17 < d4 + 1
                    then do
                      c2Arr <- readSTRef c2Ref
                      let srcCol = d3 - (if v1 < length c2Arr then c2Arr !! v1 else 0)
                      pure (gv srcCol d17 - gv (d3 - d5) d17)
                    else pure 0
                else pure 0
            writeSTRef d18Ref d18
            let lhs = gv (d3 - d5 + d16) d17
                rhs = gv (d3 - d8 + d16) d17 - d18
            d7 <- readSTRef d7Ref
            if lhs < rhs || (1 < d5 - d7 && 0 < d7)
              then do
                writeSTRef d19Ref 1
                modifySTRef' d9Ref (+ (-1))
                pure True
              else
                if rhs < lhs
                  then pure True
                  else loopD17 d4 d5 d16 (d17 + 1)

  loopD4 0

  -- 第二阶段：c[d20]
  cArr <- readSTRef cRef
  d7 <- readSTRef d7Ref
  let cArr2 = foldl step cArr [0 .. d2]
       where
        step arr d20 =
          if gv d3 (d20 + 1) > 0
            then take d20 arr
                   ++ [b !! d3 !! d20 - gv (d3 - d7) d20]
                   ++ drop (d20 + 1) arr
            else take d20 arr
                   ++ [b !! d3 !! d20 - gv (d3 - d7) d20 - 1]
                   ++ drop (d20 + 1) arr
  writeSTRef cRef cArr2

  resultRef <- newSTRef (take d3 b)
  d7F <- readSTRef d7Ref
  d9F <- readSTRef d9Ref
  d3Ref <- newSTRef d3

  let loopD21 d21
        | d21 > a * d7F = pure ()
        | otherwise = do
            curD3 <- readSTRef d3Ref
            res <- readSTRef resultRef
            b2Arr <- readSTRef b2Ref
            let res' = if length res <= curD3
                         then res ++ [replicate (d2 + 1) 0]
                         else res
                b2Arr' = if length b2Arr <= curD3
                           then b2Arr ++ [sharedArr]
                           else b2Arr
            writeSTRef resultRef res'
            writeSTRef b2Ref b2Arr'
            cArrF <- readSTRef cRef
            let applyD22 curCol d22 = do
                  v <- b2Read (curD3 - d7F) d22
                  let srcCol = res' !! (curD3 - d7F)
                      val = if v > 0 && v < d9F + 1
                              then srcCol !! d22 + cArrF !! d22
                              else srcCol !! d22
                  pure (take d22 curCol ++ [val] ++ drop (d22 + 1) curCol)
            zeroCol <- newSTRef (replicate (d2 + 1) 0)
            mapM_ (\d22 -> do
                      curCol <- readSTRef zeroCol
                      col' <- applyD22 curCol d22
                      writeSTRef zeroCol col'
                      arr <- readSTRef b2Ref
                      wv <- readArray (arr !! (curD3 - d7F)) d22
                      writeArray (arr !! curD3) d22 wv
                  ) [0 .. d2]
            finalCol <- readSTRef zeroCol
            writeSTRef resultRef
              (take curD3 res' ++ [finalCol] ++ drop (curD3 + 1) res')
            writeSTRef d3Ref (curD3 + 1)
            loopD21 (d21 + 1)

  loopD21 1
  readSTRef resultRef
