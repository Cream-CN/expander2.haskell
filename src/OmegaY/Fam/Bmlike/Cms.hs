{-# LANGUAGE BangPatterns #-}

-- | CMS —— 变长列矩阵记号。
module OmegaY.Fam.Bmlike.Cms
  ( expandText
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import OmegaY.Matrix
import OmegaY.Fam.Bmlike.Share

expandText :: String -> Int -> String
expandText text term =
  case parseBm text of
    Nothing -> kParseError
    Just e
      | matCols (beMat e) == 0 -> ""
      | otherwise ->
          colsToString (normalizeCols (expand (standardizeMatrix 0 (beMat e)) term))

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

-- ParentCache（对齐 @cms.hpp@ 内部的 ParentCache）

data PCache = PCache
  { pcCols :: !Cols
  , pcMemo :: !(Map (Int, Int) Int)
  }

parentOf :: PCache -> Int -> Int -> (PCache, Int)
parentOf pc x y =
  case Map.lookup (x, y) (pcMemo pc) of
    Just p -> (pc, p)
    Nothing ->
      let (pc1, p) = walk pc x y
          pc' = pc1 { pcMemo = Map.insert (x, y) p (pcMemo pc1) }
       in (pc', p)
 where
  walk c cur yy =
    if yy == 0
      then go (cur - 1) c cur
      else
        let (c1, p1) = parentOf c cur (yy - 1)
         in if p1 < 0
              then (c1, -1)
              else
                if colAt (pcCols c1) p1 yy < colAt (pcCols c1) cur yy
                  then (c1, p1)
                  else walk c1 p1 yy
  go px c cur
    | px < 0 = (c, -1)
    | colAt (pcCols c) px 0 < colAt (pcCols c) cur 0 = (c, px)
    | otherwise = go (px - 1) c cur

colAt :: Cols -> Int -> Int -> Int
colAt cols x y
  | x < 0 || x >= length cols = 0
  | y < 0 || y >= length (cols !! x) = 0
  | otherwise = (cols !! x) !! y

-- L 函数：L(pc, ymax, x1, x2, m)

lFunc :: PCache -> Int -> Int -> Int -> Cols -> Int
lFunc pc ymax x1 x2 m = goY ymax
 where
  goY y
    | y < 0 = (-1)
    | colAt m x2 y == 0 = goY (y - 1)
    | otherwise =
        let (_, xx) = goX pc x2 y
         in if xx == x1 then y else goY (y - 1)
   where
    goX c cc yy =
      let (c1, p) = parentOf c cc yy
       in if x1 < p then goX c1 p yy else (c1, p)

-- expand 主体（对齐 cms_detail::expand）

expand :: Matrix -> Int -> Cols
expand input fsTerm =
  let m2Prefix = take endcol m1
      lnz = goLnz ymax
       where
        goLnz y
          | y < 0 = (-1)
          | child !! y > 0 = y
          | otherwise = goLnz (y - 1)

   in if lnz < 0 || fsTerm <= 0
        then m2Prefix
        else
          let m1pc0 = PCache m1 Map.empty
              (m1pc1, br) = parentOf m1pc0 endcol lnz
           in if br < 0
                then []  -- C++ UB quirk：BR<0 时 offset_asc[-1] 越界，预言机观测为空
                else mainBody m1pc1 br lnz m2Prefix
 where
  rows = matRows input
  cols0 = matCols input
  m1 = matData input
  endcol = cols0 - 1
  child = m1 !! endcol
  ymax = length child - 1

  mainBody m1pc br lnz0 m2Prefix0 =
    let offset = [ child !! y - colAt m1 br y | y <- [0 .. lnz0 - 1] ]

        asc pc memo r x y =
          case Map.lookup (x, y) memo of
            Just v -> (pc, memo, v)
            Nothing
              | not (r <= x) -> (pc, Map.insert (x, y) False memo, False)
              | r == x -> (pc, Map.insert (x, y) True memo, True)
              | otherwise ->
                  let (pc1, p) = parentOf pc x y
                      (pc2, memo1, v) = asc pc1 memo r p y
                   in (pc2, Map.insert (x, y) v memo1, v)

        ascBool x y =
          let (_, _, v) = asc (PCache m1 Map.empty) Map.empty br x y
           in v

        offsetAscVal x y = y < lnz0 && ascBool x y

        -- added 列：n=1..fsTerm, col=br..endcol-1
        added =
          [ [ let base = colAt m1 col y
                  inc = if offsetAscVal col y then offset !! y * n else 0
               in base + inc
            | y <- [0 .. rows - 1] ]
          | n <- [1 .. fsTerm]
          , col <- [br .. endcol - 1] ]

        m2 = m2Prefix0 ++ added

        -- colLoop
        colFinal = colLoop endcol
         where
          colLoop c =
            let c' = c - 1
             in if not (br < c')
                  then c'
                  else
                    let l = lFunc m1pc ymax br c' m1
                     in if l > lnz0 then c' else colLoop c'

        trimAtYmax cols =
          if ymax > 0
            then
              let allZero = all (\c -> ymax >= length c || c !! ymax == 0) cols
              in if allZero
                   then map (\c -> if length c > ymax then take ymax c else c) cols
                   else cols
            else cols

     in if colFinal == br
          then trimAtYmax m2
          else
            let -- 追加提升后的末列
                boosted =
                  [ child !! y
                      + (if y <= lnz0
                           then (child !! y - colAt m1 br y) * fsTerm
                           else 0)
                  | y <- [0 .. rows - 1] ]
                m2b = m2 ++ [boosted]
                c = colFinal
                c_ = c + (endcol - br)
                d = length m2b - 1
                m2pc0 = PCache m2b Map.empty

                dCols = [ buildD cc | cc <- [endcol .. d - 1] ]
                 where
                  buildD cc =
                    let nc = m2b !! cc
                        ks = [0 .. length nc - 1]
                        nc' = foldl (applyK cc) nc ks
                     in nc'
                  applyK cc nc k
                    | k > lnz0 = nc
                    | otherwise =
                        let (u, ss) = walkU 0 cc
                            l = lFunc m2pc0 ymax ss d m2b
                         in if l >= k - 1
                              then
                                let v = colAt m2b c_ k + u
                                in take k nc ++ [v] ++ drop (k + 1) nc
                              else nc
                   where
                    walkU u ss =
                      let (_, nextss) = parentOf m2pc0 ss k
                       in if nextss < endcol
                            then (u, ss)
                            else walkU (u + 1) nextss

                result = take c_ m2b ++ dCols
             in trimAtYmax result
