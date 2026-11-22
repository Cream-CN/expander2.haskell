-- | BMS / UPMS 文本入口，对齐 registry.hpp 的 bmsExpandTextWith / upmsExpandText。
module OmegaY.Fam.Bms.BmsText
  ( bmsExpandTextWith
  , bmsTruncateFs
  , upmsExpandText
  , upmsTruncateFs
  ) where

import Data.Char (isDigit)
import OmegaY.Matrix
import OmegaY.Parse (parseMatrix, matrixToString)
import qualified OmegaY.Fam.Bms.BmsCore as BC
import qualified OmegaY.Fam.Bms.UpmsCore as UC

-- BMS

kParseError :: String
kParseError = "解析失败，请确认你没有使用全角括号，逗号"

kNonStandard :: String
kNonStandard = "输入的矩阵为非标准表达式"

bmsExpandTextWith :: BC.Version -> String -> Int -> String
bmsExpandTextWith ver text cliTerm =
  let (m, mTerm) = parseMatrix text
  in if matEmpty m
       then kParseError
       else if not (BC.bmsIsStandard ver m)
              then kNonStandard
              else
                let parsedTerm = case mTerm of
                                   Just n -> n
                                   Nothing -> cliTerm
                    t = if parsedTerm < 1 then 1 else parsedTerm
                    result = BC.bmsExpand ver m t
                in matrixToString result 0

-- | FS 截断：删末列。
bmsTruncateFs :: String -> Int -> String
bmsTruncateFs out _term =
  let (m, _) = parseMatrix out
  in if matCols m > 1
       then matrixToString (matResizeCols (matCols m - 1) m) 0
       else out

-- UPMS

kUpmsEmpty :: String
kUpmsEmpty = "后继形展开为空矩阵，或未能定位坏根"

-- | parseUpmsText：逐字符扫描，与 registry.hpp 432-482 逐字对齐。
--
--   要点（容易被写成"更规整"的解析器而偏离原实现）：
--   * 只按 '(' 切列、按 ')' 收列，嵌套括号不参与结构；
--   * 数字位在遇到任何非数字字符时**不中断累加**（例如 @((1),(2))@ 会得到
--     单列 @[1,2]@，而 @(1,*)@ 会得到 @[1]@）；
--   * '-' 只在尚未开始读数字时生效；
--   * ',' 处无论是否正在读数字都会重置当前值；
--   * 空列补 0，行数取各列长度的最大值，短列尾部补 0。
parseUpmsText :: String -> Matrix
parseUpmsText s =
  let cols = parseCols s
  in if null cols
       then emptyMatrix
       else
         let maxRows = maximum (0 : map length cols)
         in Matrix maxRows (length cols)
              [ take maxRows (c ++ repeat 0) | c <- cols ]
  where
    parseCols :: String -> [[Int]]
    parseCols str =
      case dropWhile (/= '(') str of
        [] -> []
        (_ : afterOpen) ->
          let (inside, afterClose) = break (== ')') afterOpen
              col = scanCol inside
              rest = case afterClose of
                       (')' : r) -> r
                       _ -> []
          in (if null col then [0] else col) : parseCols rest

    scanCol :: String -> [Int]
    scanCol = go [] 0 False False
      where
        go acc cur neg inNum [] =
          if inNum then acc ++ [if neg then negate cur else cur] else acc
        go acc cur neg inNum (c : cs)
          | isDigit c = go acc (cur * 10 + fromEnum c - fromEnum '0') neg True cs
          | c == '-' && not inNum = go acc cur True True cs
          | c == ',' =
              let acc' = if inNum then acc ++ [if neg then negate cur else cur] else acc
               in go acc' 0 False False cs
          | otherwise = go acc cur neg inNum cs

formatUpmsText :: Matrix -> String
formatUpmsText m
  | matEmpty m = ""
  | otherwise = concat [ formatCol c | c <- [0 .. matCols m - 1] ]
  where
    formatCol c =
      let lastNz = findLastNz (matRows m - 1)
          findLastNz r
            | r < 0 = 0
            | matAt m r c /= 0 = r
            | otherwise = findLastNz (r - 1)
          vals = [ show (matAt m r c) | r <- [0 .. lastNz] ]
      in "(" ++ joinComma vals ++ ")"
    joinComma [] = ""
    joinComma [x] = x
    joinComma (x : xs) = x ++ "," ++ joinComma xs

upmsExpandText :: String -> Int -> String
upmsExpandText text term =
  let m = parseUpmsText text
  in if matEmpty m
       then kParseError
       else
         let result = UC.upmsExpand m term
             out = formatUpmsText result
         in if null out
              then kUpmsEmpty
              else out

-- | UPMS FS 截断：删末列。
upmsTruncateFs :: String -> Int -> String
upmsTruncateFs out _term =
  let m = parseUpmsText out
  in if matCols m > 1
       then formatUpmsText (matResizeCols (matCols m - 1) m)
       else out
