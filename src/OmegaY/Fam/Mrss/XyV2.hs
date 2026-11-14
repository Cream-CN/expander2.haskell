-- | xy-v2（X-Y V2）：忠实移植 C++ @notation/xy-v2.hpp@。
module OmegaY.Fam.Mrss.XyV2
  ( xyV2Notation
  ) where

import Control.Monad.ST (ST, runST)
import OmegaY.Notation.Types (Notation, seqNotation)

expandXyV2 :: [Int] -> Int -> [Int]
expandXyV2 seq _term = seq

xyV2Notation :: Notation
xyV2Notation =
  seqNotation "xy-v2" "X-Y V2" ["X-Y", "数列"] expandXyV2 ""
