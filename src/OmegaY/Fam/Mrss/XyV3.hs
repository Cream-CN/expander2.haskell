-- | xy-v3（X-Y V3）：忠实移植 C++ @notation/xy-v3.hpp@。
module OmegaY.Fam.Mrss.XyV3
  ( xyV3Notation
  ) where

import Control.Monad.ST (ST, runST)
import OmegaY.Notation.Types (Notation, seqNotation)

expandXyV3 :: [Int] -> Int -> [Int]
expandXyV3 seq _term = seq

xyV3Notation :: Notation
xyV3Notation =
  seqNotation "xy-v3" "X-Y V3" ["X-Y", "数列"] expandXyV3 ""
