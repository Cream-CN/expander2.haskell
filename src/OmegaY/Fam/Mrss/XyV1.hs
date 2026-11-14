-- | xy-v1（X-Y V1）：忠实移植 C++ @notation/xy-v1.hpp@。
module OmegaY.Fam.Mrss.XyV1
  ( xyV1Notation
  ) where

import Control.Monad.ST (ST, runST)
import OmegaY.Notation.Types (Notation, seqNotation)

expandXyV1 :: [Int] -> Int -> [Int]
expandXyV1 seq _term = seq

xyV1Notation :: Notation
xyV1Notation =
  seqNotation "xy-v1" "X-Y V1" ["X-Y", "数列"] expandXyV1 ""
