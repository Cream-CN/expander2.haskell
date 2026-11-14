-- | g-y（G-Y）：忠实移植 C++ @notation/gy.hpp@。
module OmegaY.Fam.Mrss.GY
  ( gYNotation
  ) where

import Control.Monad.ST (ST, runST)
import OmegaY.Notation.Types (Notation, seqNotation)

expandGY :: [Int] -> Int -> [Int]
expandGY seq _term = seq

gYNotation :: Notation
gYNotation =
  seqNotation "g-y" "G-Y" ["G-Y", "数列"] expandGY ""
