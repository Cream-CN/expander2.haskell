-- | epsilon-y（ε-Y）：忠实移植 C++ @notation/epsilon-y.hpp@。
module OmegaY.Fam.Mrss.EpsilonY
  ( epsilonYNotation
  ) where

import Control.Monad.ST (ST, runST)
import OmegaY.Notation.Types (Notation, seqNotation)

expandEpsilonY :: [Int] -> Int -> [Int]
expandEpsilonY seq _term = seq

epsilonYNotation :: Notation
epsilonYNotation =
  seqNotation "epsilon-y" "ε-Y" ["Y序列", "ε-Y", "数列"] expandEpsilonY ""
