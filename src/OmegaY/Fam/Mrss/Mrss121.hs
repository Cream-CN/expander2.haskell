-- | mrss121（MrSS1.2.1）：文本型记号，对齐 C++ @notation/MrSS121.hpp@。
module OmegaY.Fam.Mrss.Mrss121
  ( mrss121Notation
  ) where

import OmegaY.Mrss (mrssExpandText)
import OmegaY.Notation.Types (Notation, textNotation)

mrss121Notation :: Notation
mrss121Notation =
  textNotation "mrss121" "MrSS1.2.1" ["MrSS", "矩阵", "数列"] mrssExpandText ""
