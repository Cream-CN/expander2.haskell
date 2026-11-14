-- | 记号族 mrss（已实现并差分的 id）。
module OmegaY.Fam.Mrss.Family (familyNotations) where
import OmegaY.Fam.Mrss.Mrss121 (mrss121Notation)
import OmegaY.Fam.Mrss.OmegaYSeq
  ( omegaYSeqNotation
  , omegaYMediumNotation
  , omegaYStrongNotation
  )
import OmegaY.Notation.Types (Notation)

familyNotations :: [Notation]
familyNotations =
  [ mrss121Notation
  , omegaYSeqNotation
  , omegaYMediumNotation
  , omegaYStrongNotation
  ]
