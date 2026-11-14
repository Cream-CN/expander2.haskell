-- | 记号族 mn。
module OmegaY.Fam.Mn.Family (familyNotations) where
import OmegaY.Notation.Types (Notation)
import OmegaY.Fam.Mn.E0Mn (e0MnNotation, strongE0MnNotation)
import OmegaY.Fam.Mn.OmegaMn (omegaMnNotation)
import OmegaY.Fam.Mn.NMn (nMnNotation)
import OmegaY.Fam.Mn.PcfOmegaMn (pcfOmegaMnNotation)
import OmegaY.Fam.Mn.TOmegaMn (tOmegaMnNotation)
import OmegaY.Fam.Mn.SOmega2Mn (sOmega2MnNotation)
import OmegaY.Fam.Mn.SAOmega2Mn (saOmega2MnNotation)
import OmegaY.Fam.Mn.SOmegaPowOmegaMn (sOmegaPowOmegaMnNotation)

familyNotations :: [Notation]
familyNotations =
  [ e0MnNotation
  , strongE0MnNotation
  , omegaMnNotation
  , nMnNotation 1 "1-mn" "1MN"
  , nMnNotation 2 "2-mn" "2MN"
  , nMnNotation 3 "3-mn" "3MN"
  , pcfOmegaMnNotation
  , tOmegaMnNotation
  , sOmega2MnNotation
  , saOmega2MnNotation
  , sOmegaPowOmegaMnNotation
  ]
