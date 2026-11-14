-- | 记号族 aw2mn（4 个 id）。
module OmegaY.Fam.Aw2mn.Family (familyNotations) where
import OmegaY.Notation.Types (Notation)
import OmegaY.Fam.Aw2mn.Aw2mn (aw2mnNotation)
import OmegaY.Fam.Aw2mn.Aw2mnShare (SubtractKind(..))

familyNotations :: [Notation]
familyNotations =
  [ aw2mnNotation "a-omega2-mn-2" "Aω2MN2" False V2
  , aw2mnNotation "weak-a-omega2-mn-2" "wAω2MN2" True V2
  , aw2mnNotation "a-omega2-mn-3" "Aω2MN3" False V3
  , aw2mnNotation "weak-a-omega2-mn-3" "wAω2MN3" True V3
  ]
