-- | 记号族 upmn（4 个 id）。
module OmegaY.Fam.Upmn.Family (familyNotations) where
import OmegaY.Notation.Types (Notation, textAlterNotation)
import qualified OmegaY.Fam.Upmn.Up1Mn as Up1Mn
import qualified OmegaY.Fam.Upmn.Up2MnV1a as Up2MnV1a
import qualified OmegaY.Fam.Upmn.Up2MnV1b as Up2MnV1b

familyNotations :: [Notation]
familyNotations =
  [ textAlterNotation "up1mn" "UP1MN" ["MN","UPMN","文本"]
      Up1Mn.expandText Up1Mn.expandAlter ""
  , textAlterNotation "up2mn-v1a" "UP2MN v1A" ["MN","UPMN","文本"]
      Up2MnV1a.expandText Up2MnV1a.expandAlter ""
  , textAlterNotation "up2mn-v1b" "UP2MN v1B" ["MN","UPMN","文本"]
      (Up2MnV1b.expandText False) (Up2MnV1b.expandAlter False) ""
  , textAlterNotation "up2mn-v1b-plus" "UP2MN v1B+" ["MN","UPMN","文本"]
      (Up2MnV1b.expandText True) (Up2MnV1b.expandAlter True) ""
  ]
