-- | Xy 族。只在本目录新增模块；familyNotations 只列已差分归零的 id。
module OmegaY.Fam.Xy.Family (familyNotations) where
import OmegaY.Fam.Xy.XyV1 (xyV1Notation)
import OmegaY.Fam.Xy.XyV2 (xyV2Notation)
import OmegaY.Fam.Xy.XyV3 (xyV3Notation)
import OmegaY.Notation.Types (Notation)

familyNotations :: [Notation]
familyNotations =
  [ xyV1Notation
  , xyV2Notation
  , xyV3Notation
  ]
