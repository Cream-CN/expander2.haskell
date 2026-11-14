-- | Yseq 族。只在本目录新增模块；familyNotations 只列已差分归零的 id。
module OmegaY.Fam.Yseq.Family (familyNotations) where
import OmegaY.Notation.Types (Notation)
import OmegaY.Fam.Yseq.EpsilonY (epsilonYNotation)
import OmegaY.Fam.Yseq.GY (gYNotation)

familyNotations :: [Notation]
familyNotations = [ epsilonYNotation, gYNotation ]
