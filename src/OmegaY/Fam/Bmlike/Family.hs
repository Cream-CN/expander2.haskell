-- | 记号族 bmlike（9 个 id）。
module OmegaY.Fam.Bmlike.Family (familyNotations) where

import OmegaY.Notation.Types
import qualified OmegaY.Fam.Bmlike.Bsm as Bsm
import qualified OmegaY.Fam.Bmlike.Bhm as Bhm
import qualified OmegaY.Fam.Bmlike.Cms as Cms
import qualified OmegaY.Fam.Bmlike.Dsm as Dsm
import qualified OmegaY.Fam.Bmlike.Blm as Blm
import qualified OmegaY.Fam.Bmlike.Wmm as Wmm

-- | 该族的记号清单（id 与 C++ kMetaArray 一致）。
familyNotations :: [Notation]
familyNotations =
  [ textAlterNotation "bsm" "BSM" [] Bsm.expandText Bsm.expandText ""
  , textAlterNotation "bhm" "BHM" [] Bhm.expandFs Bhm.expandAlter ""
  , textAlterNotation "1-bm-bhm" "1BM-BHM" []
      (Bhm.expandNFs 1) (Bhm.expandNAlter 1) ""
  , textAlterNotation "2-bm-bhm" "2BM-BHM" []
      (Bhm.expandNFs 2) (Bhm.expandNAlter 2) ""
  , textAlterNotation "3-bm-bhm" "3BM-BHM" []
      (Bhm.expandNFs 3) (Bhm.expandNAlter 3) ""
  , textNotation "cms" "CMS" [] Cms.expandText ""
  , textAlterNotation "dsm" "DSM" [] Dsm.expandFs Dsm.expandAlter ""
  , textNotation "blm" "BLM" [] Blm.expandText ""
  , textAlterNotation "wmm" "wMM" [] Wmm.expandFs Wmm.expandAlter ""
  ]
