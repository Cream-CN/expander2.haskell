-- | 记号族 den（3 个 id：den1 / blp / iblp）。
module OmegaY.Fam.Den.Family (familyNotations) where

import OmegaY.Notation.Types
import qualified OmegaY.Fam.Den.Den1 as Den1
import qualified OmegaY.Fam.Den.Blp as Blp
import qualified OmegaY.Fam.Den.Iblp as Iblp

-- | id 与 C++ kMetaArray 一致；tags 照抄 registry.hpp。
familyNotations :: [Notation]
familyNotations =
  [ textAlterNotation "den1" "DEN1" ["DEN", "文本", "旧版"]
      Den1.expandText Den1.expandTextAlter ""
  , textAlterNotation "blp" "blp" ["blp", "文本"]
      Blp.expandText Blp.expandTextAlter ""
  , textAlterNotation "iblp" "iblp" ["iblp", "文本", "逆"]
      Iblp.expandText Iblp.expandTextAlter ""
  ]
