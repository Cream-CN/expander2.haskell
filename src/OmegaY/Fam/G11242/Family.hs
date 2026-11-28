-- | 11242 家族：SRD / RPD / ARD / ARD2 / IPD。
module OmegaY.Fam.G11242.Family
  ( familyNotations
  ) where

import OmegaY.Fam.G11242.Graph (ard2Notation, ardNotation, rpdNotation)
import OmegaY.Fam.G11242.Ipd (ipdNotation)
import OmegaY.Fam.G11242.Srd (srdNotation)
import OmegaY.Notation.Types (Notation)

familyNotations :: [Notation]
familyNotations = [srdNotation, rpdNotation, ardNotation, ard2Notation, ipdNotation]
