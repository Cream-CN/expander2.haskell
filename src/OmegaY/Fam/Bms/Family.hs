-- | 记号族 bms（11 个 id：10 个 BMS 版本 + upms）。
module OmegaY.Fam.Bms.Family (familyNotations) where

import OmegaY.Notation.Types
import qualified OmegaY.Fam.Bms.BmsCore as BC
import qualified OmegaY.Fam.Bms.BmsText as BT

bmsNotation :: String -> String -> [String] -> BC.Version -> Notation
bmsNotation ident name tags ver =
  Notation
    { nId = ident
    , nName = name
    , nTags = tags
    , nParam = Nothing
    , nCaps = TextCaps (BT.bmsExpandTextWith ver) ""
    }

upmsNotation :: Notation
upmsNotation =
  Notation
    { nId = "upms"
    , nName = "UPMS"
    , nTags = ["UPMS", "矩阵", "文本"]
    , nParam = Nothing
    , nCaps = TextCaps BT.upmsExpandText ""
    }

-- | 该族的记号清单（id 与 C++ kMetaArray 一致）。
familyNotations :: [Notation]
familyNotations =
  [ bmsNotation "bms-v1"  "BMS v1.0" ["BMS", "矩阵", "旧版"] BC.V1
  , bmsNotation "bms-v2"  "BMS v2.0" ["BMS", "矩阵"]            BC.V2
  , bmsNotation "bms-v21" "BMS v2.1" ["BMS", "矩阵"]            BC.V21
  , bmsNotation "bms-v22" "BMS v2.2" ["BMS", "矩阵"]            BC.V22
  , bmsNotation "bms-v23" "BMS v2.3" ["BMS", "矩阵"]            BC.V23
  , bmsNotation "bms-v3"  "BMS v3.0" ["BMS", "矩阵"]            BC.V3
  , bmsNotation "bms-v31" "BMS v3.1" ["BMS", "矩阵"]            BC.V31
  , bmsNotation "bms-v32" "BMS v3.2" ["BMS", "矩阵"]            BC.V32
  , bmsNotation "bms-v33" "BMS v3.3" ["BMS", "矩阵"]            BC.V33
  , bmsNotation "bms-v4"  "BMS v4.0" ["BMS", "矩阵", "最新"]    BC.V4
  , upmsNotation
  ]
