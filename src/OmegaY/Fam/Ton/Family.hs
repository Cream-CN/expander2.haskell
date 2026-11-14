-- | 记号族 ton（9 个 id）。
module OmegaY.Fam.Ton.Family (familyNotations) where

import OmegaY.Notation.Types (Notation, textAlterNotation)
import OmegaY.Fam.Ton.TonCore (tonExpandText, TonConfig (..))
import OmegaY.Fam.Ton.Variants

mkNot :: String -> String -> [String] -> TonConfig -> Notation
mkNot ident name tags cfg =
  textAlterNotation ident name tags
    (\text term -> tonExpandText cfg text term)
    (\text term -> tonExpandText cfg text term)
    ""

familyNotations :: [Notation]
familyNotations =
  [ mkNot "ton-m"    "TON"      ["TON", "序数记号", "文本"]        tonMConfig
  , mkNot "ton-i"    "TON_I"    ["TON", "n-built", "文本"]           tonIConfig
  , mkNot "ton-dr"   "TON_DoR"  ["TON", "反射度", "文本"]            tonDrConfig
  , mkNot "ton-drc"  "TON_DRC"  ["TON", "反射配置", "文本"]          tonDrcConfig
  , mkNot "ton-drp"  "TON_DRP"  ["TON", "passthrough", "文本"]       tonDrpConfig
  , mkNot "ton-drpc" "TON_DRPC" ["TON", "反射配置", "passthrough", "文本"] tonDrpcConfig
  , mkNot "ton-ibp"  "TON_IBP"  ["TON", "n-built", "文本"]           tonIbpConfig
  , mkNot "ton-mc"   "TON_MC"   ["TON", "反射配置", "文本"]           tonMcConfig
  , mkNot "ton-mpc"  "TON_MPC"  ["TON", "反射配置", "passthrough", "文本"] tonMpcConfig
  ]
