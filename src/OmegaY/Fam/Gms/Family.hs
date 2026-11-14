-- | 记号族 gms（17 个 id）。实现者：仅在本目录内新增模块。
module OmegaY.Fam.Gms.Family (familyNotations) where
import OmegaY.Notation.Types (Notation, textNotation)
import qualified OmegaY.Fam.Gms.GmsCore as G
import qualified OmegaY.Fam.Gms.LpmsCore as L

-- | 该族的记号清单（id 与 C++ kMetaArray 完全一致）。
familyNotations :: [Notation]
familyNotations =
  [ textNotation "gms-gbms-omega-p" "GMS GBMS ω-P" []
      (G.runGms G.SysGBMS G.ProjOmegaP) ""
  , textNotation "gms-gbms-pqss" "GMS GBMS pQSS" []
      (G.runGms G.SysGBMS G.ProjPQSS) ""
  , textNotation "gms-gbms-qss" "GMS GBMS QSS" []
      (G.runGms G.SysGBMS G.ProjQSS) ""
  , textNotation "gms-gbms-full" "GMS GBMS Full" []
      (G.runGms G.SysGBMS G.ProjFull) ""
  , textNotation "gms-gbms-weird" "GMS GBMS Weirdly Full" []
      (G.runGms G.SysGBMS G.ProjWeirdlyFull) ""
  , textNotation "gms-upms-omega-p" "GMS UPMS ω-P" []
      (G.runGms G.SysUPMS G.ProjOmegaP) ""
  , textNotation "gms-upms-pqss" "GMS UPMS pQSS" []
      (G.runGms G.SysUPMS G.ProjPQSS) ""
  , textNotation "gms-upms-qss" "GMS UPMS QSS" []
      (G.runGms G.SysUPMS G.ProjQSS) ""
  , textNotation "gms-upms-full" "GMS UPMS Full" []
      (G.runGms G.SysUPMS G.ProjFull) ""
  , textNotation "gms-upms-weird" "GMS UPMS Weirdly Full" []
      (G.runGms G.SysUPMS G.ProjWeirdlyFull) ""
  , textNotation "gms-lpms2-omega-p" "GMS LPMS2 ω-P" []
      (G.runGms G.SysLPMS2 G.ProjOmegaP) ""
  , textNotation "gms-lpms2-pqss" "GMS LPMS2 pQSS" []
      (G.runGms G.SysLPMS2 G.ProjPQSS) ""
  , textNotation "gms-lpms2-qss" "GMS LPMS2 QSS" []
      (G.runGms G.SysLPMS2 G.ProjQSS) ""
  , textNotation "gms-lpms2-full" "GMS LPMS2 Full" []
      (G.runGms G.SysLPMS2 G.ProjFull) ""
  , textNotation "gms-lpms2-weird" "GMS LPMS2 Weirdly Full" []
      (G.runGms G.SysLPMS2 G.ProjWeirdlyFull) ""
  , textNotation "lpms" "LPMS" [] (L.runLpms False) ""
  , textNotation "lptss" "LPTSS" [] (L.runLpms True) ""
  ]
