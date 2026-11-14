-- | 记号族 dbms（15 个 id）。实现者：仅在本目录内新增模块。
module OmegaY.Fam.Dbms.Family (familyNotations) where
import OmegaY.Notation.Types (Notation, textAlterNotation)
import qualified OmegaY.Fam.Dbms.Up1Dbms as Up1
import qualified OmegaY.Fam.Dbms.S1Dbms as S1
import qualified OmegaY.Fam.Dbms.S2Dbms as S2
import qualified OmegaY.Fam.Dbms.Up2V1 as Up2V1
import qualified OmegaY.Fam.Dbms.Up2V1bPlus as Up2V1b
import qualified OmegaY.Fam.Dbms.Up2V1c as Up2V1c
import qualified OmegaY.Fam.Dbms.Up2V2 as Up2V2
import qualified OmegaY.Fam.Dbms.SOmegaV1 as SΩV1
import qualified OmegaY.Fam.Dbms.SOmegaV2 as SΩV2
import qualified OmegaY.Fam.Dbms.SOmegaV3 as SΩV3
import qualified OmegaY.Fam.Dbms.SOmegaP1 as SΩP1
import qualified OmegaY.Fam.Dbms.SOmega2 as SΩ2
import qualified OmegaY.Fam.Dbms.SOmegaSquare as SΩSq
import qualified OmegaY.Fam.Dbms.Ts2Dbms as Ts2
import qualified OmegaY.Fam.Dbms.TsOmegaDbms as TsΩ

familyNotations :: [Notation]
familyNotations =
  [ textAlterNotation "up1dbms" "UP1DBMS" ["MN","UPMN","DBMS","文本"]
      Up1.expandText Up1.expandAlter ""
  , textAlterNotation "s1dbms" "S1DBMS" ["MN","DBMS","文本"]
      S1.expandText S1.expandAlter ""
  , textAlterNotation "s2dbms" "S2DBMS" ["MN","DBMS","文本"]
      S2.expandText S2.expandAlter ""
  , textAlterNotation "up2dbms-v1" "UP2DBMS v1" ["MN","UPMN","DBMS","文本"]
      Up2V1.expandText Up2V1.expandAlter ""
  , textAlterNotation "up2dbms-v1b-plus" "UP2DBMS v1B+" ["MN","UPMN","DBMS","文本"]
      Up2V1b.expandText Up2V1b.expandAlter ""
  , textAlterNotation "up2dbms-v1c" "UP2DBMS v1C" ["MN","UPMN","DBMS","文本"]
      Up2V1c.expandText Up2V1c.expandAlter ""
  , textAlterNotation "up2dbms-v2" "UP2DBMS v2" ["MN","UPMN","DBMS","文本"]
      Up2V2.expandText Up2V2.expandAlter ""
  , textAlterNotation "s-omega-dbms" "SωDBMS v1" ["MN","DBMS","文本"]
      SΩV1.expandText SΩV1.expandAlter ""
  , textAlterNotation "s-omega-dbms-v2" "SωDBMS v2" ["MN","DBMS","文本"]
      SΩV2.expandText SΩV2.expandAlter ""
  , textAlterNotation "s-omega-dbms-v3" "SωDBMS v3" ["MN","DBMS","文本"]
      SΩV3.expandText SΩV3.expandAlter ""
  , textAlterNotation "s-omega-p1-dbms" "Sω+1DBMS" ["MN","DBMS","文本"]
      SΩP1.expandText SΩP1.expandAlter ""
  , textAlterNotation "s-omega2-dbms" "Sω2DBMS" ["MN","DBMS","文本"]
      SΩ2.expandText SΩ2.expandAlter ""
  , textAlterNotation "s-omega-square-dbms" "Sω^2DBMS" ["MN","DBMS","文本"]
      SΩSq.expandText SΩSq.expandAlter ""
  , textAlterNotation "ts-2-dbms" "TS2DBMS" ["MN","DBMS","文本"]
      Ts2.expandText Ts2.expandAlter ""
  , textAlterNotation "ts-omega-dbms" "TSΩDBMS" ["MN","DBMS","文本"]
      TsΩ.expandText TsΩ.expandAlter ""
  -- ts-omega-dbms 暂未纳入：Height=[HeightEntry{anchor,position}] 的 CNF 互转在
  -- cnfToTsOmega(omega) 路径上与 C++ 存在差异（parseOrdinal "w" 后 convert_matrix
  -- 的 low_column 判定不通过），导致 (1^(1@w)) 类输入解析失败。已定位在
  -- TsOmegaDbms.hs 的 convertMatrixOne/to_relative=False 分支对 suspend 结果
  -- [[],[(1,HE 0 [])]] 的 pureLow 判定（anchor=1 而非 0），需对照 C++
  -- clean_matrix_padding 的 anchor 重置逻辑修正。
  ]
