-- | 记号族 tree（5 个 id）。实现者：仅在本目录内新增模块。
-- 必须导出 familyNotations，且 id 与 C++ 侧 kMetaArray 完全一致。
module OmegaY.Fam.Tree.Family (familyNotations) where
import OmegaY.Notation.Types (Notation, textNotation)
import qualified OmegaY.Fam.Tree.Tbm as Tbm
import qualified OmegaY.Fam.Tree.Tupms as Tupms
import qualified OmegaY.Fam.Tree.Btbm as Btbm

-- | 该族的记号清单（顺序无关；最终由 Registry 统一按显示名排序）。
familyNotations :: [Notation]
familyNotations =
  [ textNotation "tbm" "TBM 超限巴斯丘矩阵" [] Tbm.expandText ""
  , textNotation "tupms" "TUPMS 超限 UPMS" [] Tupms.expandText ""
  , textNotation "btbm" "BTBM Bubby3 超限巴斯丘矩阵" [] Btbm.btbmExpandText ""
  , textNotation "btbm-weak" "BTBM-weak 弱版 Bubby3 超限巴斯丘矩阵" [] Btbm.btbwExpandText ""
  , textNotation "bbm" "BBM 分支巴斯丘矩阵" [] Btbm.bbmExpandText ""
  ]
