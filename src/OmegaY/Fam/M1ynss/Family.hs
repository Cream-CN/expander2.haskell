-- | 记号族 m1ynss（8 个 id）。实现者：仅在本目录内新增模块。
-- 必须导出 familyNotations，且 id 与 C++ 侧 kMetaArray 完全一致。
module OmegaY.Fam.M1ynss.Family (familyNotations) where
import OmegaY.Notation.Types (Notation, paramNotation, paramAlterNotation)
import qualified OmegaY.Fam.M1ynss.Flat as Flat
import qualified OmegaY.Fam.M1ynss.Tm1 as Tm1
import qualified OmegaY.Fam.M1ynss.Bm1 as Bm1
import qualified OmegaY.Fam.M1ynss.Bt as Bt
import qualified OmegaY.Fam.M1ynss.Bt3 as Bt3
import qualified OmegaY.Fam.M1ynss.Btl as Btl

-- | 该族的记号清单（顺序无关；最终由 Registry 统一按显示名排序）。
familyNotations :: [Notation]
familyNotations =
  [ paramNotation
      "-1y-(n+1)ss"
      "(-1)Y-(n+1)SS 负一 Y 序列系统"
      ["Minus1", "BM-like", "参数化", "文本"]
      (0, 25, 0, "n (0-25):")
      Flat.expand
      ""
  , paramNotation
      "t--1y-(n+1)ss"
      "T(n+1)SS 超限负一 Y 序列系统"
      ["Minus1", "BM-like", "参数化", "递归嵌套", "文本"]
      (0, 25, 0, "n (0-25):")
      Tm1.expandTm1
      ""
  , paramNotation
      "abs-bm1ynss"
      "AbsB(n+1)SS 绝对分支负一 Y 序列系统"
      ["Minus1", "BM-like", "参数化", "递归嵌套", "文本"]
      (0, 25, 0, "n (0-25):")
      Bm1.expandAbs
      ""
  , paramNotation
      "rel-bm1ynss"
      "RelB(n+1)SS 相对分支负一 Y 序列系统"
      ["Minus1", "BM-like", "参数化", "递归嵌套", "文本"]
      (0, 25, 0, "n (0-25):")
      Bm1.expandRel
      ""
  , paramNotation
      "bt1ynss"
      "BT*(n+1)SS v1"
      ["Minus1", "BT", "参数化", "文本"]
      (1, 25, 1, "n (1-25):")
      (Bt.expandBt Bt.Bt1)
      ""
  , paramNotation
      "bt2ynss"
      "strong BT*(n+1)SS v2"
      ["Minus1", "BT", "参数化", "文本"]
      (1, 25, 1, "n (1-25):")
      (Bt.expandBt Bt.Bt2)
      ""
  , paramAlterNotation
      "bt3ynss"
      "weak BT*(n+1)SS v3"
      ["Minus1", "BT", "参数化", "文本"]
      (1, 25, 1, "n (1-25):")
      Bt3.expandBt3
      Bt3.expandBt3Alter
      ""
  , paramNotation
      "btlynss"
      "BTL(n+1)SS 带标记"
      ["Minus1", "BT", "参数化", "文本"]
      (1, 25, 1, "n (1-25):")
      Btl.expandBtl
      ""
  ]
