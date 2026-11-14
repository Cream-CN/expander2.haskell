-- | mrss121（MrSS1.2.1）：文本型记号，对齐 C++ @notation/MrSS121.hpp@。
--
--   C++ 侧经自定义 @MrssNotation@ 适配器：has_text 优先，expand_text =
--   @Mrss121Notation::expand_string@；FS 截断走默认 truncate_fs（MrSS 解析后
--   pop 末元素，即共享模块 mrssTruncate）；alter 不截断。
module OmegaY.Fam.Mrss.Mrss121
  ( mrss121Notation
  ) where

import OmegaY.Mrss (mrssExpandText)
import OmegaY.Notation.Types (Notation, textNotation)

mrss121Notation :: Notation
mrss121Notation =
  textNotation "mrss121" "MrSS1.2.1" ["MrSS", "矩阵", "数列"] mrssExpandText ""
