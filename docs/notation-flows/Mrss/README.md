# Mrss（2 个 id：`mrss121` `omega-y-sequence`）

**一句话导读**：这族两支算法互不相同——`mrss121` 在 `Expr` 树上做 MrSS1.2.1 坏根定位与坏部复制（`src/OmegaY/Mrss.hs`）；`omega-y-sequence` 在 `EntryGraph` 条目池上做 ω-Y sequence 展开，与 YMagma 族两条共用 `src/OmegaY/Fam/Mrss/OmegaYSeq.hs` 流水和 `EntryGraph` 节点池。前者对应 `docs/diagrams/04-kernel-flow/` **第五链（基础序列系）**的「找坏根 → 复制提升」同构动作（数据结构是 `Expr` 树而非整数序列）；后者对应**第二链（ω-Y / MrSS 系）**。

## 族头事实

- `src/OmegaY/Fam/Mrss/Family.hs:11-17` 的 `familyNotations` 实际返回 **4** 个记号：`mrss121Notation`、`omegaYSeqNotation`、`omegaYMediumNotation`、`omegaYStrongNotation`。后两条在手册归类上属 YMagma 族（见 `docs/notation-flows/YMagma/README.md`），本族细目只写前 2 个。
- 同目录 `EpsilonY.hs` / `GY.hs` / `XyV1.hs` / `XyV2.hs` / `XyV3.hs` 里还留有 `epsilon-y` / `g-y` / `xy-v1` / `xy-v2` / `xy-v3` 的字面 `seqNotation` 定义（`src/OmegaY/Fam/Mrss/EpsilonY.hs:14`、`Mrss/GY.hs:14`、`Mrss/XyV1.hs:14`、`Mrss/XyV2.hs:14`、`Mrss/XyV3.hs:14`），但五份都是恒等残枝：`expandEpsilonY seq _term = seq`（`EpsilonY.hs:10`）、`expandGY seq _term = seq`（`GY.hs:10`）、`expandXyV1/V2/V3 seq _term = seq`（`XyV1.hs:10`、`XyV2.hs:10`、`XyV3.hs:10`）。`Mrss.Family.familyNotations` 只导入 `Mrss121` 与 `OmegaYSeq` 的四个名字（`Family.hs:4-11`），并未导入这五份残枝；它们不进 `notationPool`，与 Yseq / Xy 之间不存在 `poolMap` 后写覆盖先写的关系。注册表唯一生效的拷贝在 `Fam.Yseq.Family`（`Registry.hs:56`；`Yseq/EpsilonY.hs`、`Yseq/GY.hs`）与 `Fam.Xy.Family`（`Registry.hs:57`；`Xy/XyV1..3.hs`），这 5 个 id 的权威归属是 Yseq / Xy 族，本手册不为它们写细目。
- 两条在册 id 之间**不共用算法骨架**：`mrss121` 走 `Mrss.hs` 的 `Expr` 树文本支，`omega-y-sequence` 走 `OmegaYSeq.hs` 的图展开支；各自小节写自己的步骤。

### `mrss121` · MrSS1.2.1（idOrder #12）

- **族 / 模块**：Mrss · `src/OmegaY/Fam/Mrss/Mrss121.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Mrss.mrssExpandText`（`src/OmegaY/Mrss.hs:201-205`；注册 `Mrss121.hs:9-11`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`1,(2,3),4` 形态的 `Expr` 文本 + `--term` → 同形态 `Expr` 文本；解析失败输出 `[]`
- **term / param 语义**：`term` = 坏部复制次数，`max 1 term0` 兜底（`Mrss.hs:205, 197`）；`param` 不参与（`TextCaps` 分支不看 `nParam`）
- **步骤**：
  1. `mrssParse text`（`Mrss.hs:29-33`；`parseSequenceUntil` `49-70`、`parseElement` `72-83`）；`Nothing` → 输出 `"[]"`（`204`），不产生 `Left`
  2. `mrssExpand e (max 1 term0)`（`193-198`）→ `mrssStep seq term`（`181-191`）
  3. 短路：`null seq` → `[]`（`183`）；`isOne (last seq)`（`95-97`，`Atom 1`）→ `init seq`（`184`）
  4. `findBadRoot`（`146-159`）：长度 `< 2` → `Nothing`；从 `length seq - 2` 倒扫——原子对 `cv < lv` 命中（`158`）；否则 `startsWith`（`135-140`）判定末项前缀包含候选（`159`）；`Nothing` → `init seq`（`187`）
  5. `difference`（`161-162`）= 末项 `lastValue` − 根 `lastValue` − 1；`bad = take (length seq - 1 - root) (drop root seq)`（`190`）
  6. `replaceLast`（`165-178`）：`result0 = init seq`；`copies = [addValue e (diff * i) | i <- [1..term], e <- bad]`（`168`，`addValue` `118-121` 只在最右分支加值）；`rootVal = lastValue (head bad)`；补足循环：末项 `lastValue` ≤ `rootVal` 且 `extra < term` 时再追加一份偏移 `diff*(term+extra+1)` 的 `bad`（`173-176`）
  7. `mrssToString`（`35-37`）序列化：空 → `[]`；`Atom` 显示数值；`Node` 包 `(...)`，逗号连接（`exprToString` `39-41`、`intercalateComma` `43-46`）
- **分叉**：全族唯一的 `Expr` 树文本支，不碰 `EntryGraph`；与基础序列系的差别是坏部为嵌套 `Expr`、平移走 `addValue` 而非整数列位移
- **错误**：无自有 `Left`；解析失败坍缩成文本 `"[]"`（`Mrss.hs:204`）；仅 `TextCaps` 不校验（§00 通用，空文本/负 term 不被 CLI 拦）

### `omega-y-sequence` · ω-Y sequence（idOrder #23）

- **族 / 模块**：Mrss · `src/OmegaY/Fam/Mrss/OmegaYSeq.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`OmegaYSeq.expandOmegaYSeq`（`OmegaYSeq.hs:601-602`；注册对象 `omegaYSeqNotation` `610-613`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` 参与 `eff = term + length seq`（`OmegaYSeq.hs:597`）；生成轮次由 `omegaYLimit` 内 `loopN` 按 `eff` 跑；`param` 不参与
- **步骤**：
  1. `SeqCaps` 外壳：`term < 1` 拦下、`parseSequence` 切序列、空则拦下（`Types.hs:75, 79`）
  2. `expandOmegaY KindSeq`（`589-599`）：`null seq` → `[]`；`last seq == 1` → `init seq`；`length seq == 1` → `seq`；`eff = term + length seq`
  3. `omegaYLimitKind`（`375-376`）→ `omegaYLimit KindSeq`（`378-426`）：`phantomVal = kInfinity`（32 位 INT_MAX，`380, 19-20`）；`fromSequence`（`84-99`）、`drawMountain`（`123-164`）；退化守卫原样返回 `seq`（`384, 388, 394, 400, 404, 407`）；重画 `m1`、`findSameRow`（`428-436`）、`collectMagma`（`438-474`）、`loopN`（`476-488`）、`toSequenceG`（`101-110`）
  4. 收尾：`KindSeq` 支结果 `length full >= 2` 时截末项 `init full`，否则原样（`599`）；`seqToString` 拼后缀 `""`
- **分叉**：与 YMagma 两条同骨架，三处 `KindSeq` 专属：phantom 取 `kInfinity`、`eff` 加序列长度、输出截末项；收集分派走 `collect` 的非 Strong 支（`collectUsual`/`collect1D`，`287-302`）
- **错误**：仅 §00 通用 `SeqCaps` 两条（`项数必须为正整数` / `序列不能为空`）；族内不产生自有 `Left`

## 本族的「共用骨架 + 独有分支」判定

- **独有实现**：`mrss121` 整条独立（`Expr` 树 + `mrssParse`/`mrssExpand`/`replaceLast`），不与图展开支共享任何内核函数。
- **共用骨架 + Kind 分叉**：`omega-y-sequence` 与 YMagma 族两条共用 `OmegaYSeq.hs` 全套骨架，差别只在 `Kind`（`KindSeq` / `KindMedium` / `KindStrong`）驱动的 phantom 取值、`eff` 计算、收集分派与是否截末项。
