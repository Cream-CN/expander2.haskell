# Gms（17 个 id：`gms-gbms-omega-p` `gms-gbms-pqss` `gms-gbms-qss` `gms-gbms-full` `gms-gbms-weird` `gms-upms-omega-p` `gms-upms-pqss` `gms-upms-qss` `gms-upms-full` `gms-upms-weird` `gms-lpms2-omega-p` `gms-lpms2-pqss` `gms-lpms2-qss` `gms-lpms2-full` `gms-lpms2-weird` `lpms` `lptss`）

**一句话导读**：这族是 General Matrix System 的二维参数化矩阵展开——前 15 个是「底层系统 × 投影」3×5 笛卡尔积，共用同一份 `GmsCore` 内核；`lpms` / `lptss` 走另一条 `LpmsCore` 提升状态机路径。它对应 `docs/diagrams/04-kernel-flow/` 的**矩阵系**内核链（父项链 → 取坏部 → 复制提升 → 序列化），也是全仓库唯一跨族复用 `Bmlike.Share.parseBm` 的族（`Common.hs:31`；Bmlike 自己的 id 由另一册展开，本册不写）。

## 共用骨架

### A. 十五个 `runGms` 版本（`gms-*`）

1. `parseBmExpr`（`src/OmegaY/Fam/Gms/Common.hs:156`，即 `Bmlike.Share.parseBm`，`src/OmegaY/Fam/Bmlike/Share.hs:211`）解析 BM 矩阵文本；`Nothing` → `kParseError`（`src/OmegaY/Fam/Gms/GmsCore.hs:499`）。
2. `beMat e` 取矩形矩阵；`matCols m == 0` → 直接返回空串 `""`（`GmsCore.hs:502-503`）。
3. `fromMatrix m`（`Common.hs:150`）把矩形 Matrix 转成列向量表 `FMat = [Col]`。
4. `fs a term system proj`（`GmsCore.hs:506` → 定义 `GmsCore.hs:476`）：`index < 0` → `throwGms`（`GmsCore.hs:478`）；先 `rawAdjustedFs a 0` 取基准，再 `go` 按 `rawIndex` 递增取候选，用 `compareFor proj` 比较，相同则跳过、不同则接受为下一逻辑序号；`rawIndex > index + 10000` → `throwGms`（`GmsCore.hs:487`）。
5. `rawAdjustedFs`（`GmsCore.hs:468`）：`index >= 2` 时先取 `oldFiniteFs a 1` / `oldFiniteFs a 2`，按两者长度决定 `oldIndex`（`GmsCore.hs:470-472`）；再 `oldFiniteFs a oldIndex system`；按 `trimEnabled proj` 决定结果是否过 `trimZeroRows`（`Common.hs:126`）。
6. `oldFiniteFs`（`GmsCore.hs:382`）：空矩阵 → `[]`；末列全零（`isZeroCol`，`Common.hs:64`）→ 砍末列；否则 `prepareLimitContext a`（`GmsCore.hs:87`）建父项链与极限上下文（`computeParents`，`GmsCore.hs:21`）；`index == 0` 取 `lcAPrime`、`index == 1` 取 `lcFs1`；`index >= 2` 进入 **System 判据**（见下「分叉判据一」）。
7. 结果 `Just result` → `fmatToString result`（`Common.hs:136`）；`Nothing` → `kParseError`（`GmsCore.hs:508`）。

**分叉判据一：System**（在第 6 步 `oldFiniteFs` 内生效，`GmsCore.hs:399-409`）：

- `SysGBMS`（或 `lcK ctx <= 1`）：`buildGbmsD ctx`（`GmsCore.hs:128`）造 GBMS 增量列 `gbmsD`，交 `projectionRecurrence ctx gbmsD index`（`GmsCore.hs:149`）拼列。
- `SysUPMS`：`buildUpms ctx`（`GmsCore.hs:194`）造 `Y / v / D` 矩阵，交 `projectionRecurrence ctx (urD upms) index`。
- `SysLPMS2`：在 UPMS 结果上 `buildLpms2X ctx upms []`（`GmsCore.hs:302`）造 X 列，`next = lcFs1 ++ xMat`，再 `oldFiniteFs next (index - 1) system` 递归一轮。

**分叉判据二：Proj**（在第 4、5 步生效）：

- `trimEnabled proj`（`GmsCore.hs:456`）：仅 `ProjFull` / `ProjWeirdlyFull` 对结果 `trimZeroRows`（`GmsCore.hs:474`）。
- `compareFor proj`（`GmsCore.hs:459`）：仅 `ProjWeirdlyFull` 先比 `rowCount`（`Common.hs:50`），其余走 `matCompare`（`Common.hs:93`）。
- 注：`limitBuilder`（`GmsCore.hs:413`）、`limitFull`（`GmsCore.hs:425`）、`limitWeirdlyFull`（`GmsCore.hs:445`）在族内有定义，但 `runGms` 活路径上没有调用点，属未接线的残枝。

### B. 两个 `runLpms` 版本（`lpms` / `lptss`）

1. `parseBmExpr text`（`Common.hs:156`）；`Nothing` → `kParseError`（`src/OmegaY/Fam/Gms/LpmsCore.hs:683`）。
2. `beMat e`；`matCols m == 0` → 返回 `""`（`LpmsCore.hs:686-687`）。
3. `inp = fromMatrix m`（`Common.hs:150`）；`rows = maxRows inp`（`LpmsCore.hs:14`）；`std = standardize inp rows`（`LpmsCore.hs:17`）。
4. 预检 `legal inp && not (lastAllZero std)`（`LpmsCore.hs:692`；`legal` `LpmsCore.hs:39`、`lastAllZero` `LpmsCore.hs:61`）：不成立 → 砍末列 `dropped = init inp`，`fmatToString (standardize dropped rows)`（`LpmsCore.hs:693-695`）。
5. 成立 → `lpmsFs inp term`（`LpmsCore.hs:696` → 定义 `LpmsCore.hs:662`）：`n = max 0 n0`（`LpmsCore.hs:664`）；`legal` 失败 → `[]`；末列全零 → 砍末列；`rows < 3 || val (last m) 2 == 0` → `bmsFs m n rows`（`LpmsCore.hs:309`）；否则 `lpmsFs (lpmsSingle m) n` 递归（`LpmsCore.hs:678`）。
6. `lpmsSingle`（`LpmsCore.hs:451`）：`makePrep`（`LpmsCore.hs:203`）定位根列 `root` 与层数 `t`，失败 → `[]`；成功后 `foldl stepState`（`LpmsCore.hs:510`）跑提升状态机，收尾组装并 `standardize`。
7. 结果统一 `fmatToString`（`Common.hs:136`）序列化。

## 逐记号细目

### `gms-gbms-omega-p` · GMS GBMS ω-P

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `src/OmegaY/Fam/Gms/Family.hs:10-11`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysGBMS G.ProjOmegaP`（`Family.hs:11`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 `(a,b)(c,d)` + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = `fs` 的展开序号 `index`，原样传入（`GmsCore.hs:506`），无 `max 1` 兜底；`index < 0` 走 `throwGms`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysGBMS`（`GmsCore.hs:400-401`），Proj 判据为 `ProjOmegaP`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=GBMS 用 `buildGbmsD` 增量列，Proj=ω-P 走默认 `matCompare` 且不 `trimZeroRows`
- **错误**：解析失败或内核 `Nothing` → `解析失败，请确认你没有使用全角括号，逗号`（`GmsCore.hs:499` / `GmsCore.hs:508`，文本定义 `Bmlike/Share.hs:34`）；空矩阵 → 空串 `""`（`GmsCore.hs:503`），非错误；另见 §00 通用三分支（`TextCaps` 路径不做校验）

### `gms-gbms-pqss` · GMS GBMS pQSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:12-13`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysGBMS G.ProjPQSS`（`Family.hs:13`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysGBMS`，Proj 判据为 `ProjPQSS`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=GBMS，Proj=pQSS 走默认 `matCompare` 且不 `trimZeroRows`
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-gbms-qss` · GMS GBMS QSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:14-15`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysGBMS G.ProjQSS`（`Family.hs:15`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysGBMS`，Proj 判据为 `ProjQSS`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=GBMS，Proj=QSS 走默认 `matCompare` 且不 `trimZeroRows`
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-gbms-full` · GMS GBMS Full

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:16-17`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysGBMS G.ProjFull`（`Family.hs:17`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysGBMS`，Proj 判据为 `ProjFull`：结果过 `trimZeroRows`（`GmsCore.hs:474`，`trimEnabled` `GmsCore.hs:457`），比较仍用 `matCompare`（`GmsCore.hs:461`）
- **分叉**：骨架 + 参数差异；System=GBMS，Proj=Full 是两个会裁零行的投影之一
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-gbms-weird` · GMS GBMS Weirdly Full

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:18-19`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysGBMS G.ProjWeirdlyFull`（`Family.hs:19`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysGBMS`，Proj 判据为 `ProjWeirdlyFull`：结果裁零行（`GmsCore.hs:474`），且比较先比 `rowCount`、相同再 `matCompare`（`GmsCore.hs:463-466`）
- **分叉**：骨架 + 参数差异；System=GBMS，Proj=WeirdlyFull 是唯一改比较次序的投影，同时裁零行
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-upms-omega-p` · GMS UPMS ω-P

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:20-21`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysUPMS G.ProjOmegaP`（`Family.hs:21`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysUPMS`（`GmsCore.hs:403-405`），Proj 判据为 `ProjOmegaP`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=UPMS 先 `buildUpms` 造 `Y/v/D`，Proj=ω-P 走默认比较
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-upms-pqss` · GMS UPMS pQSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:22-23`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysUPMS G.ProjPQSS`（`Family.hs:23`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysUPMS`，Proj 判据为 `ProjPQSS`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=UPMS，Proj=pQSS 走默认比较
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-upms-qss` · GMS UPMS QSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:24-25`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysUPMS G.ProjQSS`（`Family.hs:25`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysUPMS`，Proj 判据为 `ProjQSS`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=UPMS，Proj=QSS 走默认比较
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-upms-full` · GMS UPMS Full

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:26-27`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysUPMS G.ProjFull`（`Family.hs:27`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysUPMS`，Proj 判据为 `ProjFull`：结果裁零行（`GmsCore.hs:474`），比较仍用 `matCompare`
- **分叉**：骨架 + 参数差异；System=UPMS，Proj=Full 裁零行
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-upms-weird` · GMS UPMS Weirdly Full

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:28-29`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysUPMS G.ProjWeirdlyFull`（`Family.hs:29`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysUPMS`，Proj 判据为 `ProjWeirdlyFull`：裁零行，比较先比 `rowCount`（`GmsCore.hs:463-466`）
- **分叉**：骨架 + 参数差异；System=UPMS，Proj=WeirdlyFull 改比较次序且裁零行
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-lpms2-omega-p` · GMS LPMS2 ω-P

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:30-31`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysLPMS2 G.ProjOmegaP`（`Family.hs:31`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysLPMS2`（`GmsCore.hs:407-409`）：`buildLpms2X` 造 X 列后 `oldFiniteFs` 递归一轮；Proj 判据为 `ProjOmegaP`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=LPMS2 独有「造 X 列 + 递归」支路，Proj=ω-P 走默认比较
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-lpms2-pqss` · GMS LPMS2 pQSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:32-33`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysLPMS2 G.ProjPQSS`（`Family.hs:33`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysLPMS2`，Proj 判据为 `ProjPQSS`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=LPMS2 递归支路，Proj=pQSS 走默认比较
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-lpms2-qss` · GMS LPMS2 QSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:34-35`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysLPMS2 G.ProjQSS`（`Family.hs:35`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysLPMS2`，Proj 判据为 `ProjQSS`（标准比较、不裁零行）
- **分叉**：骨架 + 参数差异；System=LPMS2 递归支路，Proj=QSS 走默认比较
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-lpms2-full` · GMS LPMS2 Full

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:36-37`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysLPMS2 G.ProjFull`（`Family.hs:37`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysLPMS2`，Proj 判据为 `ProjFull`：结果裁零行（`GmsCore.hs:474`），比较仍用 `matCompare`
- **分叉**：骨架 + 参数差异；System=LPMS2 递归支路，Proj=Full 裁零行
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `gms-lpms2-weird` · GMS LPMS2 Weirdly Full

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/GmsCore.hs`（注册 `Family.hs:38-39`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`GmsCore.runGms`（`GmsCore.hs:496`），注册闭包 `G.runGms G.SysLPMS2 G.ProjWeirdlyFull`（`Family.hs:39`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `gms-gbms-omega-p`，`term` = `fs` 的 `index`；`param` 不参与
- **步骤**：走 §共用骨架 A 第 1–7 步；System 判据取 `SysLPMS2`，Proj 判据为 `ProjWeirdlyFull`：裁零行，比较先比 `rowCount`（`GmsCore.hs:463-466`）
- **分叉**：骨架 + 参数差异；System=LPMS2 递归支路，Proj=WeirdlyFull 改比较次序且裁零行
- **错误**：同 `gms-gbms-omega-p`（`GmsCore.hs:499` / `GmsCore.hs:508` / `GmsCore.hs:503`）

### `lpms` · LPMS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/LpmsCore.hs`（注册 `Family.hs:40`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`LpmsCore.runLpms`（`LpmsCore.hs:680`），注册闭包 `L.runLpms False`（`Family.hs:40`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` 进入 `lpmsFs` 为 `n0`，`n = max 0 n0`（`LpmsCore.hs:664`）；`bmsFs` 用 `n` 作为 B 部复制上界（`LpmsCore.hs:336`）；`param` 不参与
- **步骤**：走 §共用骨架 B 第 1–7 步；`runLpms` 首参 `isLptss = False`（`Family.hs:40`）
- **分叉**：与 `lptss` 共用同一份 `LpmsCore` 引擎；源码里 `isLptss` 形参在 `runLpms` 函数体内未被读取（`LpmsCore.hs:681` 绑定后无引用），两条路径在当前实现下行为一致
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`LpmsCore.hs:683`，文本定义 `Bmlike/Share.hs:34`）；空矩阵 → 空串 `""`（`LpmsCore.hs:687`）；`legal` 失败 / `findRoot` / `makePrep` 失败时内部退化为 `[]` 或砍末列，不产生错误文本；另见 §00 通用三分支

### `lptss` · LPTSS

- **族 / 模块**：Gms · `src/OmegaY/Fam/Gms/LpmsCore.hs`（注册 `Family.hs:41`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`LpmsCore.runLpms`（`LpmsCore.hs:680`），注册闭包 `L.runLpms True`（`Family.hs:41`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：同 `lpms`，`term` → `lpmsFs` 的 `n0`，`n = max 0 n0`；`param` 不参与
- **步骤**：走 §共用骨架 B 第 1–7 步；`runLpms` 首参 `isLptss = True`（`Family.hs:41`）
- **分叉**：与 `lpms` 共用同一份 `LpmsCore` 引擎；`isLptss` 形参在函数体内未被读取（`LpmsCore.hs:681`），当前实现下与 `lpms` 无行为分叉
- **错误**：同 `lpms`（`LpmsCore.hs:683` / `LpmsCore.hs:687`）
