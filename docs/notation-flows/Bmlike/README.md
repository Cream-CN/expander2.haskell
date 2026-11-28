# Bmlike（9 个 id：`bsm` `bhm` `1-bm-bhm` `2-bm-bhm` `3-bm-bhm` `cms` `dsm` `blm` `wmm`）

**一句话导读**：这族是矩阵系里除 BMS 外的 BM 记号，共六套算法（BSM/BHM/CMS/DSM/BLM/wMM）9 个 id，输入 `(a,b)(c,d)` BM 矩阵文本，输出仍是 BM 矩阵文本；共用 `Share.hs` 的解析、父项链、比较与标准化工具，各 id 的坏根选取与回写算法各自独立。它对应 `docs/diagrams/04-kernel-flow/` 的**第一链（矩阵系）**：`parseMatrix`（本族是 `parseBm`）→ 标准/极限形检查 → 取坏根 → 复制坏部 → 矩阵序列化。

## 共用骨架

### A. 文本层共用判定（`src/OmegaY/Fam/Bmlike/Share.hs` 及各模块入口）

1. 入口先 `parseBm text`（`Share.hs:212`）：去首尾空白，按 `()` 切列、逗号切数；`Nothing` → `kParseError`（`Share.hs:34`，文本见 §错误）。
2. `Just e` 后 `matCols (beMat e) == 0` → 返回空串 `""`（各模块入口同款判定：`Bsm.hs:15`、`Bhm.hs:18`、`Cms.hs:18`、`Dsm.hs:21`、`Blm.hs:17`、`Wmm.hs:22`）。
3. 部分 id 再判 `isLimit m`（`Share.hs:135`：末列首行 > 0）；非 limit → `dropLastColumn m`（`Share.hs:139`）后 `matrixToBmString`（`Share.hs:270`）。**只有 `bhm` / `1..3-bm-bhm`（经 `fsWith`）/ `dsm` / `wmm` 走这条**；`bsm` / `cms` / `blm` 没有 `isLimit` 判定。
4. 进核心前 `standardizeMatrix 0`（`Share.hs:67`，行高不足补 0 行）；结果侧按需 `normalizeMatrix`（`Share.hs:54`，按各列高度裁行）。
5. 共用工具：父项链 `parentsOf`（`Share.hs:79`）、`parentAtOf`（`Share.hs:99`）、列/矩阵比较 `columnCompare`（`Share.hs:121`）/ `compareMat`（`Share.hs:125`）、`bm4Expand`（`Share.hs:165`）。
6. 输出：矩阵形走 `matrixToBmString`（`Share.hs:270`，空列显示 `(0)`）；列列表形走各模块自带 `colsToString`（口径相同）。九个 id 后缀都是 `""`（`Family.hs:15-26`）。

### B. 注册口径：哪个函数是活入口（`src/OmegaY/Fam/Bmlike/Family.hs:15-26`）

`textAlterNotation` 会丢弃第一个函数参数（见 `00-common.md` §6），活入口一律是**第二个**：

| id | 注册构造器 | 活入口（第二支） | 未接线的残枝（第一支） |
|---|---|---|---|
| `bsm` | `textAlterNotation`（`Family.hs:15`） | `Bsm.expandText`（两支同名同体） | 无独立残枝：第一支也是 `Bsm.expandText` |
| `bhm` | `textAlterNotation`（`Family.hs:16`） | `Bhm.expandAlter`（`Bhm.hs:29`） | `Bhm.expandFs`（`Bhm.hs:25`）未接线 |
| `1-bm-bhm` | `textAlterNotation`（`Family.hs:17-18`） | `Bhm.expandNAlter 1`（`Bhm.hs:37`） | `Bhm.expandNFs 1`（`Bhm.hs:33`）未接线 |
| `2-bm-bhm` | `textAlterNotation`（`Family.hs:19-20`） | `Bhm.expandNAlter 2`（`Bhm.hs:37`） | `Bhm.expandNFs 2`（`Bhm.hs:33`）未接线 |
| `3-bm-bhm` | `textAlterNotation`（`Family.hs:21-22`） | `Bhm.expandNAlter 3`（`Bhm.hs:37`） | `Bhm.expandNFs 3`（`Bhm.hs:33`）未接线 |
| `cms` | `textNotation`（`Family.hs:23`） | `Cms.expandText`（`Cms.hs:14`） | 无 |
| `dsm` | `textAlterNotation`（`Family.hs:24`） | `Dsm.expandAlter`（`Dsm.hs:14`） | `Dsm.expandFs`（`Dsm.hs:11`）未接线 |
| `blm` | `textNotation`（`Family.hs:25`） | `Blm.expandText`（`Blm.hs:13`） | 无 |
| `wmm` | `textAlterNotation`（`Family.hs:26`） | `Wmm.expandAlter`（`Wmm.hs:15`） | `Wmm.expandFs`（`Wmm.hs:12`）未接线 |

> 残枝与活入口的差别只在 `shorter` / `strong` 布尔位：FS 支传 `True`，alter 支传 `False`（`Bhm.hs:26,30,34,38`；`Dsm.hs:11,14`；`Wmm.hs:12,15`）。

---

### `bsm` · BSM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Bsm.hs` · 文本型 `TextCaps`（注册走 `textAlterNotation`，两支同为 `Bsm.expandText`，`Family.hs:15`），后缀 `""`
- **入口**：`Bsm.expandText`（`Bsm.hs:11`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = 坏部复制轮次，取 `if term < 1 then 1 else term`（`Bsm.hs:18`）；`param` 不参与
- **步骤**：
  1. `parseBm`（`Bsm.hs:12`）；`Nothing` → `kParseError`（`13`）；`matCols == 0` → `""`（`15`）
  2. `bsmExpand`（`18`；定义 `86`）：`standardizeMatrix 0`（`87`）；`nc == 0` 原样返回（`89-90`）；`expandBody`（`91`；定义 `94`）
  3. `expandBody`：`parentsOf` 建父项链（`99`）；`prefix` 截掉末列（`100-101`）；`lastNonzero` 取末列最底非零行 `lnz`（`103-109`）；`specialRoots` 沿 `lnz` 行父项链回溯（`112-116`）；`roots` 沿 `lnz-1` 行回溯后按父项属于 `specialRoots` 过滤（`119-127`）
  4. `lnz < 0` 或 `null roots` → `prefix`（`130-131`）；否则 `pickAndExpand`（`132,139`）
  5. `pickRoot`（`152`）逐个候选根比较：属 `specialRoots` 的走末行以外行值比对（`157-160`），其余走 `expansionAppend` 造候选段再 `compareMat cand threshold < 0`（`162-164`）；选定 `rActual` 后 `delta`（`142`；定义 `38`）、`expansion`（`143`；定义 `49`）按 `term` 轮次补列
  6. 顶行全零则裁掉（`145-147`），`normalizeMatrix`（`148`），`matrixToBmString`（`17`）
- **分叉**：共用骨架 A；**无 `isLimit` 判定**。选根判据是 BSM 独有（特殊根 / 普通根两套比较），回写用 `ascending`（`24`）沿父链判上升
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`Share.hs:34`，经 `Bsm.hs:13`）；空矩阵落成 `""`，无其它自有错误文本

### `bhm` · BHM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Bhm.hs` · 文本型 `TextCaps`（`textAlterNotation` 第二支，`Family.hs:16`），后缀 `""`
- **入口**：`Bhm.expandAlter`（`Bhm.hs:29`），由 `expandNotation` 的 `TextCaps` 分支调用；另一支 `Bhm.expandFs`（`Bhm.hs:25`）**未接线**
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = 展开轮次 `index`，`computeExpansion` 里 `w <- [1..index+1]`（`Bhm.hs:92`）；代码内不做 `max 1` 兜底；`param` 不参与
- **步骤**：
  1. `fsWith text term False core`（`30`；定义 `14`）：`parseBm`（`15`）；`Nothing` → `kParseError`（`16`）；`matCols == 0` → `""`（`18`）；`not (isLimit m)` → `matrixToBmString (dropLastColumn m)`（`19`）；否则 `matrixToBmString (normalizeMatrix (core m term False))`（`20`）
  2. core = `\m idx _ -> bhmExpand m idx False`（`30`）；`bhmExpand`（定义 `107`）：`standardizeMatrix 0`（`108`）；`nc == 0` 原样（`110-111`）；`body`（`112`；定义 `115`）
  3. `body`：`parentsOf`（`120`）；`pr = parentAtOf pTab right top`（`121`）；`specialRoot`（`122`）；`rawRoots` / `roots`（`124-131`）；`top < 0` 或 `null roots` → `dropLastColumn m`（`134-135`）
  4. `aMap = ascensionThresholds`（`138`；定义 `48`）；`vMap = ascensionVector`（`139`；定义 `68`）；`threshold = extend`（`140`；定义 `100`）；`pick`（`147`）按 `compareMat cand threshold < 0` 选根
  5. `computeExpansion`（`145`；定义 `84`）：基底列 + `ascendVector`（`73`）补列，`shorter=False` 保留最后一轮（`93-95`）
- **分叉**：共用骨架 A 的 `isLimit` 出口；选根与阈值表是 BHM 本体。alter 支 `shorter=False`；未接线的 `expandFs` 是 `shorter=True` 版
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`Share.hs:34`，经 `Bhm.hs:16`）；非 limit / 无坏根出口走 `dropLastColumn` 序列化，无独立错误文本

### `1-bm-bhm` · 1BM-BHM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Bhm.hs` · 文本型 `TextCaps`（`textAlterNotation` 第二支，`Family.hs:17-18`），后缀 `""`
- **入口**：`Bhm.expandNAlter 1`（`Bhm.hs:37`），由 `expandNotation` 的 `TextCaps` 分支调用；另一支 `Bhm.expandNFs 1`（`Bhm.hs:33`）**未接线**
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = 展开轮次 `index`；固定参数 `n = 1`；`param` 不参与
- **步骤**：
  1. `fsWith text term False (\m idx _ -> bmBhmExpand m idx 1 False)`（`38`）；`fsWith` 内判定同 `bhm` 第 1 步
  2. `bmBhmExpand`（定义 `157`）：`standardizeMatrix 0`（`158`）；`nc == 0` 原样（`159-160`）；`top < 0` → `dropLastColumn m`（`163-164`）
  3. 判据 `top < n`（`166`，`n = 1`）：成立 → `bm4Expand m index shorter`（`167`；`Share.hs:165`）；否则 → `bhmExpand m index shorter`（`168`，步骤同 `bhm` 第 3–5 步）
- **分叉**：在 BHM 本体前多一层 `top < n` 分流：行高不足固定阈值 `n=1` 时走 Share 的 `bm4Expand` 通用骨架，否则走 BHM 本体；`n` 在注册时写死
- **错误**：同 `bhm`（`kParseError` 经 `Bhm.hs:16`）

### `2-bm-bhm` · 2BM-BHM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Bhm.hs` · 文本型 `TextCaps`（`textAlterNotation` 第二支，`Family.hs:19-20`），后缀 `""`
- **入口**：`Bhm.expandNAlter 2`（`Bhm.hs:37`），由 `expandNotation` 的 `TextCaps` 分支调用；另一支 `Bhm.expandNFs 2`（`Bhm.hs:33`）**未接线**
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = 展开轮次 `index`；固定参数 `n = 2`；`param` 不参与
- **步骤**：
  1. `fsWith text term False (\m idx _ -> bmBhmExpand m idx 2 False)`（`38`）
  2. `bmBhmExpand`（`157`）：标准化、空矩阵、`top < 0` 判定同 `1-bm-bhm` 第 2 步
  3. 判据 `top < n`（`166`，`n = 2`）：成立 → `bm4Expand`（`167`）；否则 → `bhmExpand`（`168`）
- **分叉**：与 `1-bm-bhm` 同一份函数体，仅固定参数 `n = 2`，`top < 2` 才走 `bm4Expand`
- **错误**：同 `bhm`（`kParseError` 经 `Bhm.hs:16`）

### `3-bm-bhm` · 3BM-BHM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Bhm.hs` · 文本型 `TextCaps`（`textAlterNotation` 第二支，`Family.hs:21-22`），后缀 `""`
- **入口**：`Bhm.expandNAlter 3`（`Bhm.hs:37`），由 `expandNotation` 的 `TextCaps` 分支调用；另一支 `Bhm.expandNFs 3`（`Bhm.hs:33`）**未接线**
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = 展开轮次 `index`；固定参数 `n = 3`；`param` 不参与
- **步骤**：
  1. `fsWith text term False (\m idx _ -> bmBhmExpand m idx 3 False)`（`38`）
  2. `bmBhmExpand`（`157`）：标准化、空矩阵、`top < 0` 判定同 `1-bm-bhm` 第 2 步
  3. 判据 `top < n`（`166`，`n = 3`）：成立 → `bm4Expand`（`167`）；否则 → `bhmExpand`（`168`）
- **分叉**：与 `1-bm-bhm` 同一份函数体，仅固定参数 `n = 3`，`top < 3` 才走 `bm4Expand`
- **错误**：同 `bhm`（`kParseError` 经 `Bhm.hs:16`）

### `cms` · CMS

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Cms.hs` · 文本型 `TextCaps`（`textNotation`，`Family.hs:23`），后缀 `""`
- **入口**：`Cms.expandText`（`Cms.hs:14`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本（变长列，列尾零由 `normalizeCols` 裁掉）
- **term / param 语义**：`term` = 复制轮次 `fsTerm`；`fsTerm <= 0` 直接返回前缀列（`Cms.hs:110`），不做 `max 1` 兜底；`param` 不参与
- **步骤**：
  1. `parseBm`（`15`）；`Nothing` → `kParseError`（`16`）；`matCols == 0` → `""`（`18`）
  2. `expand (standardizeMatrix 0 m) term`（`20`；定义 `101`）→ `normalizeCols`（`25`）→ `colsToString`（`20,32`）
  3. `expand`：`m1 = matData input`（`121`）；`endcol = cols0 - 1`（`122`）；`child` 末列（`123`）；`lnz` 找末列最底非零行（`103-108`）
  4. `lnz < 0 || fsTerm <= 0` → `m2Prefix` 前缀列（`110-111`）；否则 `parentOf m1pc0 endcol lnz` 取坏根列 `br`（`114`；`parentOf` 定义 `52`）；`br < 0` → `[]`（`115-116`，C++ UB quirk，预言机观测为空）；否则 `mainBody`（`117`；定义 `127`）
  5. `mainBody`：`offset` 坏部列差（`127`）；`asc` 上升判定带 memo（`129-138`）；`pairs = n=1..fsTerm × col=br..endcol-1` 构造 `added` 列（`143-156`）；`m2 = 前缀 ++ added`（`158`）
  6. `colLoop` 从末列回扫找 `colFinal`，用 `lFunc`（`161-169`；`lFunc` 定义 `85`）判列是否保留；`colFinal == br` → `trimAtYmax m2`（`180-181`）；否则追加 `boosted` 提升末列（`184-190`），`buildD` 重写 `dCols`（`196-219`），`take c_ m2b ++ dCols`（`220`），再 `trimAtYmax`（`221`）
- **分叉**：共用骨架 A；**无 `isLimit` 判定、无 shorter 截断**。独有 PCache 父项缓存 `parentOf` 与 `lFunc` 列保留判据，回写按 `asc` 上升标记乘 `offset * n`
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`Share.hs:34`，经 `Cms.hs:16`）；`br < 0` 落成空列列表 → 空串，无独立错误文本

### `dsm` · DSM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Dsm.hs` · 文本型 `TextCaps`（`textAlterNotation` 第二支，`Family.hs:24`），后缀 `""`
- **入口**：`Dsm.expandAlter`（`Dsm.hs:14`），由 `expandNotation` 的 `TextCaps` 分支调用；另一支 `Dsm.expandFs`（`Dsm.hs:11`）**未接线**
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = 复制次数 `times`，`generateExpansion` 内 `addIter` / `addCols` 走 `[1..times]`（`Dsm.hs:117,142`）；`param` 不参与
- **步骤**：
  1. `run text term False`（`14`；定义 `17`）：`parseBm`（`18`）；`Nothing` → `kParseError`（`19`）；`matCols == 0` → `""`（`21`）；`not (isLimit m)` → `matrixToBmString (dropLastColumn m)`（`22`）；否则 `expandNormal (toCols m) term`（`24`；`toCols` 定义 `411`），`shorter && not (null longer)` → `init longer`（`26`，alter 支 `shorter=False` 不裁）；`colsToString`（`25,399`）
  2. `expandNormal`（定义 `416`）：`null matrix` → `[]`（`418`）；末列全零 → 去掉末列（`419`）；`getBadItemInfo matrix`（`421`；定义 `291`）；`Nothing` → `[]`（`422`）
  3. `Just (BadInfo br bc pcm)`：`constructMatrixValues (generateExpansion pcm br bc times False)`（`425`；`generateExpansion` 定义 `92`，`constructMatrixValues` 定义 `73`）；`init full`（`426`）；`normalizeCols`（`427`）
  4. `getBadItemInfo`：`buildPrm` 造行优先父项表 PRM（`309`）；`goT` 找 `targetRow`（`332`）；父列 `< 0` → `Nothing`（`305-306`）；`buildBad` 经 `getPredecessor`（`54`）展开前驱池（`343-348`），`findChosen` 按 `colsCompare candSeg standardSeg < 0` 选坏项（`372-387`；`colsCompare` 定义 `262`）
  5. `generateExpansion`：`targetRow == badRow` → `smallExpansion`（`110,115`）；否则 `fullExpansion`（`111,140`）；`strong=False`（`425` 传入）
- **分叉**：共用骨架 A 的 `isLimit` 出口；独有对角线父项表 PRM、前驱池 `getPredecessor` 与小/完整两套展开。alter 支 `strong=False`、`shorter=False`；未接线的 `expandFs` 是 `True` 版
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`Share.hs:34`，经 `Dsm.hs:19`）；`getBadItemInfo Nothing` 落成 `[]` → 空串，无独立错误文本

### `blm` · BLM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Blm.hs` · 文本型 `TextCaps`（`textNotation`，`Family.hs:25`），后缀 `""`
- **入口**：`Blm.expandText`（`Blm.hs:13`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本
- **term / param 语义**：`term` = `a`，第二轮逐列循环上界 `a * d7F`（`Blm.hs:237`）；`param` 不参与
- **步骤**：
  1. `parseBm`（`14`）；`Nothing` → `kParseError`（`15`）；`matCols == 0` → `""`（`17`）
  2. `blmExpand (standardizeMatrix 0 m) term`（`19`；定义 `44`）→ `normalizeCols`（`19,24`）→ `colsToString`（`19,31`）
  3. `blmExpand` 走 `runST`：`d3` = 末列号、`d2` = 末行号（`48-49`）；`gv` 越界读 0（`50-53`）；各 `STRef` / 共享 `STUArray` 初始化（`55-66`）
  4. `loopD4`（`75`）扫末列：首处正下方为 0 的行触发 `loopD5`；`loopD5`（`84`）× `loopD6`（`90`）建 `c` 阈值、`c2` 父列表、`b2` 标记；`loopD11`/`loopD12`/`loopD13`/`loopD14`（`130-172`）做二级父链判定；`loopD16`/`loopD17`（`174-212`）回溯修正标记
  5. 第二阶段：`cArr2` 按末列与根列差补 `c`（`217-229`）
  6. `loopD21`（`236`）：`d21 > a * d7F` 停（`237`）；逐列按 `b2Read` 与 `cArrF` 经 `applyD22`（`251`）造新列，`d3Ref` 递增（`270`）；`loopD21 1` 起（`273`），取回 `resultRef`（`274`）
- **分叉**：共用骨架 A 的解析/标准化；**无 `isLimit` 判定、无 shorter/strong 分支**。坏根分析与回写全部是 BLM 独有的 ST 可变数组多层循环（`loopD4`…`loopD21`），不复用 Share 的 `parentsOf` / `compareMat`
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`Share.hs:34`，经 `Blm.hs:15`）；无其它自有错误文本

### `wmm` · wMM

- **族 / 模块**：Bmlike · `src/OmegaY/Fam/Bmlike/Wmm.hs` · 文本型 `TextCaps`（`textAlterNotation` 第二支，`Family.hs:26`），后缀 `""`
- **入口**：`Wmm.expandAlter`（`Wmm.hs:15`），由 `expandNotation` 的 `TextCaps` 分支调用；另一支 `Wmm.expandFs`（`Wmm.hs:12`）**未接线**
- **输入 → 输出**：`(a,b)(c,d)` BM 矩阵文本 + `--term` → BM 矩阵文本（稀疏列网格，不存在的列不输出）
- **term / param 语义**：`term` = 迭代次数 `fsterm`，`loopN` 从 `1` 跑到 `fsterm`（`Wmm.hs:264`）；`param` 不参与
- **步骤**：
  1. `run text term False`（`15`；定义 `18`）：`parseBm`（`19`）；`Nothing` → `kParseError`（`20`）；`matCols == 0` → `""`（`22`）；`not (isLimit m)` → `matrixToBmString (dropLastColumn m)`（`23`）；否则 `wmmExpand m term`（`25`；定义 `192`）；`shorter` → `markLastNonexistent`（`26`；定义 `350`，alter 支 `False` 不调）；`gridToString`（`27,363`）
  2. `wmmExpand`：`m0` 包成稀疏 `Grid`（`196`）；`lnzX` / `lnzY` / `lnz` 取末列最底非零值（`198-200`）；`buildVertical` 建纵向序列 `vertM`（`203`；定义 `117`）
  3. `collection0` 沿 `parentOn` 父链收集末列各项到达 LNZ-1 的位置（`204-221`）；`counts` / `columns` / `r`（`224-228`）；`root` 按 `counts !! r == 1` 判单父或取最右收集列（`230-233`）；`width` / `height`（`235-236`）
  4. `a1` 把末列 LNZ 位减 1（`240-244`）；`a2 = appendRootSuffix` 把根列后缀拷到末列（`246-257`）
  5. `loopN a2 vertA 1`（`259`；定义 `263`）：`n > fsterm` 停；`oneIter`（`270`）：`referenceMap`（`274`；定义 `275`）按纵向比较建参照行映射，`addColumn`（`289`；定义 `290`）按 `[1..width]` 加列，`pushRow`（`298`）经 `ascGet`（`324`）判上升段取值
- **分叉**：共用骨架 A 的 `isLimit` 出口；独有稀疏 `Grid`、纵向序列 `verticalCompare` / `verticalIncrease`（`97,110`）与参照行映射。alter 支 `shorter=False`；未接线的 `expandFs` 是 `True` 版（会 `markLastNonexistent`）
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`Share.hs:34`，经 `Wmm.hs:20`）；无其它自有错误文本
