# Bms（11 个 id：`bms-v1` `bms-v2` `bms-v21` `bms-v22` `bms-v23` `bms-v3` `bms-v31` `bms-v32` `bms-v33` `bms-v4` `upms`）

**一句话导读**：这族算的是矩阵系「标准形检查 → 取坏序列 → 复制坏部」的 BMS 一支，输入 `(a,b)(c,d)[n]` 矩阵文本，输出仍是 `(a,b)(c,d)` 矩阵文本；10 个 BMS 版本共用同一份骨架，只在**取坏序列**、**算 C 标记**、**复制回写**三处按 `Version` 分叉，`upms` 是另一条独立的坏根（后继型）分支。它对应 `docs/diagrams/04-kernel-flow/` 的**第一链（矩阵系）**：`parseMatrix` → 标准形检查 → `getBadSequence`（按版本选分支）→ `copyBadSequence` → `bmsExpand` 迭代 `term` 次 → `matrixToString`。

## 共用骨架

### A. 10 个 BMS 版本共用的文本外壳（`src/OmegaY/Fam/Bms/BmsText.hs`）

1. `bmsExpandTextWith ver text cliTerm`（`BmsText.hs:24`）：`parseMatrix text`（`BmsText.hs:25`；`src/OmegaY/Parse.hs:77`）得 `(m, mTerm)`，`[` 之后的十进制串即矩阵内自带 term。
2. `matEmpty m`（`BmsText.hs:26`）→ 返回 `kParseError`（`BmsText.hs:27`；文本见 §错误）。
3. `BC.bmsIsStandard ver m`（`BmsText.hs:28`；`src/OmegaY/Fam/Bms/BmsCore.hs:391`）→ 非标准返回 `kNonStandard`（`BmsText.hs:29`）。标准形检查内部同样调 `getBadSequence` / `copyBadSequence`，版本分叉口径与下文 `bmsExpand` 一致（`BmsCore.hs:441,447`）。
4. term 取值：`parsedTerm = mTerm` 有就用矩阵里的，没有回退 `cliTerm`（`BmsText.hs:31-33`）；`t = if parsedTerm < 1 then 1 else parsedTerm`（`BmsText.hs:34`）。
5. `BC.bmsExpand ver m t`（`BmsText.hs:35`；`BmsCore.hs:366`）。
6. `matrixToString result 0`（`BmsText.hs:36`；`Parse.hs:137`）——term 实参传 `0`，输出尾部**不追加** `[term]`。

### B. `bmsExpand` 内部共用段（`src/OmegaY/Fam/Bms/BmsCore.hs:366-386`）

1. `matRows m == 0 || matCols m == 0` → 原样返回 `m`（`BmsCore.hs:367`）。
2. `ver = verCode ver0`（`BmsCore.hs:369`；码表 `BmsCore.hs:17-27`）；`nr = matRows m`；`n = matCols m - 1`；`num = if term < 1 then 1 else term`（`BmsCore.hs:370-372`）；`delta0 = replicate nr 0`（`373`）；`c0` 零数组、`c1` 把第 1 列置 1（`376-377`）。
3. `getBadSequence ver m delta0 c1 n nr`（`BmsCore.hs:378`；定义 `84`）→ `(bad, delta1, c2)`。
4. `bad <= 0` → 截末列：列数 >1 取 `matResizeCols (matCols m - 1) m`，否则 `matResizeCols 0 m`（`BmsCore.hs:379-382`）。
5. 否则 `nn = n + bad * num`（`383`），`matResizeCols nn m`（`384`），`copyBadSequence ver s1 delta1 c2 n nn nr bad`（`BmsCore.hs:385`；定义 `313`），再 `matResizeCols nn s2`（`386`）。

### C. 三处版本分叉（10 个版本的差别只在这里）

**分叉点 1 · `getBadSequence`（`BmsCore.hs:86-90`）**：先看 `matAt s 0 n == 0`，是则 `(0, delta0, c0)`（`85`，全版本共用）；否则按 verCode：

| verCode | 分支函数 | 定义行 |
|---|---|---|
| 100 | `bs100` | `BmsCore.hs:100` |
| 210 | `bs210` | `BmsCore.hs:119` |
| 220 | `bs220` | `BmsCore.hs:143` |
| 其他 | `bsGeneric` | `BmsCore.hs:155` |

**分叉点 2 · `postProcess`（仅 `bsGeneric` 调用，`BmsCore.hs:178,184-191`）**：算 C 标记数组；非 generic 三支（100/210/220）不进这里，C 保持初值 `c0`。

| verCode | 分支 | 定义行 |
|---|---|---|
| 200 | `p200or310` | `BmsCore.hs:193` |
| 310 | `p310`（先 `p200or310` 再 `checkK` 清零） | `BmsCore.hs:221` |
| 230 | `p230or320` | `BmsCore.hs:210` |
| 320 | `p320`（先 `p230or320` 再 `checkK` 清零） | `BmsCore.hs:235` |
| 330 | `p330` | `BmsCore.hs:249` |
| 400 | `p400` | `BmsCore.hs:279` |
| 300 | `_` 分支：返回 `c0`，C 不被使用 | `BmsCore.hs:191` |

**分叉点 3 · `copyBadSequence`（`BmsCore.hs:315-325`）**：

| verCode | 回写循环 | 定义行 | 回写规则 |
|---|---|---|---|
| 100 / 210 / 220 | `loopS` | `BmsCore.hs:327` | 新值 = 源列值 + `delta !! l`，不用 C |
| 200 / 230 / 310 / 320 / 330 / 400 | `loopM` | `BmsCore.hs:334` | 新值 = 源列值 + `(delta !! l) * cGet c nr l m`，m 在 1..bad 轮转 |
| 300 | `loopK` | `BmsCore.hs:343` | `findM` 定位 mM，按行条件 `cond` 决定是否加 `delta`，k 在 1..bad 轮转 |

### D. 版本对照表（`verCode` 见 `BmsCore.hs:17-27`，注册见 `src/OmegaY/Fam/Bms/Family.hs:31-40`）

| id | Version | verCode | 分叉点 1 | 分叉点 2 | 分叉点 3 |
|---|---|---|---|---|---|
| `bms-v1` | V1 | 100 | `bs100` | 不调用（C 保持 `c0`） | `loopS` |
| `bms-v2` | V2 | 200 | `bsGeneric` | `p200or310` | `loopM` |
| `bms-v21` | V21 | 210 | `bs210` | 不调用（C 保持 `c0`） | `loopS` |
| `bms-v22` | V22 | 220 | `bs220` | 不调用（C 保持 `c0`） | `loopS` |
| `bms-v23` | V23 | 230 | `bsGeneric` | `p230or320` | `loopM` |
| `bms-v3` | V3 | 300 | `bsGeneric` | `_` → `c0`（C 不使用） | `loopK` |
| `bms-v31` | V31 | 310 | `bsGeneric` | `p310` | `loopM` |
| `bms-v32` | V32 | 320 | `bsGeneric` | `p320` | `loopM` |
| `bms-v33` | V33 | 330 | `bsGeneric` | `p330` | `loopM` |
| `bms-v4` | V4 | 400 | `bsGeneric` | `p400` | `loopM` |

---

### `bms-v1` · BMS v1.0

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V1`（`BmsText.hs:24`，注册 `Family.hs:31`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本（尾部不追加 `[term]`）
- **term / param 语义**：`term` = 坏部复制次数；矩阵内 `[n]` 优先于 `--term`，`< 1` 兜底为 1（`BmsText.hs:31-34`）；`param` 不参与
- **步骤**：
  1. 走 §共用骨架 A 第 1–4 步（解析 → 空矩阵 → 标准形 → 取 term）
  2. §共用骨架 B 第 1–2 步，`ver = verCode V1 = 100`（`BmsCore.hs:18,369`）
  3. 分叉点 1 走 `bs100`（`BmsCore.hs:100`）：`row = nr - 1`（`104`）；`mVal = findFirstZero s n row`（`105`；`findFirstZero` 定义 `93`）；`goK 0` 从 `k=0` 扫到 `n`，找第一个使前 `mVal-1` 行都满足 `matAt s l (n-k) < matAt s l n` 的 `k`（`110-111`）；`bad = k`，`delta'` 各行取末列减该列差、lnz 行取 0（`113-115`）；C 保持 `c0`
  4. 分叉点 3 走 `loopS`（`BmsCore.hs:327`）：逐列回写 `源列值 + (delta !! l)`（`330-332`）
  5. §共用骨架 B 第 5 步收尾，§A 第 6 步 `matrixToString`
- **分叉**：骨架 + 版本参数；`bs100` 属回写不用 C 的旧版三支之一（与 v21/v22 同走 `loopS`），取坏列按各行列差比较
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`BmsText.hs:18,27`）；`输入的矩阵为非标准表达式`（`BmsText.hs:21,29`）

### `bms-v2` · BMS v2.0

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V2`（`BmsText.hs:24`，注册 `Family.hs:32`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 200`（`BmsCore.hs:19,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）：`findBad` 用 `scanL` 自左扫列找 `bad` 与 `delta1`（`161-176`）
  3. 分叉点 2 走 `p200or310`（`BmsCore.hs:193`）：`stepK` 按 `k=1..bad` 填 C 第 `k+1` 列，`findL` 找左界（`196-208`），取值条件 `matAt s r (n-bad) < matAt s r (n-bad+k)` 且 C 左邻为 1
  4. 分叉点 3 走 `loopM`（`BmsCore.hs:334`）：回写 `源列值 + (delta !! l) * cGet c nr l m`（`337-339`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；首个 generic 取坏序列 + 按 C 标记加权回写的版本；C 算法就是 `p310` 的基底
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v21` · BMS v2.1

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V21`（`BmsText.hs:24`，注册 `Family.hs:33`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 210`（`BmsCore.hs:21,369`）
  2. 分叉点 1 走 `bs210`（`BmsCore.hs:119`）：`mVal = findFirstZero`（`124`）；`outer k curBad curDelta` 外层扫列（`125-137`）；`innerL k l` 先在第 0 行向左放宽出 `k1`（`138-141`），再 `all` 判定行差（`129`）；`delta` 同 `bs100` 口径（`131-135`）；C 保持 `c0`
  3. 分叉点 3 走 `loopS`（`BmsCore.hs:327`）
  4. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；取坏列前先在第 0 行 `innerL` 放宽步长，其余与 `bs100` 同构；回写不用 C
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v22` · BMS v2.2

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V22`（`BmsText.hs:24`，注册 `Family.hs:34`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 220`（`BmsCore.hs:22,369`）
  2. 分叉点 1 走 `bs220`（`BmsCore.hs:143`）：`mVal = findFirstZero`（`148`）；`l = getConcestor s mVal n`（`149`；`getConcestor` 定义 `69`）；`bad = if l >= 0 then n - l else 0`（`150`）；`delta'` 按列差（`151-153`）；C 保持 `c0`
  3. 分叉点 3 走 `loopS`（`BmsCore.hs:327`）
  4. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；坏列由「各层父项的公共祖先」`getConcestor` 决定，是 v22 独有判据；回写不用 C
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v23` · BMS v2.3

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V23`（`BmsText.hs:24`，注册 `Family.hs:35`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 230`（`BmsCore.hs:23,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）：`findBad` / `scanL` 取 `bad` 与 `delta1`（`161-176`）
  3. 分叉点 2 走 `p230or320`（`BmsCore.hs:210`）：双层循环按行 `j`、列 `k` 填 C，`goP` 沿 `getParentIB`（`54`）递推判 1/0（`214-219`）
  4. 分叉点 3 走 `loopM`（`BmsCore.hs:334`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；C 走 IB 祖先递推算法（`p320` 的基底），与 v2 的父项行比较算法不同
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v3` · BMS v3.0

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V3`（`BmsText.hs:24`，注册 `Family.hs:36`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 300`（`BmsCore.hs:24,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）：`findBad` / `scanL` 取 `bad` 与 `delta1`
  3. 分叉点 2 落 `_` 分支：`postProcess` 直接返回 `c0`，C 不被使用（`BmsCore.hs:191`）
  4. 分叉点 3 走 `loopK`（`BmsCore.hs:343`）：`findM` 定位 mM（`356-361`）；`cond = l == 0 || k == 1 || (matAt acc l (n-bad-mM) < matAt acc l (n-mM) && matAt acc l (n-mM) < matAt acc l n + (delta !! l))`（`349-351`），成立才加 `delta`；k 在 1..bad 轮转（`354`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；唯一走 `loopK` 的版本，回写条件不看 C，看相邻列的双行严格递增；C 数组虽按 200 路径算过但保持初值（`BmsCore.hs:191` 注释）
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v31` · BMS v3.1

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V31`（`BmsText.hs:24`，注册 `Family.hs:37`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 310`（`BmsCore.hs:25,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）
  3. 分叉点 2 走 `p310`（`BmsCore.hs:221`）：先 `p200or310 c0`（`222`）；`mTop = findMTop` 找最顶非零行（`223-227`）；`checkK` 若某列在上部任一行 C=0，则把该列除首行外整列清零（`228-233`）
  4. 分叉点 3 走 `loopM`（`BmsCore.hs:334`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；在 v2 的 `p200or310` 之上多加一轮「上部存在 0 即整列清零」的 `checkK` 修正
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v32` · BMS v3.2

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V32`（`BmsText.hs:24`，注册 `Family.hs:38`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 320`（`BmsCore.hs:26,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）
  3. 分叉点 2 走 `p320`（`BmsCore.hs:235`）：先 `p230or320 c0`（`236`）；`findMTop` + `checkK` 同样做上部 0 整列清零（`237-247`）
  4. 分叉点 3 走 `loopM`（`BmsCore.hs:334`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；在 v23 的 `p230or320` 之上多加一轮 `checkK` 清零修正，与 v31 对 v2 的关系对称
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v33` · BMS v3.3

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V33`（`BmsText.hs:24`，注册 `Family.hs:39`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 330`（`BmsCore.hs:27,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）
  3. 分叉点 2 走 `p330`（`BmsCore.hs:249`）：先把 C 全清零（`250-252`）；`lmost` 取最顶非零行（`253`），置该行第 1 列 = 1（`254`）；`c3` 按 `getParentIB` 逆扫 `k=bad-1..0` 沿父列置位（`255-261`）；`c4` 把 1 向下铺到 `lmost` 行（`262-267`）；`c5` 再对其余行按父列关系补位（`268-276`）
  4. 分叉点 3 走 `loopM`（`BmsCore.hs:334`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；C 算法为 v33 独有：先清零再从顶非零行单点向上/向下铺开
- **错误**：同 `bms-v1` 两条自有错误

### `bms-v4` · BMS v4.0

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/BmsCore.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.bmsExpandTextWith BC.V4`（`BmsText.hs:24`，注册 `Family.hs:40`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)[n]` 矩阵文本 + `--term` → `(a,b)(c,d)` 矩阵文本
- **term / param 语义**：同 `bms-v1`；`param` 不参与
- **步骤**：
  1. §共用骨架 A 第 1–4 步；§B 第 1–2 步，`ver = 400`（`BmsCore.hs:27,369`）
  2. 分叉点 1 走 `bsGeneric`（`BmsCore.hs:155`）
  3. 分叉点 2 走 `p400`（`BmsCore.hs:279`）：外层 `l <- [bad,bad-1..2]`、内层 `m <- [0..row]` 调 `processM`（`280-281`）；`findPPQ` 经 `goN2` / `goO`（`289-307`）找 `(pp, q)`；`colIdx = bad - pp + 1`，`newVal = if cGet cAcc nr m colIdx == 1 && q == m then 1 else 0`（`287-288`）
  4. 分叉点 3 走 `loopM`（`BmsCore.hs:334`）
  5. §B 第 5 步、§A 第 6 步收尾
- **分叉**：骨架 + 版本参数；C 算法为 v4 独有，按双层 `(l,m)` 与 `findPPQ` 比较结果逐格定 1/0
- **错误**：同 `bms-v1` 两条自有错误

### `upms` · UPMS

- **族 / 模块**：Bms · `src/OmegaY/Fam/Bms/UpmsCore.hs`（文本外壳 `src/OmegaY/Fam/Bms/BmsText.hs`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`BT.upmsExpandText`（`BmsText.hs:113`，注册 `Family.hs:25`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`(a,b)(c,d)` 矩阵文本（无 `[n]` 解析口径，见下）+ `--term` → `(a,b)(c,d)` 矩阵文本（列尾零按 `formatUpmsText` 裁掉）
- **term / param 语义**：`term` = 坏部复制次数 `h = 1..term`；与 10 个 BMS 版本不同，`term < 1` 直接返回空矩阵（`UpmsCore.hs:291`），不做 `max 1` 兜底；`param` 不参与
- **步骤**：
  1. `parseUpmsText text`（`BmsText.hs:114`；定义 `61`）逐字符扫描成 `Matrix`：按 `(` 切列、`)` 收列，数字位遇非数字不中断累加，`,` 重置当前值，空列补 0，短列尾部补 0（`70-93`）
  2. `matEmpty m`（`BmsText.hs:115`）→ `kParseError`（`116`）
  3. `UC.upmsExpand m term`（`BmsText.hs:118`；`UpmsCore.hs:289`）：
     1. 行列 0 → `emptyMatrix`（`290`）；`term < 1` → `emptyMatrix`（`291`）
     2. `cols0` 转列列表（`295`）；`lastAllZero = all (== 0) (last cols0)`（`297`）
     3. 末列全零（后继形）→ `cols1 = init cols0`，`colsToMatrix`（`300-301`）
     4. 否则 `findBadRoot cols0 M.empty`（`303`；定义 `76`）：`findT` 找末列最底非零行 `t`（`80-84`），`getBParent` 取坏根列 `root`（`88`；`27`）；`Nothing` → `emptyMatrix`（`305`）
     5. `Just (BadRoot root t)`：`gCols = take root cols0`（`307`），`bCols` 取坏部（`308`），`computeDelta`（`309`；`96`），`computeVr` 算坏部验证根缓存 `vr`（`310`；`117`）
     6. `resultCols = gCols ++ bCols ++ concat [generateBh bCols delta t h root vr | h <- [1..term]]`（`311-316`；`generateBh` 定义 `275`）；`colsToMatrix resultCols`（`317`；`320`）
  4. `formatUpmsText result`（`BmsText.hs:119`；定义 `96`）：每列裁到最后一个非零行再包括号；`null out` → `kUpmsEmpty`（`120-121`）
- **分叉**：**独立分支**，不经过 §共用骨架 A 的 `parseMatrix` / `bmsIsStandard` / `bmsExpand`，也不进 §B 的 `getBadSequence` / `copyBadSequence`；解析器是自己的 `parseUpmsText`，坏根取法是 `findBadRoot`（`getBParent` + 祖先缓存 `getAAncestors`），回写是 `generateBh` 按 `vr` 标记乘 `h * delta`
- **错误**：`解析失败，请确认你没有使用全角括号，逗号`（`BmsText.hs:18,116`）；`后继形展开为空矩阵，或未能定位坏根`（`BmsText.hs:49,121`）；`term < 1` 与坏根 `Nothing` 都落成空矩阵、空串，不产生独立错误文本

## 附：未接线的残枝

- `bmsTruncateFs`（`BmsText.hs:39`）与 `upmsTruncateFs`（`BmsText.hs:125`）只在本模块导出，`Family.hs` 未把它们接进任何 `nCaps`，不是入口函数。
