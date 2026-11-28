# Tree（5 个 id：`tbm` `tupms` `btbm` `btbm-weak` `bbm`）

**一句话导读**：这族是 BMS / UPMS 矩阵记号的**超限（transfinite）树形版本**——骨架仍是矩阵系那套「父项链 → 竖直序列 → 提升阈值 → 按 `term` 复制列」，但每个条目不再是单个整数，而是 `Entry { eValue, eHeight :: Expr }`，高度本身又是一棵可递归展开的表达式树；`btbm` / `btbm-weak` / `bbm` 进一步引入多层 Expr 导航与特例分支。它对应 `docs/diagrams/04-kernel-flow/` 的**矩阵系**骨架（父链 → 阈值 → 复制列），只是数据结构换成嵌套树，不另立独立内核链。

## 共用骨架

1. 入口解析：`pParse`（`src/OmegaY/Fam/Tree/Tbm.hs:439` / `src/OmegaY/Fam/Tree/Btbm.hs:599`）把 `(v^高度)(v,...)` 文本解析成 `Expr`；解析失败或有残留 → `kParseError`。
2. FS 包装：`tbmFs`（`Tbm.hs:322`）/ `tupmsFs`（`src/OmegaY/Fam/Tree/Tupms.hs:268`）/ `fs`（`Btbm.hs:323`）先判 `isLimit`：非极限传 `index 0`、极限传 `index` 给本体展开。
3. 本体二分（`expand` / `tuExpand` / `fsCoreWith`）：末列为空 → 砍末列；否则看末条目的高度 `isOne`：后继情形走复制列支路，极限情形把高度递归展开后接回。
4. 后继支路统一动作：`columnVerticals`（`Tbm.hs:132`）取竖直序列、`parents`（`Tbm.hs:142`）求父项链、`columnSub`（`Tbm.hs:193`）求偏移、阈值函数定各列提升上界、`copyColumn`（`Tbm.hs:265`）按 `w ∈ [1..index]` 复制列。
5. 极限支路统一动作：取末条目高度，递归跑 FS 包装得 `newH`，`toVertical`（`Tbm.hs:184`）切成竖直段，替换末列尾部条目。
6. 输出：`display`（`Tbm.hs:329` / `Btbm.hs:490`）把 `Expr` 拼回 `(v^...)(...)` 文本；五个 id 后缀都是 `""`。

### 与 Bms 的对应关系（以源码为准）

- Bms 矩阵的条目是 `Int`；本族 `Entry` 多挂一个 `eHeight :: Expr`（`Tbm.hs:40`、`Btbm.hs:18-21`），这是「超限」的额外结构：极限支路递归展开的就是这个内嵌高度。
- `tbm` ↔ BMS 系：后继/极限二分、父链、阈值、复制列一一对应；`tupms` ↔ UPMS 系：阈值判定换成带最小堆与延迟位置的 `tupmsAscensionThreshold`（`Tupms.hs:59`）。
- `btbm` / `btbm-weak` / `bbm` 在单层树之上再加**多层 Expr**：`nextLayer` / `skipLayers` / `tailLayer` / `tailCol` / `rootLayer`（`Btbm.hs:115-141`）定位根层与尾层，并各自带一个特例判据（`isSpecial` / `bIsSpecial`）。

## 逐记号细目

### `tbm` · TBM 超限巴斯丘矩阵

- **族 / 模块**：Tree · `src/OmegaY/Fam/Tree/Tbm.hs`（注册 `src/OmegaY/Fam/Tree/Family.hs:12`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Tbm.expandText`（`Tbm.hs:454`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：树形矩阵文本 `(v^高度)(v,...)` + `--term` → 同形态树形矩阵文本
- **term / param 语义**：`term` = 后继支路的复制层数 `index`（`copyColumn` 里 `w ∈ [1..index]`，`Tbm.hs:293`）；非极限表达式按 `index 0` 展开（`Tbm.hs:324`）；`param` 不参与
- **步骤**：
  1. `pParse` 解析（`Tbm.hs:456`）；失败 → `kParseError`（`Tbm.hs:458`）
  2. `tbmFs e term`（`Tbm.hs:457` → `Tbm.hs:322`）：`isLimit e`（`Tbm.hs:271`）? 极限走 `expand e term`，否则 `expand e 0`
  3. `expand`（`Tbm.hs:308`）：空 → 原样；末列空 → `init`；末条目高度 `isOne`（`Tbm.hs:48`）→ `expandSuccessor`，否则 `expandLimit`
  4. `expandSuccessor`（`Tbm.hs:277`）：`columnVerticals` 取竖直序列、`parents` 求父项链定 `r`；`columnSub` 求偏移；`ascensionThreshold`（`Tbm.hs:209`）定阈值；`copyColumn` 按 `w ∈ [1..index]`、`i ∈ [r..n-1]` 追加列
  5. `expandLimit`（`Tbm.hs:297`）：`newH = tbmFs (eHeight 末条目) index`；`toVertical` 切段后替换末列尾部
  6. `display` 序列化（`Tbm.hs:329`）
- **分叉**：全族共用骨架第 1–6 步；阈值用标准 `ascensionThreshold`，无堆/延迟判定，无多层导航
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Tbm.hs:458`，文本定义 `Tbm.hs:53`）；仅 §00 通用三分支（`TextCaps` 路径不做校验）

### `tupms` · TUPMS 超限 UPMS

- **族 / 模块**：Tree · `src/OmegaY/Fam/Tree/Tupms.hs`（注册 `Family.hs:13`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Tupms.expandText`（`Tupms.hs:272`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：树形矩阵文本 + `--term` → 同形态树形矩阵文本
- **term / param 语义**：`term` = 后继支路复制层数 `index`；与 `tbmFs` 不同，`tupmsFs` 不做 `isLimit` 分流，恒把 `index` 传给 `tuExpand`（`Tupms.hs:269-270`）；`param` 不参与
- **步骤**：
  1. `parseExprText` 解析（`Tupms.hs:274`，定义 `Tbm.hs:449`）；失败 → `kParseError`（`Tupms.hs:276`）
  2. `tupmsFs e term`（`Tupms.hs:275` → `Tupms.hs:268`）：直接 `tuExpand e index`
  3. `tuExpand`（`Tupms.hs:254`）：末列空 → `init`；末条目高度 `isOne` → `tuExpandSuccessor`，否则 `tuExpandLimit`
  4. `tuExpandSuccessor`（`Tupms.hs:223`）：父链/竖直序列/偏移同骨架；阈值改用 `tupmsAscensionThreshold`（`Tupms.hs:236` → `Tupms.hs:59`）：最小堆 `heapPop`（`Tupms.hs:36`）扫列，`decideUpgrade`（`Tupms.hs:144`）做 UPMS 提升判定，`compareLoop`（`Tupms.hs:192`）逐段比较 X/Y 变换列；再 `copyColumn` 按 `w ∈ [1..index]` 追加
  5. `tuExpandLimit`（`Tupms.hs:243`）：`newH = tupmsFs (eHeight 末条目) index`，`toVertical` 切段接回
  6. `display` 序列化（复用 `Tbm.display`，`Tupms.hs:27` 导入）
- **分叉**：骨架 + 独立阈值支路；`tupmsAscensionThreshold` 是本 id 独有实现（堆 + 延迟位置 + UPMS 比较），其余父链/复制/极限递归动作与 `tbm` 同构
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Tupms.hs:276`，文本经 `Tbm.kParseError`，`Tbm.hs:53`）；仅 §00 通用三分支

### `btbm` · BTBM Bubby3 超限巴斯丘矩阵

- **族 / 模块**：Tree · `src/OmegaY/Fam/Tree/Btbm.hs`（注册 `Family.hs:14`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Btbm.btbmExpandText`（`Btbm.hs:617`），由 `expandNotation` 的 `TextCaps` 分支调用；内部经 `runNotation`（`Btbm.hs:611`）调 `fs isSpecial expandSpecial`（`Btbm.hs:618`）
- **输入 → 输出**：多层树形矩阵文本（解析时值 `-1`，显示时 `+1`，`Btbm.hs:581` / `Btbm.hs:499`）+ `--term` → 同形态文本
- **term / param 语义**：`term` = 特例支路 `expandSpecial` 的复制基数与标准支路 `buildTail` 的层数 `index`；非极限按 `index 0`（`Btbm.hs:325`）；`param` 不参与
- **步骤**：
  1. `runNotation` 内 `pParse` 解析（`Btbm.hs:613`）；失败或有残留 → `kParseError`（`Btbm.hs:615`）
  2. `fs`（`Btbm.hs:323`）：`isLimit e`（`Btbm.hs:248`）? `fsCoreWith isSpecial expandSpecial e index`，否则同函数 `index 0`
  3. `fsCoreWith`（`Btbm.hs:252`）：`eCols` 空 → 原样；`tailLayer e`（`Btbm.hs:121`）`< 0` → 砍末列；`isSpecial e tLayer`（`Btbm.hs:164`）成立 → `expandSpecial`（`Btbm.hs:179`），否则 `standardCase`（`Btbm.hs:263`）
  4. `standardCase`（`Btbm.hs:263`）：`tailCol` / `root` / `rootLayer` 定位根列与尾列，`rootAppendingStart`（`Btbm.hs:196`）定追加起点；`ascendReplace`（`Btbm.hs:145`）整体替换提升，`buildTail`（`Btbm.hs:279`）按 `j ∈ [1..index]` 拼尾列
  5. `expandSpecial`（`Btbm.hs:179`）：逐层递归特例，末层 `replicate index` 复制基条目
  6. `display` 序列化（`Btbm.hs:490`）
- **分叉**：骨架 + 多层导航与特例分支；判据 `isSpecial` 与替换 `expandSpecial` 为本 id 接入的版本，标准支路 `standardCase` 用 `ascendReplace`
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Btbm.hs:615`，文本定义 `Btbm.hs:37`）；仅 §00 通用三分支

### `btbm-weak` · BTBM-weak 弱版 Bubby3 超限巴斯丘矩阵

- **族 / 模块**：Tree · `src/OmegaY/Fam/Tree/Btbm.hs`（注册 `Family.hs:15`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Btbm.btbwExpandText`（`Btbm.hs:620`），由 `expandNotation` 的 `TextCaps` 分支调用；内部经 `runNotation`（`Btbm.hs:611`）调 `wFs`（`Btbm.hs:621`）
- **输入 → 输出**：多层树形矩阵文本 + `--term` → 同形态文本
- **term / param 语义**：`term` = `weakCase` 里 `buildTail` 的层数 `index` 与特例复制基数；`param` 不参与
- **步骤**：
  1. `runNotation` 内 `pParse` 解析（`Btbm.hs:613`）；失败 → `kParseError`（`Btbm.hs:615`）
  2. `wFs`（`Btbm.hs:389`）：`eCols` 空 → 原样；`tailLayer < 0` → 砍末列；`isSpecial e tLayer` 成立 → `expandSpecial`（`Btbm.hs:397`），否则 `weakCase`（`Btbm.hs:400`）
  3. `weakCase`（`Btbm.hs:400`）：定位根/尾列后，先由尾列各条目高度 `toHeight` 收集 `criticalVert`（`Btbm.hs:412-414`）；`wAscendReplace`（`Btbm.hs:330`）按临界竖直序列逐块替换提升（超过临界的条目补 `criticalVert` 剩余段，`popLoop` 去重复值）；`buildTail`（`Btbm.hs:419`）按 `j ∈ [1..index]` 拼尾列
  4. 特例支路同 `btbm` 的 `expandSpecial`（`Btbm.hs:179`）
  5. `display` 序列化（`Btbm.hs:490`）
- **分叉**：与 `btbm` 共用多层导航与特例判据；标准支路换成弱版 `weakCase` / `wAscendReplace`（带临界竖直序列与弹出去重），不走 `standardCase` / `ascendReplace`；且不经 `fs` 的 `isLimit` 分流，直接进 `wFs`
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Btbm.hs:615`，文本定义 `Btbm.hs:37`）；仅 §00 通用三分支

### `bbm` · BBM 分支巴斯丘矩阵

- **族 / 模块**：Tree · `src/OmegaY/Fam/Tree/Btbm.hs`（注册 `Family.hs:16`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Btbm.bbmExpandText`（`Btbm.hs:623`），由 `expandNotation` 的 `TextCaps` 分支调用；内部经 `runNotation`（`Btbm.hs:611`）调 `fs bIsSpecial bExpandSpecial`（`Btbm.hs:624`）
- **输入 → 输出**：多层树形矩阵文本 + `--term` → 同形态文本
- **term / param 语义**：`term` = 特例支路 `bExpandSpecial` 的复制基数与标准支路 `buildTail` 的层数 `index`；非极限按 `index 0`（`Btbm.hs:325`）；`param` 不参与
- **步骤**：
  1. `runNotation` 内 `pParse` 解析（`Btbm.hs:613`）；失败 → `kParseError`（`Btbm.hs:615`）
  2. `fs`（`Btbm.hs:323`）：`isLimit e`? `fsCoreWith bIsSpecial bExpandSpecial e index`，否则 `index 0`
  3. `fsCoreWith`（`Btbm.hs:252`）：`tailLayer < 0` → 砍末列；`bIsSpecial e tLayer`（`Btbm.hs:462`）成立 → `bExpandSpecial`（`Btbm.hs:473`），否则 `standardCase`（`Btbm.hs:263`）
  4. `bIsSpecial` 判据（`Btbm.hs:462`）：跳到 `tLayer` 层后还有下一层，且下一层末列为空；`bExpandSpecial`（`Btbm.hs:473`）按 `tLayer > 0` 递归、末层 `replicate index` 复制基条目
  5. 标准支路同 `btbm` 的 `standardCase`（`Btbm.hs:263`）：`ascendReplace` + `buildTail`
  6. `display` 序列化（`Btbm.hs:490`）
- **分叉**：与 `btbm` 共用 `fs` / `fsCoreWith` / `standardCase` 骨架；特例判据换成 `bIsSpecial`、特例展开换成 `bExpandSpecial`（判定目标是「下一层末列为空」，递归层数条件为 `tLayer > 0`）
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Btbm.hs:615`，文本定义 `Btbm.hs:37`）；仅 §00 通用三分支
