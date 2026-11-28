# M1ynss（8 个 id：`-1y-(n+1)ss` `t--1y-(n+1)ss` `bt1ynss` `bt2ynss` `bt3ynss` `btlynss` `abs-bm1ynss` `rel-bm1ynss`）

**一句话导读**：这族是 `-1Y` 记号在「列 + 父标号」树上的复制展开——先建父表、定位根列、算提升向量，再按 `term` 次复制提升复制波；它是全注册表唯一的参数化族（`ParamCaps`），按 `docs/diagrams/04-kernel-flow/` 口径落在**第四链（序数系）**的旁支（父链整理 → 定位根 → 提升复制 → 拼接），但源码没有走 `parseCol` / `cmpOrd` 那套 `Expr` 列接口，而是在自有的嵌套树（`Node/Column`、`BtNode/BtColumn`）上本地实现父表比较，并与 `-1Y` 递归嵌套（`step` 子树 / `higher` 子树）纠缠。与矩阵系 Bms 相比，二者都是「定位坏根 + 复制坏部」，但 Bms 在矩阵上做列标准化与行补零，本族在树结构上做父表与层遍历。

## 共用骨架

1. **区间校验**：八个 id 全是 `ParamCaps`，`expandNotation` 先查 `nParam` 的 `(mn, mx)`，越界返回 `参数必须是 <mn> 到 <mx> 之间的整数`（`src/OmegaY/Notation/Types.hs:67-72`）；通过后才调族实现。区间两种：`(0, 25, 0, "n (0-25):")` 与 `(1, 25, 1, "n (1-25):")`，逐 id 见下。
2. **入口兜底**：各入口统一 `term' = max 1 term`（CLI 的 `--term 0` 或缺省 0 兜底为 1）；参数 `n` 在入口处再钳制一次（`max 0/min 25` 或 `max 1/min 25`），例外是 `Bt.expandBt` 不做内部钳制，区间只由 `Types` 把关。
3. **手写递归下降解析**：共用 `src/OmegaY/Fam/M1ynss/PP.hs` 的解析 monad（`runP` `sp` `peek` `atEnd` `numTok` `err`）；解析失败统一坍缩为 `kParseError` = `解析失败，请确认你没有使用全角括号，逗号`（`PP.hs:20`）；解析后必须吃完全文（`atEnd`），否则 `err`。
4. **展开内核**：父表 `parents`/`btComputeParents`/`computeParents` → 根 `root`/`btRoot`/`brRoot` → 提升向量 `ascensionVector`/`btAscensionVector`/`brAscensionVector` 与阈值树 `ascensionThresholds`/`btThresholds`/`brThresholds` → 按 `term'` 次折叠复制波（`addWave`/`btAscendReplace`/`brAscendReplace`/`ascendReplace`）；空表、非极限或无根时砍末列（`popLast`/`btPopLast`/`brPopLast`/`init`）。
5. **渲染输出**：`render`/`display`/`btDisplay`/`brDisplay` 把树拼回括号串，八 id 后缀全为 `""`。

**参数 `n` 是什么量**：`n` 是**每列父标号数组的宽度**（列内前 `n` 个整数槽位），不是展开层数、也不是嵌套深度；`term` 才是复制波次数（内核 `fs` 的 `index`）。嵌套深度由输入表达式本身的树结构决定，不由参数控制。

**族内四组结构**：
- `Flat`：平面列数组，`step` 是整数下标，无嵌套子表达式。
- `Tm1`：递归嵌套 `Node`，`step` 是子 `Node`；极限判定在 `step` 子树上做，`fs` 可下钻子树。
- `Bm1`：复用 `Tm1` 的 `Node` 与 Parser，父表顶层父分两支——`abs` 绝对比较 + 排除祖先链，`rel` 阈值树 + 相对 key + `fill_top`。
- `Bt` / `Bt3` / `Btl`：列子结构是 `higher` 数组（多个子表达式），显示 `*^expr` 前缀；`Bt1` 绝对父、`Bt2` 相对 key 父；`Bt3` 用竖直高度比较与 critical 列挑选；`Btl` 带 `mark` 字段与 `Lower/Higher/Star/Mark/MarkStar` 类型分派。

### `-1y-(n+1)ss` · (-1)Y-(n+1)SS 负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Flat.hs` · 参数化 `ParamCaps(0–25，默认 0)`，后缀 `""`
- **入口**：`Flat.expand`（`Flat.hs:206`），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：平面列序列 `(a,b,…)(c,d,…)`，每列前 `n` 个数为父标号、第 `n+1` 个数为 `step` 下标，加 `--term` → 同形态括号串
- **term / param 语义**：`term` = 复制波次数 `index`，`term' = max 1 term`（`Flat.hs:209`）；`param n` = 每列父标号 `arr` 长度，入口钳 `n = max 0 (min 25 n0)`（`Flat.hs:208`）
- **步骤**：
  1. 兜底 `n` / `term'`（`Flat.hs:208-209`）
  2. `runP text (parseExpr n)`（`Flat.hs:210`；`parseExpr` `Flat.hs:145`，`parseColumn` `Flat.hs:165`）
  3. `Left _` → `kParseError`（`Flat.hs:211`）
  4. `Right m` → `fs m term' n`（`Flat.hs:212`；`fs` `Flat.hs:109`）
  5. `fs`：`parents e n`（`Flat.hs:26`）建父表；`root pcols n`（`Flat.hs:68`）定位根；无根 → `init` 砍末列（`Flat.hs:115`）；有根 → `ascensionVector`（`Flat.hs:79`）、`ascensionThresholds`（`Flat.hs:86`），`foldl addWave` 复制 `index` 次（`Flat.hs:123`）
  6. `display`（`Flat.hs:142`）拼括号串
- **分叉**：全族唯一平面结构，无嵌套子树、无极限递归；`addWave` 在 `b == n` 时改写 `step`（`Flat.hs:129-131`）
- **错误**：区间越界 → `参数必须是 0 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Flat.hs:211`，文本见 `PP.hs:20`）

### `t--1y-(n+1)ss` · T(n+1)SS 超限负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Tm1.hs` · 参数化 `ParamCaps(0–25，默认 0)`，后缀 `""`
- **入口**：`Tm1.expandTm1`（`Tm1.hs:362`），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：嵌套列表达式，`step` 位置是子表达式（裸数字 / ω / `w` 或 `(...)` 嵌套），加 `--term` → 嵌套括号串；子层全零显示成数字、两列零/一显示 ω（`render` `Tm1.hs:236`）
- **term / param 语义**：`term' = max 1 term`（`Tm1.hs:365`）；`n` = `arr` 长度，钳 `max 0 (min 25 n0)`（`Tm1.hs:364`）
- **步骤**：
  1. 兜底 `n` / `term'`（`Tm1.hs:364-365`）
  2. `parseNode True n`（`Tm1.hs:368`；`parseNode` `Tm1.hs:259`，`parseColumn` `Tm1.hs:310`）；未吃完全文 → `err`（`Tm1.hs:371`）
  3. `Left _` → `kParseError`（`Tm1.hs:373`）
  4. `lnz m term' n`（`Tm1.hs:374`；`lnz` `Tm1.hs:228`）：`not isLimit` → `fs e 0 n`；否则 `fs e index n`
  5. `fs`（`Tm1.hs:188`）：`step` 子树 `isLimit`（`Tm1.hs:83`）→ 递归 `fs` 进 `cStep` 子树（`Tm1.hs:194-198`）；否则 `parents`（`Tm1.hs:103`）/ `root`（`Tm1.hs:144`）/ `ascensionVector`（`Tm1.hs:155`）/ `ascensionThresholds`（`Tm1.hs:162`）/ `addWave`（`Tm1.hs:214`）
  6. `render … True`（`Tm1.hs:236`）
- **分叉**：相对 `Flat` 多一层递归嵌套：极限判定落在 `step` 子树上，`fs` 可下钻子树；显示带 ω / 裸数字缩写
- **错误**：区间越界 → `参数必须是 0 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Tm1.hs:373`）

### `bt1ynss` · BT*(n+1)SS v1 超限星负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Bt.hs` · 参数化 `ParamCaps(1–25，默认 1)`，后缀 `""`
- **入口**：`Bt.expandBt Bt.Bt1`（`Bt.hs:637`；`Bt1` 变体由 `src/OmegaY/Fam/M1ynss/Family.hs:48` 传入），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：列带 `*^子表达式` `higher` 数组的 BT 表达式，加 `--term` → 同形态串；子层空显示 `*`（`btTopDisplay` `Bt.hs:105`）
- **term / param 语义**：`term' = max 1 term`（`Bt.hs:639`）；`n` = `arr` 宽度；注意 `expandBt` 入口**不做 `n` 的内部钳制**（`Bt.hs:638`），区间只由 `Types` 把关
- **步骤**：
  1. 兜底 `term'`（`Bt.hs:639`）
  2. `runP text (parseNode n)`（`Bt.hs:640`；`parseNode` `Bt.hs:644`，`parseColumn` `Bt.hs:671`，`readHigher` `Bt.hs:708`）；未吃完全文 → `kParseError`（`Bt.hs:642`）
  3. `btDisplay (btFs e term' n variant)`（`Bt.hs:641`；`btDisplay` `Bt.hs:110`）
  4. `btFs`（`Bt.hs:357`）：空表 → 原样；`not btIsLimit`（`Bt.hs:399`）→ `btPopLast`（`Bt.hs:230`）；否则 `btTailLayer`（`Bt.hs:267`）判特判：`isSpecial` → `btFsSpecial`（`Bt.hs:335`）；否则 `btComputeParents`（`Bt.hs:425`）→ `Bt1` 走 `bt1ComputeParents`（`Bt.hs:433`，`highestWithoutBase` 比较 + `forbidden` 祖先排除，`walkTopFn` `Bt.hs:473`）
  5. `btRoot`（`Bt.hs:306`）/ `btRootLayer`（`Bt.hs:286`）/ `btThresholds`（`Bt.hs:183`）/ `btAscensionVector`（`Bt.hs:317`）/ `btCloneSlice`（`Bt.hs:226`）/ `btAscendReplace`（`Bt.hs:241`），`addWave` 折叠 `[index…1]`（`Bt.hs:396`）
- **分叉**：BT 组共用 `btFs` 骨架；`Bt1` 父树求法 = 同趟下层父 + 顶层父（绝对 `higher` 比较 + 祖先链排除）
- **错误**：区间越界 → `参数必须是 1 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Bt.hs:642`）

### `bt2ynss` · strong BT*(n+1)SS v2 超限星负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Bt.hs` · 参数化 `ParamCaps(1–25，默认 1)`，后缀 `""`
- **入口**：`Bt.expandBt Bt.Bt2`（`Bt.hs:637`；`Family.hs:55` 传入 `Bt2`），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：同 `bt1ynss` 的 BT `higher` 数组表达式 + `--term` → 同形态串
- **term / param 语义**：`term' = max 1 term`（`Bt.hs:639`）；`n` = `arr` 宽度，入口不做内部钳制（`Bt.hs:638`）
- **步骤**：
  1. 兜底 `term'`、`parseNode n`、非全文结束判错，同 `bt1ynss` 第 1–3 步（`Bt.hs:639-642`）
  2. `btFs`（`Bt.hs:357`）骨架同 `bt1ynss`；分叉在 `btComputeParents` → `Bt2` 走 `bt2ComputeParents`（`Bt.hs:488`）：`bt2Lower`（`Bt.hs:493`）下层父 + `bt2FillTop`（`Bt.hs:595`）相对 key 填顶层父（`bt2ToRelative` `Bt.hs:540`、`bt2RelCompare` `Bt.hs:561`）
  3. 根 / 阈值 / 提升 / 复制波同 `bt1ynss` 第 5 步（`Bt.hs:306-396`）
- **分叉**：与 `bt1ynss` 共用 `btFs` 全部后续步骤，唯一结构级区别是顶层父用相对 key 比较，不用绝对 `higher` 比较
- **错误**：区间越界 → `参数必须是 1 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Bt.hs:642`）

### `bt3ynss` · weak BT*(n+1)SS v3 超限星负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Bt3.hs` · 参数化 `ParamCaps(1–25，默认 1)`，后缀 `""`；注册方式 `paramAlterNotation`（`Family.hs:57-64`），活入口是**第二参数**
- **入口**：`Bt3.expandBt3Alter`（`Bt3.hs:599`），由 `expandNotation` 的 `ParamCaps` 分支调用；第一参数 `Bt3.expandBt3`（`Bt3.hs:590`）**未接线**（`Types.hs:57-58` 丢弃 `_expand`）
- **输入 → 输出**：同 BT `higher` 数组表达式 + `--term` → 同形态串
- **term / param 语义**：`term' = max 1 term`（`Bt3.hs:602`）；`n` 钳 `max 1 (min 25 n0)`（`Bt3.hs:601`）
- **步骤**：
  1. 兜底 `n` / `term'`（`Bt3.hs:601-602`）
  2. `parseNode n`（`Bt3.hs:603`；`parseNode` `Bt3.hs:617`）；未吃完全文 → `kParseError`（`Bt3.hs:605`）
  3. `lnz True e term' n`（`Bt3.hs:604`；`lnz` `Bt3.hs:583`，`short_ = True`）→ `expand e index n True`（`Bt3.hs:497`，`lnzM1 = True` 启用 `skip1` / `skip2` 短分支，`Bt3.hs:523-562`）
  4. `expand`：`computeParents`（`Bt3.hs:117`，`lowerTree` + `computeTopParents` `Bt3.hs:135` 竖直高度比较 `hHeightCmp` `Bt3.hs:166`）；`computeTail`（`Bt3.hs:336`）；`root`（`Bt3.hs:376`）；`isSpecial`（`Bt3.hs:412`）→ `fsSpecial`（`Bt3.hs:424`）；否则 `thresholds`（`Bt3.hs:184`）/ `ascensionVector`（`Bt3.hs:398`）/ `computeVertical`（`Bt3.hs:253`）/ `ascendReplace`（`Bt3.hs:443`，`pickM` 按 `heightCmp` 挑 critical 列，`Bt3.hs:460-468`）
  5. `safeRun` 捕获运行时异常 → 返回空串（`Bt3.hs:609-614`）
  6. `display`（`Bt3.hs:44`）
- **分叉**：BT 组里唯一用竖直高度比较求父树；alter 支 `short_ = True` 走 lnz_m1 跳过批；`expandBt3`（`short_ = False`）是注册表里没接上的残枝
- **错误**：区间越界 → `参数必须是 1 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Bt3.hs:605`）；运行时异常被 `safeRun` 捕获后返回空串（`Bt3.hs:613`），不产生 `ERR:` 文本

### `btlynss` · BTL(n+1)SS 带标记超限星负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Btl.hs` · 参数化 `ParamCaps(1–25，默认 1)`，后缀 `""`
- **入口**：`Btl.expandBtl`（`Btl.hs:431`），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：列带 `mark` 标记与 `higher` 子树的 BTL 表达式（`(lower;mark^higher)`），加 `--term` → 同形态串
- **term / param 语义**：`term' = max 1 term`（`Btl.hs:434`）；`n` 钳 `max 1 (min 25 n0)`（`Btl.hs:433`）
- **步骤**：
  1. 兜底 `n` / `term'`（`Btl.hs:433-434`）
  2. `parseNode n`（`Btl.hs:435`；`parseNode` `Btl.hs:448`）；未吃完全文 → `kParseError`（`Btl.hs:437`）
  3. `lnz e term' n`（`Btl.hs:436`；`lnz` `Btl.hs:424`）：`not isLimit` → `fs e 0 n`；否则 `fs e index n`
  4. `fs`（`Btl.hs:370`）：`computeParents`（`Btl.hs:121`，`lowerEntry` `Btl.hs:144` + `typeInfo` `Btl.hs:159` 定 `BType`：`Lower/Higher/Star/Mark/MarkStar`）；`tailInfo`（`Btl.hs:209`）取末层类型；`Undef` → `popLast`（`Btl.hs:378`）；`Star` → `fsStar`（`Btl.hs:346`）；否则 `root`（`Btl.hs:227`）/ `rootLayer`（`Btl.hs:235`）/ `ascensionVector`（`Btl.hs:243`）/ `thresholds`（`Btl.hs:263`）/ `ascendReplace`（`Btl.hs:318`）；`Mark` 型单独切复制区（`Btl.hs:390-399`）
  5. `safeRun` 捕获异常 → `kParseError`（`Btl.hs:439-444`）
  6. `display`（`Btl.hs:46`）
- **分叉**：唯一带 `mark` 字段与类型分派；`Mark` 型复制区构造与 `Higher` / `Star` 不同
- **错误**：区间越界 → `参数必须是 1 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Btl.hs:437`）；`error "illegal tail"`（`Btl.hs:181`）在 `safeRun` 内被捕获并坍缩为 `kParseError`（`Btl.hs:443`）

### `abs-bm1ynss` · AbsB(n+1)SS 绝对分支负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Bm1.hs` · 参数化 `ParamCaps(0–25，默认 0)`，后缀 `""`
- **入口**：`Bm1.expandAbs`（`Bm1.hs:438`）= `expandBm False`（`Bm1.hs:419`），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：复用 `Tm1` 递归嵌套 `Node` 表达式 + `--term` → 嵌套串（`brRender` `Bm1.hs:402`）
- **term / param 语义**：`term' = max 1 term`（`Bm1.hs:422`）；`n` 钳 `max 0 (min 25 n0)`（`Bm1.hs:421`）
- **步骤**：
  1. 兜底 `n` / `term'`（`Bm1.hs:421-422`）
  2. `parseNode True n`（`Bm1.hs:425`；复用 `Tm1.parseNode`）；未吃完全文 → `err`（`Bm1.hs:428`）
  3. `Left _` → `kParseError`（`Bm1.hs:430`）
  4. 本地 `lnz`（`Bm1.hs:432-435`）：`not brIsLimit`（`Bm1.hs:19`）→ `brFs e 0 n rel`；否则 `brFs e idx n rel`
  5. `brFs`（`Bm1.hs:345`）：空表 → 原样；`not brIsLimit` → `brPopLast`（`Bm1.hs:298`）；否则 `brComputeParents`（`Bm1.hs:333`，`rel = False` → `brAbs` `Bm1.hs:82`：绝对 `compareNode` 比较 + `forbidden` 祖先链排除，`topWalk` `Bm1.hs:117`）；`brTailLayer`（`Bm1.hs:244`）/ `brRoot`（`Bm1.hs:271`）/ `brRootLayer`（`Bm1.hs:256`）/ `brThresholds`（`Bm1.hs:129`）/ `brAscensionVector`（`Bm1.hs:281`）/ `cloneSlice`（`Bm1.hs:294`）/ `brAscendReplace`（`Bm1.hs:307`），复制波折叠 `[index…1]`（`Bm1.hs:378`）
  6. `brRender … True`（`Bm1.hs:436`）
- **分叉**：与 `rel-bm1ynss` 共用 `brFs` 骨架，仅父表顶层父求法不同；`abs` 支排除祖先链
- **错误**：区间越界 → `参数必须是 0 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Bm1.hs:430`）

### `rel-bm1ynss` · RelB(n+1)SS 相对分支负一 Y 序列系统

- **族 / 模块**：M1ynss · `src/OmegaY/Fam/M1ynss/Bm1.hs` · 参数化 `ParamCaps(0–25，默认 0)`，后缀 `""`
- **入口**：`Bm1.expandRel`（`Bm1.hs:441`）= `expandBm True`（`Bm1.hs:419`），由 `expandNotation` 的 `ParamCaps` 分支调用
- **输入 → 输出**：同 `abs-bm1ynss` 的嵌套 `Node` 表达式 + `--term` → 嵌套串
- **term / param 语义**：`term' = max 1 term`（`Bm1.hs:422`）；`n` 钳 `max 0 (min 25 n0)`（`Bm1.hs:421`）
- **步骤**：
  1. 兜底、解析、本地 `lnz` 同 `abs-bm1ynss` 第 1–4 步（`Bm1.hs:421-435`）
  2. `brFs`（`Bm1.hs:345`）骨架同 `abs`；分叉在 `brComputeParents`：`rel = True` → `brLower`（`Bm1.hs:45`）下层父 + `brFillTop`（`Bm1.hs:215`）相对 key 填顶层父（`brToRelative` `Bm1.hs:169`、`brKeyCmp` `Bm1.hs:189`）
  3. 尾层 / 根 / 阈值 / 提升 / 复制波同 `abs-bm1ynss` 第 5 步（`Bm1.hs:244-378`）
- **分叉**：与 `abs-bm1ynss` 共用 `brFs` 全部后续步骤，唯一结构级区别是顶层父用相对 key（阈值树 + `fill_top`），不做绝对比较与祖先排除
- **错误**：区间越界 → `参数必须是 0 到 25 之间的整数`（`Types.hs:70-72`）；解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Bm1.hs:430`）
