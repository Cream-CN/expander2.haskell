# Xy（3 个 id：`xy-v1` `xy-v2` `xy-v3`）

**一句话导读**：这族把整数序列建成「山」（entry 节点 + parent/head/foot 链），按 `term` 轮次把坏部所在列复制延展后再取各列顶值，输出仍是逗号分隔整数序列；它对应
`docs/diagrams/04-kernel-flow/` 的**第三链（X-Y 系）**——`World` arena → 建树 → 取主维/参考链 → 记忆化（v2）或递归（v1/v3）展开 → 回吐序列；其中 `xy-v1` 同时被**第五链（基础序列系）**提及（`docs/diagrams/README.md` 内核链 5）。

## 共用骨架

1. `expandNotation`（`src/OmegaY/Notation/Types.hs:61`）的 `SeqCaps` 分支先做三件事：`term < 1` 拦下（`Types.hs:75`）、`parseSequence`（`src/OmegaY/Parse.hs:16`）切序列、空则拦下（`Types.hs:79`）。三个 id 全是 `SeqCaps`，没有 `TextCaps` 那条不校验路径。
2. 入口守门：`term < 1 || null seq` → 原样回显；否则 `term'` 兜底并截断到 10 轮（v1/v2 `min term 10`，v3 `if term > 10 then 10 else term`）。
3. 建 arena：v1 / v2 共用 `Common.newWorld`（`src/OmegaY/Fam/Xy/Common.hs:55`）；v3 自带一份 `XyV3.newWorld`（`src/OmegaY/Fam/Xy/XyV3.hs:62`），不复用 Common。
4. 整数序列 → entry 序列：v1 / v2 走 `toSequenceV12`（`Common.hs:167`），v3 走 `toSequenceV3`（`XyV3.hs:298`）；parent 一律指向左侧最近的较小元素，找不到则挂合成根。
5. 展开内核：画山 → 取坏根 `b` 与顶 `t` → 主维/参考链处理 → 按 `term'` 轮次复制列 → 输出各列顶值。**第 5 步起三份实现各自独立**：不是「同一个展开函数 + 版本开关」，而是 v1 / v2 / v3 三份模块各自定义 `expandImpl`（v3 名 `expand_impl`），只在 v1/v2 之间共享 Common 的 arena 与建树工具。

### `xy-v1` · X-Y V1（idOrder 第 66 位）

- **族 / 模块**：Xy · `src/OmegaY/Fam/Xy/XyV1.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandXyV1`（`XyV1.hs:427`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 展开轮次，`term' = min term 10`（`XyV1.hs:430`）；`term < 1` 或空序列在入口直接原样回显（`XyV1.hs:428`）；`param` 不参与（`SeqCaps` 分支不看 `nParam`）
- **步骤**：
  1. 守门 `term < 1 || null seq = seq`（`XyV1.hs:428`）；算 `term'`（`XyV1.hs:430`）
  2. `newWorld`（`Common.hs:55`）建槽位 arena
  3. `toSequenceV12`（`Common.hs:167`）把序列建成 entry 序列
  4. `expandImpl w s term' dXY`（`XyV1.hs:261`，`dXY = [1,0,1]`，`XyV1.hs:19`）：
     - `null s0` → `[]`（`XyV1.hs:262-263`）；末项 `e_value <= 1` → `mapM (valOf w) (init s0)`（`XyV1.hs:265-267`）
     - `mref <- newSTRef (map (: []) s0)`（`XyV1.hs:269`）；`drawMountain`（`XyV1.hs:37`）逐列 `climb` 推脚，返回各列顶 id
     - 取 `topLast`，按 `e_head` / `e_parent` 得 `t`、`bId0`；`fetchDimensionSequence`（`XyV1.hs:89`）取主维；`expandDimensionSequence`（`XyV1.hs:120`）在维度序列上递归
     - 末列顶值 >1：`buildOuter`（`XyV1.hs:320`）建外层节点序列后递归 `expandImpl`（`XyV1.hs:292`）算 `ex`；否则弹出末列顶、`e_foot = Nothing`（`XyV1.hs:301-305`）
     - `bId == -1` → 取 init 各值（`XyV1.hs:306-307`）；否则 `preprocess`（`XyV1.hs:141`）给 b 列之后编号，末列元素值减一（`XyV1.hs:312`），`copyCol`（`XyV1.hs:336`）按 `[0..n-1]` 轮 × `[bCloumn+1..tCloumn]` 列复制，最后 `mapM (valOf w . head) mFinal`（`XyV1.hs:317`）
- **分叉**：与 §共用骨架 共用第 1–4 步。v1 独有的一套：`drawMountain` 返回各列顶、`fetchDimensionSequence` / `expandDimensionSequence`、`calcMaxCopyrow`（`XyV1.hs:163`）、`copyItem`（`XyV1.hs:217`）、`copyRowElt`（`XyV1.hs:364`）、`fixParent`（`XyV1.hs:407`）、`buildOuter`；没有 memo，也没有参考链字段（`e_chain` 不参与本路径）
- **错误**：仅 §00 通用三分支里的 `SeqCaps` 两条（`项数必须为正整数` / `序列不能为空`）；族内无自有 `Left` 或运行时错误

### `xy-v2` · X-Y V2（idOrder 第 67 位）

- **族 / 模块**：Xy · `src/OmegaY/Fam/Xy/XyV2.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandXyV2`（`XyV2.hs:718`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 展开轮次，`term' = min term 10`（`XyV2.hs:721`）；`term < 1` 或空序列原样回显（`XyV2.hs:719`）；`param` 不参与
- **步骤**：
  1. 守门（`XyV2.hs:719`）；算 `term'`（`XyV2.hs:721`）
  2. `newWorld`（`Common.hs:55`）；`toSequenceV12`（`Common.hs:167`）建树
  3. `memo <- newSTRef []`（`XyV2.hs:724`）
  4. `expandImpl w s term' dXY memo`（`XyV2.hs:562`，`dXY` 见 `XyV2.hs:20`）：
     - `null s0` / 末项 `e_value <= 1` 短路取 init 各值（`XyV2.hs:563-569`）
     - `getBootCloumn`（`XyV2.hs:225`）：自画一座山，取末列倒数第二的 parent 列；行列等长或 `d` 短于 3 直接返回，否则 `getReferenceChain`（`XyV2.hs:130`）+ `getMainDimensionSequence`（`XyV2.hs:194`）递归
     - 按 parent 列重建 `mref`（`XyV2.hs:574-585`）；`drawMountain`（`XyV2.hs:35`）就地改 mref，不返回 tops
     - 末列少于 2 或 `t` 无 parent → 取 init 各值（`XyV2.hs:588-595`）；`bId` 可上移到启动列 `c`（`XyV2.hs:598-601`），算 `len`；`setElementNo`（`XyV2.hs:90`）编号
     - `length d > 2`：`getReferenceChain` 结果记入 `e_chain`（`XyV2.hs:609-615`）；`expandElementDimensionSequence`（`XyV2.hs:462`）先用 `findMemo`（`XyV2.hs:549`）查 memo，命中复用，未命中递归 `expandImpl` 并写回 memo（`XyV2.hs:535-541`）
     - `ex` 分支按 `e_foot tE2`：脚值 >1 → 建外层 `o`、末列减一后递归 `expandImpl`（`XyV2.hs:629-648`）；否则弹末列顶、末列减一、parent 上移两级（`XyV2.hs:651-681`）
     - b 列 `no >= b.no` 的元素复制进末列（`XyV2.hs:683-701`）；`copyCloumn`（`XyV2.hs:331`）按 `[0..n-1]` 轮 × 列复制；输出各列顶值（`XyV2.hs:708`）
- **分叉**：与 v1 共用 Common arena 与 `toSequenceV12`；分叉在启动列定位（`getBootCloumn` + `getReferenceChain` + `getMainDimensionSequence` 递归）、`Memo`（`XyV2.hs:459`）记忆化、以及 v2 独有的列复制一套：`isEqualSequnce`（`XyV2.hs:249`）、`calcMaxRow`（`XyV2.hs:402`）、`copyElement`（`XyV2.hs:268`）、`copyElt`（`XyV2.hs:359`）、`fixParentV2`（`XyV2.hs:440`）
- **错误**：仅 `SeqCaps` 两条通用错误；族内无自有 `Left` 或运行时错误

### `xy-v3` · X-Y V3（idOrder 第 68 位）

- **族 / 模块**：Xy · `src/OmegaY/Fam/Xy/XyV3.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandXyV3`（`XyV3.hs:703`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 展开轮次，`term' = if term > 10 then 10 else term`（`XyV3.hs:707`）；`null seq` 或 `term < 1` 原样回显（`XyV3.hs:704`）；`param` 不参与
- **步骤**：
  1. 守门（`XyV3.hs:704`）；算 `term'`（`XyV3.hs:707`）
  2. `newWorld`（`XyV3.hs:62`）：独立 `V3` 槽位 arena（`V3` 节点定义 `XyV3.hs:39`），不复用 Common
  3. `toSequenceV3`（`XyV3.hs:298`）建树
  4. `expand_impl w s term' dXY False`（`XyV3.hs:623`，`dXY = [1,0,1]`，`XyV3.hs:700`）：
     - `null s` / 末值 `vValue <= 1` → 取 init 各值（`XyV3.hs:624-629`）
     - `markArena`（`XyV3.hs:117`）记水位 → `getBootIndex`（`XyV3.hs:397`）取 `(bi,bj)` → `releaseTo`（`XyV3.hs:122`）回收 `getBootIndex` 自画的临时山（`XyV3.hs:633-635`）
     - `drawMountain`（`XyV3.hs:322`）重画；`setElementRefrence`（`XyV3.hs:217`）沿 head/foot 定 `vRef`；`setElementNo`（`XyV3.hs:259`，`Just bId`）编号
     - `expandDimensionSequnece`（`XyV3.hs:531`，源码拼写）：`getMds`（`XyV3.hs:389`）取主维序列；需要时 `upchain`（`XyV3.hs:585`）补链、`fixParentUp`（`XyV3.hs:610`）修 parent，并递归 `expand_impl`（`XyV3.hs:580`）
     - `crossDim` 判据：`vValue t - vValue b > 1 && length d == 1`（`XyV3.hs:650`）；成立 → `getOds`（`XyV3.hs:372`）+ `subD`（`divideV3` / `mergeV3`）递归 `expand_impl`；否则 t.foot 弹出、parent 上移，`ex = []`（`XyV3.hs:657-667`）
     - 末列全部减一（`XyV3.hs:669`）；b 列元素复制进末列（`XyV3.hs:677-688`）；`copyCloumn`（`XyV3.hs:498`）按 `[0..n-1]` 轮 × `[bCol+1..tCol]` 列复制；输出各列顶值（`XyV3.hs:694-696`）
- **分叉**：v3 是独立第三份实现，与 v1/v2 不共享 arena、不共享行运算：自带 `compareRowV3`（`XyV3.hs:139`）、`rowAdditionV3`（`XyV3.hs:157`）、`rowDifferenceV3`（`XyV3.hs:168`）、`divideV3`（`XyV3.hs:183`）、`mergeV3`（`XyV3.hs:190`）、`getFootRowV3`（`XyV3.hs:201`）；新增 `vRef` 参考链与 arena 水位回收，列复制一套为 `copyElement`（`XyV3.hs:440`）/ `copyCloumn`（`XyV3.hs:498`）。`expandXyV3Count`（`XyV3.hs:713`）是返回节点总数的调试包装，未接注册表
- **错误**：仅 `SeqCaps` 两条通用错误；族内无自有 `Left` 或运行时错误

## 本族的「共用骨架 + 变体分叉」判定

- **共用骨架 + 模块内独立实现（不是版本开关）**：三个 id 走同一段「守门 → 建 arena → 建树 → 画山取 b/t → 复制列 → 取顶值」流程，但内核各写一份：v1 `expandImpl`（`XyV1.hs:261`）、v2 `expandImpl`（`XyV2.hs:562`，多 `memo` 参数）、v3 `expand_impl`（`XyV3.hs:623`，多 `f` 参数）。源码里不存在按版本号选分支的统一展开函数。
- **v1 / v2 共享**：`Common.hs` 的 `World` arena、`allocE`/`getE`/`modE`/`setE`、`compareRow`/`rowAddition`/`rowDifference`/`getFootRow`/`isDimensionLimitedV12`、`toSequenceV12`。
- **v2 独有**：`getReferenceChain` 参考链 + `getMainDimensionSequence` 主维序列 + `getBootCloumn` 启动列递归；`Memo` 记忆化（`findMemo` 命中复用）；`calcMaxRow` / `copyElement` / `copyElt` / `fixParentV2` 列复制套。
- **v3 独有**：整套 `V3` 节点与 V3 行运算、`vRef` 参考链、`markArena`/`releaseTo` 水位回收、`getBootIndex`/`getOds`/`getMds`、`upchain`/`fixParentUp`、`copyElement`/`copyCloumn` 另一套。
- **v1 独有**：`fetchDimensionSequence` / `expandDimensionSequence` 维度序列递归、`calcMaxCopyrow` / `copyItem` / `copyRowElt` / `fixParent` / `buildOuter`；无 memo、无参考链。
- 全族共同点：只吃整数序列、只吐整数序列，`param` 一律不参与；`term` 只作展开轮次，入口都截断到 10 轮；三份模块内部都不返回 `Left`，异常输入一律走「原样回显」或取 init 各值。
