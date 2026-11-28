# Dbms（15 个 id：`up1dbms` `up2dbms-v1` `up2dbms-v1b-plus` `up2dbms-v1c` `up2dbms-v2` `s1dbms` `s2dbms` `s-omega-dbms` `s-omega-dbms-v2` `s-omega-dbms-v3` `s-omega-p1-dbms` `s-omega2-dbms` `s-omega-square-dbms` `ts-2-dbms` `ts-omega-dbms`）

**一句话导读**：这族算的是 DBMS 序数矩阵展开——把文本解析成「列→条目」的 `Expr`，取末列末项定根，按版本重写高度、拼回新末列，再按 `term` 轮次复制右侧列；本族 `Common` / `MnUp2` 是跨族共享逻辑的源头，被 `Fam.Upmn` 的实现复用（Upmn 各 id 见其族手册）。它对应 `docs/diagrams/04-kernel-flow/` 的**第四链（序数系）**：`parseCol` 解析条目列 → `cmpOrd`/`cmpEntry` 比较条目 → `predecessor` 求前驱 → `remapCol`/`shiftCol` 重写列 → 拼接（`suspend`/`unsuspend`）。

## 共用骨架

下列步骤是 15 个 id 进入实现函数之后共同走过的段；骨架到「重写高度 / 定 up 位」一步为止一致，之后按 §分叉 各走各的。

1. **取 term 兜底**：各文件 `expandTextImpl`（或 `Up2DbmsCore.expandWith`）先做 `term' = if term < 1 then 1 else term`，CLI 的 `--term 0` 或缺省 0 等价 1（`src/OmegaY/Fam/Dbms/Up1Dbms.hs:227` 等同名分支）。
2. **解析文本为 `Expr`**：非 TS 版调本文件 `parse`（条目解析器 `entryP` 走共享 `parseCol`，`src/OmegaY/Fam/Dbms/Common.hs:52`）；Up2 三版走 `MnUp2.parseExpr`（`src/OmegaY/Fam/Dbms/MnUp2.hs:107`）；TS 两版走 `fromDisplay`（`Ts2Dbms.hs:540` / `TsOmegaDbms.hs:736`）。解析失败 → `errParse`（`Common.hs:20`）。
3. **空表达式短路**：`null expr` → 输出空串 `""`，不进展开。
4. **合法性校验**：非 TS 版用 `exprValid` 或 `anyOutOfRange` 检查条目值（及高度位）是否落在 `[0, n)`；越界 → `errParse`。TS 版无此静态校验，非法结构在解析段随 `fromDisplay` 的 `Left` 一并映射为 `errParse`。
5. **末列空短路**：`null (last expr)` → 去掉末列后直接 `display` 输出。
6. **展开主体**（`expand` / `dbmsExpand` / `expandF`）：取末列末项 `(root, h)` → 按版本重算新高度或定 up 提升位 → `mergeCol` 拼出新末列（新高度条目 + 根列 `expr !! root`）→ `addCopy` 从 `root+1` 到 `right` 按 `term'` 轮次 `copyCol` 复制，偏移 `(right - root) * w` → `shorter` 决定是否去掉结果末列。
7. **序列化**：`display` / `displayExpr` 把 `Expr` 拼回括号文本；15 个 id 的后缀都是 `""`（`Family.hs:22-51`）。

### 模块与依赖（族内谁用谁）

- `src/OmegaY/Fam/Dbms/Common.hs`：解析/比较助手（`parseCol` `parseNum` `expect` `skipSp` `skipIdx` `dropTrailing` `errParse` `errResource`），族内各实现模块直接 import；`MnUp2.hs:22` 也 import 它。
- `src/OmegaY/Fam/Dbms/MnUp2.hs`：Up2 共享内核（`Expr = [Col]`，`Col = [(value, sep)]`；`parseExpr` `displayExpr` `exprValid` `dbmsExpand` `exprVert` `parents` `copyCol` 及 `v1ComputeUp1`/`mnComputeUp1`/`dbmsComputeUp2`）。被 `Up2DbmsCore.hs:6` import。
- `src/OmegaY/Fam/Dbms/Up2DbmsCore.hs`：Up2 v1 / v1b+ / v1c 的共用外壳 `expandWith`，只传 `v1flag` / `v1c` / `longer` 三个开关。
- `Up1Dbms` `S1Dbms` `S2Dbms` `Up2V2` `SOmegaV1/V2/V3/P1/2/Square` `Ts2Dbms` `TsOmegaDbms` 各自直接 import `Common`，高度表示与重写规则互不相同。

### 版本分叉总览

- **上行版 `up*`（5 个）**：展开时先算一条 up 提升位序列（`computeUp` / `upList`），复制列时按 up 位决定根值条目是否随偏移提升；高度表示分三档：单高度 int（`up1dbms`）、MnUp2 的 `(value, sep)` 相对列体系（`up2dbms-v1/v1b-plus/v1c`）、双高度 `H(h0,h1)`（`up2dbms-v2`）。
- **σ 版 `s*`（8 个）**：不算 up 位，复制时 `copyVal` 恒按 `v >= root` 偏移；分叉全部集中在高度类型与「重写高度」那一条规则（`computeNewH` / `computeNewHeight`）。
- **to-sequence 版 `ts*`（2 个）**：条目高度是序数表达式（`OrdO` CNF），展开前先 `convertMatrix` + `relativeToCnf` 互转；极限项走基本序列近似（`fundamentalSeq` / `limitApproximation`）；展开段的内部 `Left` 映射为 `errResource`（`Common.hs:24`）。

---

### `up1dbms` · UP1DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/Up1Dbms.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Up1.expandAlter`（`Up1Dbms.hs:253`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `Up1.expandText`（`Up1Dbms.hs:250`）是 `textAlterNotation` 丢弃的第一支，**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^高度`（1-based 显示）+ `--term` → 同形括号列文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`；`param` 不参与（`TextCaps` 不看 `nParam`）
- **步骤**：
  1. `parse`（`Up1Dbms.hs:13`）：`skipSp` 后要求首字符 `(`，逐列 `parseCol entryP`，列尾 `value == -1` 条目由 `dropTrailing` 去掉；空串解析为 `[]`
  2. `null expr` → `""`（`Up1Dbms.hs:231`）；`anyOutOfRange`（`Up1Dbms.hs:241`）查条目值与末高度位是否越界，越界 → `errParse`
  3. `null (last expr)` → `display (init expr)`（`Up1Dbms.hs:237-238`）
  4. `expand`（`Up1Dbms.hs:198`）：末列末项 `E r h`；`newH = colHeight (expr !! h)`（`Up1Dbms.hs:208`）；`mergeCol newCol0 [E r newH]` 再 `mergeCol` 根列 `expr !! r`（`Up1Dbms.hs:211-212`）
  5. `computeUp expr r h`（`Up1Dbms.hs:97`）：从 `r+1` 向右逐列，`heightAfter` 取根以上高度，`parentAt` 找父，`walkUp` 沿父链回溯，`decidedLoop` 用 `relCol`/`relCmp` 相对列比较定 up 位，产出 `[Int]` 提升序列
  6. `addCopy`（`Up1Dbms.hs:215-220`）：`copyCol` 按 up 位 `copyEntry` 提升；`shorter` 截去结果末列
  7. `display`（`Up1Dbms.hs:43`）序列化
- **分叉**：与 §共用骨架 共用 1–4、7 步；独有第 5 步 `computeUp`（含 `walkUp` `decidedLoop` `relCol` `relCmp`），复制列 `copyCol`（`Up1Dbms.hs:67-68`）按 up 位提升根值与高度位；高度表示是单个 int（`E v h`，`Up1Dbms.hs:7`）
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up1Dbms.hs:229`，`Common.hs:20`）；条目/高度越界 → 同一文本（`Up1Dbms.hs:235`）；其余仅 §00 通用三分支

### `up2dbms-v1` · UP2DBMS v1

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/Up2V1.hs`（外壳 `src/OmegaY/Fam/Dbms/Up2DbmsCore.hs`，内核 `src/OmegaY/Fam/Dbms/MnUp2.hs`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Up2V1.expandAlter`（`Up2V1.hs:8`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `Up2V1.expandText`（`Up2V1.hs:5`）**未接线**
- **输入 → 输出**：MnUp2 括号列文本（条目带 sep 逗号）+ `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`Up2DbmsCore.hs:10`）；`param` 不参与
- **步骤**：
  1. `expandWith True False text term True`（`Up2V1.hs:8`）：`v1flag=True`、`v1c=False`，`longer=True`
  2. `parseExpr`（`MnUp2.hs:107`，`smnFromDisplay` 同时接受带括号与不带括号的列）解析失败 → `errParse`（`Up2DbmsCore.hs:12`）；`null expr` → `""`（`Up2DbmsCore.hs:14-15`）
  3. `exprValid`（`MnUp2.hs:112`）查所有条目值 `v ∈ [0, n)`，失败 → `errParse`（`Up2DbmsCore.hs:17-18`）
  4. `null (last expr)` → `displayExpr (init expr)`（`Up2DbmsCore.hs:20-21`）
  5. `dbmsExpand expr term' (not longer) v1flag v1c`（`MnUp2.hs:376`）：`exprVert`（`MnUp2.hs:163`）算纵向向量、`parents`（`MnUp2.hs:183`）定父位 `(ri, rj)`；`isFinite = snd (rc !! top) == 0`（`MnUp2.hs:390`）
  6. 有限分支 `v1flag` 为真 → `v1ComputeUp1`（`MnUp2.hs:224`，直接 fill `ri` 以上为 1）；非有限分支 → `dbmsComputeUp2`（`MnUp2.hs:302`，`v1c=False`）
  7. 拼新末列 `newCol = init rc ++ drop rj (expr !! ri)`（`MnUp2.hs:400`）；`addCopy`（`MnUp2.hs:403-407`）用 `copyCol`（`MnUp2.hs:353`）复制，`v1flag=True` 时 `j <= rj` 的根条目也随 up 提升（`MnUp2.hs:362`）
  8. `displayExpr`（`MnUp2.hs:100`）序列化
- **分叉**：与 `up2dbms-v1b-plus` / `up2dbms-v1c` 共用 `expandWith` 外壳与 `dbmsExpand` 主体；本版独有 `v1flag=True`：有限分支走 `v1ComputeUp1` 而非 `mnComputeUp1`，且 `copyCol` 带 `j <= rj` 提升例外；`v1c=False`
- **错误**：解析失败 / 校验失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up2DbmsCore.hs:12`、`Up2DbmsCore.hs:18`，`Common.hs:20`）；其余仅 §00 通用三分支

### `up2dbms-v1b-plus` · UP2DBMS v1B+

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/Up2V1bPlus.hs`（外壳 `Up2DbmsCore.hs`，内核 `MnUp2.hs`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Up2V1b.expandAlter`（`Up2V1bPlus.hs:8`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `Up2V1bPlus.expandText`（`Up2V1bPlus.hs:5`）**未接线**
- **输入 → 输出**：MnUp2 括号列文本 + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`；`param` 不参与
- **步骤**：
  1. `expandWith False False text term True`（`Up2V1bPlus.hs:8`）：`v1flag=False`、`v1c=False`
  2. 解析 / 空短路 / `exprValid` 校验 / 末列空短路：同 `up2dbms-v1` 第 2–4 步（`Up2DbmsCore.hs:11-21`）
  3. `dbmsExpand`（`MnUp2.hs:376`）：有限分支 `v1flag` 为假 → `mnComputeUp1`（`MnUp2.hs:229`，按父链与相对列比较逐列定 up）；非有限分支 → `dbmsComputeUp2`（`MnUp2.hs:302`，`v1c=False`）
  4. 拼新末列、`addCopy` 复制、`displayExpr` 序列化：同 `up2dbms-v1` 第 7–8 步；`copyCol` 无 `v1flag` 的 `j <= rj` 例外（`MnUp2.hs:362`）
- **分叉**：三版 Up2 中本版两个开关都为假——有限分支走 `mnComputeUp1`，非有限分支 `walkY`（`MnUp2.hs:330-340`）按 `rj+1` 父链回溯；与 v1 的差别在有限分支计算函数，与 v1c 的差别在 `walkY` 停止判定
- **错误**：解析失败 / 校验失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up2DbmsCore.hs:12`、`Up2DbmsCore.hs:18`，`Common.hs:20`）；其余仅 §00 通用三分支

### `up2dbms-v1c` · UP2DBMS v1C

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/Up2V1c.hs`（外壳 `Up2DbmsCore.hs`，内核 `MnUp2.hs`）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`Up2V1c.expandAlter`（`Up2V1c.hs:8`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `Up2V1c.expandText`（`Up2V1c.hs:5`）**未接线**
- **输入 → 输出**：MnUp2 括号列文本 + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`；`param` 不参与
- **步骤**：
  1. `expandWith False True text term True`（`Up2V1c.hs:8`）：`v1flag=False`、`v1c=True`
  2. 解析 / 空短路 / `exprValid` 校验 / 末列空短路：同 `up2dbms-v1` 第 2–4 步（`Up2DbmsCore.hs:11-21`）
  3. `dbmsExpand`（`MnUp2.hs:376`）：有限分支 → `mnComputeUp1`（`MnUp2.hs:229`）；非有限分支 → `dbmsComputeUp2`（`MnUp2.hs:302`），`v1c=True` 传入
  4. `dbmsComputeUp2` 的 `walkY`（`MnUp2.hs:330-340`）：`v1c` 为真时停止条件同时比对列长 `rj+2` 与 `rj` 位父链 `p !! rj`（`MnUp2.hs:337-339`），回溯位取 `rj` 而非 `rj+1`
  5. 拼新末列、`addCopy` 复制、`displayExpr` 序列化：同 `up2dbms-v1` 第 7–8 步
- **分叉**：本版独有 `v1c=True` 判定，只改 `dbmsComputeUp2.walkY` 这一条回溯规则；有限分支与 v1b+ 相同（`mnComputeUp1`）
- **错误**：解析失败 / 校验失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up2DbmsCore.hs:12`、`Up2DbmsCore.hs:18`，`Common.hs:20`）；其余仅 §00 通用三分支

### `up2dbms-v2` · UP2DBMS v2

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/Up2V2.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Up2V2.expandAlter`（`Up2V2.hs:315`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `Up2V2.expandText`（`Up2V2.hs:312`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(h1,h0+1)` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`Up2V2.hs:298`）；`param` 不参与
- **步骤**：
  1. `parse`（`Up2V2.hs:68`）：`parseH`（`Up2V2.hs:44`）解析 `(h1,h0)` 高度对，`entryP`（`Up2V2.hs:54`）接受 `^` 或裸 `(` 起首的高度；`dropTrailing` 去尾
  2. `null expr` → `""`（`Up2V2.hs:302-303`）；`exprValid`（`Up2V2.hs:291`）查条目值与 `h0` 均落在 `[0, n)`，失败 → `errParse`（`Up2V2.hs:305-306`）
  3. `null (last expr)` → `display (init expr)`（`Up2V2.hs:308-309`）
  4. `expand`（`Up2V2.hs:263`）：末列末项 `E r h`；`computeNewHeight`（`Up2V2.hs:130`）在根列找首个高度 `<= h` 的条目定新高度；找不到时比较 `height1` 与 `h1`，取 `H r (h1-1)` 或列高回退；`mergeCol` 拼新末列（`Up2V2.hs:276-279`）
  5. `computeUp expr r h`（`Up2V2.hs:197`）：`heightAfter` 取根以上高度，`parentAt` 定位父，`walkY` 沿父链回溯，`loopUp` 用 `toRelCol`/`relColCmp` 相对列比较定 `[Bool]` 提升位
  6. `addCopy`（`Up2V2.hs:282-286`）：`copyCol`（`Up2V2.hs:114`）带 BMS 升级分支——根条目且未 up 时，按高度 `h1` 决定直接提升还是补一条 `finite` 条目再复制（`Up2V2.hs:120-126`）
  7. `display`（`Up2V2.hs:35`）序列化
- **分叉**：双高度 `H(h0,h1)` 体系；独有第 5 步 `computeUp`（`heightAfter`/`parentAt`/`walkY`/`loopUp`）与第 6 步 `copyCol` 的升级分支；不经过 `MnUp2`
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up2V2.hs:300`，`Common.hs:20`）；越界 → 同一文本（`Up2V2.hs:306`）；其余仅 §00 通用三分支

### `s1dbms` · S1DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/S1Dbms.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`S1.expandAlter`（`S1Dbms.hs:116`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `S1.expandText`（`S1Dbms.hs:113`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^高度` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`S1Dbms.hs:99`）；`param` 不参与
- **步骤**：
  1. `parse`（`S1Dbms.hs:14`）：`entryP`（`S1Dbms.hs:31`）解析 `值^高度`，`dropTrailing` 去尾
  2. `null expr` → `""`（`S1Dbms.hs:103-104`）；`anyOutOfRange`（`S1Dbms.hs:88`）越界 → `errParse`（`S1Dbms.hs:106-107`）
  3. `null (last expr)` → `display (init expr)`（`S1Dbms.hs:109-110`）
  4. `expand`（`S1Dbms.hs:65`）：末列末项 `E r h`；`newH = colHeight (expr !! h)`（`S1Dbms.hs:75`）；`mergeCol newCol0 [E r newH]` 再 `mergeCol` 根列（`S1Dbms.hs:78-79`）
  5. `addCopy`（`S1Dbms.hs:81-83`）：`copyCol`（`S1Dbms.hs:62-63`）的 `copyVal` 恒按 `v >= r` 偏移，无 up 位
  6. `display`（`S1Dbms.hs:41`）序列化
- **分叉**：全族最薄的一条——不算 up 位、没有高度重算辅助函数；新高度直接取前驱列 `expr !! h` 的列高；复制规则是 σ 版的基线，其余 σ 版都在它之上换高度类型与重写规则
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`S1Dbms.hs:101`，`Common.hs:20`）；越界 → 同一文本（`S1Dbms.hs:107`）；其余仅 §00 通用三分支

### `s2dbms` · S2DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/S2Dbms.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`S2.expandAlter`（`S2Dbms.hs:163`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `S2.expandText`（`S2Dbms.hs:160`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(h1,h0+1)` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`S2Dbms.hs:146`）；`param` 不参与
- **步骤**：
  1. `parse`（`S2Dbms.hs:39`）：`parseH`（`S2Dbms.hs:15`）解析高度对 `H(h0,h1)`，`entryP`（`S2Dbms.hs:25`）接受 `^` 或裸 `(` 起首；`dropTrailing` 去尾
  2. `null expr` → `""`（`S2Dbms.hs:150-151`）；`anyOutOfRange`（`S2Dbms.hs:138`）查条目值与 `h0` 越界 → `errParse`（`S2Dbms.hs:153-154`）
  3. `null (last expr)` → `display (init expr)`（`S2Dbms.hs:156-157`）
  4. `expand`（`S2Dbms.hs:113`）：末列末项 `E r h`；`computeNewHeight`（`S2Dbms.hs:97`）在根列找首个 `cmpH h <= 0` 的条目，取其值或前一条目高度为新高度；找不到时按 `height1` 回退
  5. `mergeCol` 拼新末列（`S2Dbms.hs:126-129`）；`addCopy`（`S2Dbms.hs:131-133`）`copyCol`（`S2Dbms.hs:89-90`）按 `v >= r` 偏移，`copyH` 只偏移 `h0`
  6. `display`（`S2Dbms.hs:57`）序列化
- **分叉**：在 `s1dbms` 基线上把单高度 int 换成双高度对 `H(h0,h1)`，重写规则换成 `computeNewHeight` 的首条 ≤ 高度查找；仍无 up 位
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`S2Dbms.hs:148`，`Common.hs:20`）；越界 → 同一文本（`S2Dbms.hs:154`）；其余仅 §00 通用三分支

### `s-omega-dbms` · SωDBMS v1

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/SOmegaV1.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SΩV1.expandAlter`（`SOmegaV1.hs:201`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `SΩV1.expandText`（`SOmegaV1.hs:198`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(高度序列...)` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`SOmegaV1.hs:184`）；`param` 不参与
- **步骤**：
  1. `parse`（`SOmegaV1.hs:124`）：`parseH`（`SOmegaV1.hs:92`）解析高度整数列表（内部反转、减 1），`entryP`（`SOmegaV1.hs:113`）；`dropTrailing` 去尾
  2. `null expr` → `""`（`SOmegaV1.hs:188-189`）；`exprValid`（`SOmegaV1.hs:177`）查条目值与高度首项 ∈ `[0, n)` 且高度非空，失败 → `errParse`（`SOmegaV1.hs:191-192`）
  3. `null (last expr)` → `display (init expr)`（`SOmegaV1.hs:194-195`）
  4. `expand`（`SOmegaV1.hs:153`）：末列末项 `E r h`，`rh = head h`；`computeNewH`（`SOmegaV1.hs:67`）用 `topSep` 取首段连续长度、`findBelowH` 二分定位、`lexEq` 前缀等值判定，定出新高度
  5. `mergeCol` 拼新末列（`SOmegaV1.hs:167-168`）；`addCopy`（`SOmegaV1.hs:170-172`）`copyCol`（`SOmegaV1.hs:64-65`）按 `v >= r` 偏移，`copyH` 逐位偏移
  6. `display`（`SOmegaV1.hs:142`）序列化
- **分叉**：高度表示是整数列表 `Height = [Int]`；独有重写规则 `computeNewH`（`topSep`/`findBelowH`/`lexEq`）；无 up 位
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmegaV1.hs:186`，`Common.hs:20`）；越界 → 同一文本（`SOmegaV1.hs:192`）；其余仅 §00 通用三分支

### `s-omega-dbms-v2` · SωDBMS v2

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/SOmegaV2.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SΩV2.expandAlter`（`SOmegaV2.hs:254`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `SΩV2.expandText`（`SOmegaV2.hs:251`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(值项[@位置])`，未定义值显示 `ω` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`SOmegaV2.hs:237`）；`param` 不参与
- **步骤**：
  1. `parse`（`SOmegaV2.hs:168`）：`parseH`（`SOmegaV2.hs:129`）支持外层套一层括号；`parseItem`（`SOmegaV2.hs:116`）经 `parseHVal`（`SOmegaV2.hs:106`）认 `ω`/`w` 为未定义值；`dropTrailing` 去尾
  2. `null expr` → `""`（`SOmegaV2.hs:241-242`）；`exprValid`（`SOmegaV2.hs:226`）查值 ∈ `[0,n)`、已定义值落界、未定义位置位合法，失败 → `errParse`（`SOmegaV2.hs:244-245`）
  3. `null (last expr)` → `display (init expr)`（`SOmegaV2.hs:247-248`）
  4. `expand`（`SOmegaV2.hs:203`）：末列末项 `E r h`；`computeNewH`（`SOmegaV2.hs:76`）按末条目三态分派——值未定义 → `heightFill` 填值再降位置位；位置未定义 → 取根列列高；都已定义 → 在根列找首条 ≤ 高度条目，`heightFill`（`SOmegaV2.hs:58`）填位后与前一条目比较取高
  5. `mergeCol` 拼新末列（`SOmegaV2.hs:216-217`）；`addCopy`（`SOmegaV2.hs:219-221`）`copyCol`（`SOmegaV2.hs:55-56`）按 `v >= r` 偏移，`copyHE` 只偏移已定义的值位
  6. `display`（`SOmegaV2.hs:186`）序列化
- **分叉**：高度条目是 `HEntry(pDef, p, vDef, v)` 四元组；独有 `heightFill` 填位规则与 `computeNewH` 三态分派；`cmpUndef`（`SOmegaV2.hs:16`）处理未定义位比较
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmegaV2.hs:239`，`Common.hs:20`）；越界 → 同一文本（`SOmegaV2.hs:245`）；其余仅 §00 通用三分支

### `s-omega-dbms-v3` · SωDBMS v3

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/SOmegaV3.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SΩV3.expandAlter`（`SOmegaV3.hs:224`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `SΩV3.expandText`（`SOmegaV3.hs:221`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(值@位置)` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`SOmegaV3.hs:207`）；`param` 不参与
- **步骤**：
  1. `parse`（`SOmegaV3.hs:145`）：`parseH`（`SOmegaV3.hs:105`）支持外层套括号；`parseItem`（`SOmegaV3.hs:96`）要求 `值@位置`；`dropTrailing` 去尾
  2. `null expr` → `""`（`SOmegaV3.hs:211-212`）；`exprValid`（`SOmegaV3.hs:198`）查条目值与高度值 ∈ `[0, n)`，失败 → `errParse`（`SOmegaV3.hs:214-215`）
  3. `null (last expr)` → `display (init expr)`（`SOmegaV3.hs:217-218`）
  4. `expand`（`SOmegaV3.hs:173`）：末列末项 `E r h`；`computeNewH`（`SOmegaV3.hs:63`）：位置位 `p > 0` → `heightFillDec` 降一位填根；`p == 0` 且高度仅一条 → 取根列列高；否则按次条位置位在根列找首条 ≤ 高度条目，`heightFillDec`（`SOmegaV3.hs:53`）填位后与前一条目比较取高；列高为空时返回 `Nothing`（新末列不加新高度条目）
  5. `mergeCol` 拼新末列（`SOmegaV3.hs:187-189`）；`addCopy`（`SOmegaV3.hs:191-193`）`copyCol`（`SOmegaV3.hs:50-51`）按 `v >= r` 偏移，`copyHE` 只偏移值位、位置位不动
  6. `display`（`SOmegaV3.hs:163`）序列化
- **分叉**：高度条目是 `HE(v, p)` 二元组；`cmpH` 长度反向比较（短者为大，`SOmegaV3.hs:28`）；独有 `heightFillDec` 降级填位规则；`computeNewH` 可返回 `Nothing`
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmegaV3.hs:209`，`Common.hs:20`）；越界 → 同一文本（`SOmegaV3.hs:215`）；其余仅 §00 通用三分支

### `s-omega-p1-dbms` · Sω+1DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/SOmegaP1.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SΩP1.expandAlter`（`SOmegaP1.hs:292`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `SΩP1.expandText`（`SOmegaP1.hs:289`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(值@(ω|位置+1))` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`SOmegaP1.hs:275`）；`param` 不参与
- **步骤**：
  1. `parse`（`SOmegaP1.hs:209`）：`parseH`（`SOmegaP1.hs:172`）支持外层套括号；`parseItem`（`SOmegaP1.hs:163`）经 `parseHPos`（`SOmegaP1.hs:153`）认 `ω`/`w` 为 ω 位；`dropTrailing` 去尾
  2. `null expr` → `""`（`SOmegaP1.hs:279-280`）；`exprValid`（`SOmegaP1.hs:265`）查值 ∈ `[0,n)`、非 ω 位落界，失败 → `errParse`（`SOmegaP1.hs:282-283`）
  3. `null (last expr)` → `display (init expr)`（`SOmegaP1.hs:285-286`）
  4. `expand`（`SOmegaP1.hs:240`）：末列末项 `E r h`；`computeNewH`（`SOmegaP1.hs:139`）先 `findLowerHP`（`SOmegaP1.hs:115`）：位置是 ω 位 → 直接得 `HP False r`；否则 `findMaxBelow`（`SOmegaP1.hs:91`，经 `descend`（`SOmegaP1.hs:71`）下钻）找根列以下最大条目，再沿其高度递归找低位；找到新位置位 → `heightFillDec`（`SOmegaP1.hs:63`）填根；找不到 → 取 `findMaxBelow` 结果的高度
  5. `mergeCol` 拼新末列（`SOmegaP1.hs:253-256`）；`addCopy`（`SOmegaP1.hs:258-260`）`copyCol`（`SOmegaP1.hs:60-61`）按 `v >= r` 偏移，`copyHP` 不动 ω 位
  6. `display`（`SOmegaP1.hs:227`）序列化
- **分叉**：位置位带 ω 标记 `HP(isW, num)`；独有 `descend` / `findMaxBelow` / `findLowerHP` 这条下钻找低位链；`findLowerHP` 的 ω 分支直接落 `HP False r`
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmegaP1.hs:277`，`Common.hs:20`）；越界 → 同一文本（`SOmegaP1.hs:283`）；其余仅 §00 通用三分支

### `s-omega2-dbms` · Sω2DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/SOmega2.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SΩ2.expandAlter`（`SOmega2.hs:305`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `SΩ2.expandText`（`SOmega2.hs:302`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(值@(ω+n|位置+1))` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`SOmega2.hs:288`）；`param` 不参与
- **步骤**：
  1. `parse`（`SOmega2.hs:221`）：`parseH`（`SOmega2.hs:184`）支持外层套括号；`parseItem`（`SOmega2.hs:175`）经 `parseHPos`（`SOmega2.hs:153`）认 `ω`/`w` 及 `ω+n`；`dropTrailing` 去尾
  2. `null expr` → `""`（`SOmega2.hs:292-293`）；`exprValid`（`SOmega2.hs:278`）查值 ∈ `[0,n)`、非高位落界，失败 → `errParse`（`SOmega2.hs:295-296`）
  3. `null (last expr)` → `display (init expr)`（`SOmega2.hs:298-299`）
  4. `expand`（`SOmega2.hs:253`）：末列末项 `E r h`；`computeNewH`（`SOmega2.hs:139`）调 `findLowerHP`（`SOmega2.hs:115`）：高位 `hi` 为真 → `n > 0` 得 `HP True (n-1)`，否则 `HP False r`；非高位走 `findMaxBelow`/`descend` 同 P1 链；`heightFillDec`（`SOmega2.hs:63`）填根或取最大条目高度
  5. `mergeCol` 拼新末列（`SOmega2.hs:266-269`）；`addCopy`（`SOmega2.hs:271-273`）`copyCol`（`SOmega2.hs:60-61`）按 `v >= r` 偏移，`copyHP` 不动高位
  6. `display`（`SOmega2.hs:239`）序列化
- **分叉**：骨架与 `s-omega-p1-dbms` 同构（`descend`/`findMaxBelow`/`findLowerHP`/`heightFillDec` 同名同体）；唯一实质分叉在 `findLowerHP` 的高位分支：ω 位按 `n-1` 递减（P1 是直接落普通位）；`parseHPos` 多认 `ω+n` 写法
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmega2.hs:290`，`Common.hs:20`）；越界 → 同一文本（`SOmega2.hs:296`）；其余仅 §00 通用三分支

### `s-omega-square-dbms` · Sω^2DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/SOmegaSquare.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SΩSq.expandAlter`（`SOmegaSquare.hs:327`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `SΩSq.expandText`（`SOmegaSquare.hs:324`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `值^(值@ω 次幂位)` + `--term` → 同形文本
- **term / param 语义**：`term` = 复制轮次，`term' = max 1 term`（`SOmegaSquare.hs:310`）；`param` 不参与
- **步骤**：
  1. `parse`（`SOmegaSquare.hs:242`）：`parseH`（`SOmegaSquare.hs:205`）支持外层套括号；`parseItem`（`SOmegaSquare.hs:196`）经 `parseHP`（`SOmegaSquare.hs:162`）解析 `ω^n+m` / 普通位 / 未定义 ω 位；`dropTrailing` 去尾
  2. `null expr` → `""`（`SOmegaSquare.hs:314-315`）；`exprValid`（`SOmegaSquare.hs:300`）查值 ∈ `[0,n)`、已定义 `b` 落界，失败 → `errParse`（`SOmegaSquare.hs:317-318`）
  3. `null (last expr)` → `display (init expr)`（`SOmegaSquare.hs:320-321`）
  4. `expand`（`SOmegaSquare.hs:275`）：末列末项 `E r h`；`computeNewH`（`SOmegaSquare.hs:148`）调 `findLowerHP`（`SOmegaSquare.hs:121`）：`b` 未定义 → 直接得 `HP a r True`；已定义则先查默认位 `HP(a-1, 0, False)`，再经 `findMaxBelow`（`SOmegaSquare.hs:97`，`descend`（`SOmegaSquare.hs:77`）下钻）比较双层 `HP(a,b,bd)` 递归找低位；`heightFillDec`（`SOmegaSquare.hs:69`）填根或取最大条目高度
  5. `mergeCol` 拼新末列（`SOmegaSquare.hs:288-291`）；`addCopy`（`SOmegaSquare.hs:293-295`）`copyCol`（`SOmegaSquare.hs:66-67`）按 `v >= r` 偏移，`copyHP` 不动 `a`、只偏移已定义的 `b`
  6. `display`（`SOmegaSquare.hs:260`）序列化
- **分叉**：位置位是双层 ω 次幂 `HP(a, b, bDef)`；独有 `cmpHP` 的 `a` 先比、`b` 带未定义态比较；`findLowerHP` 带默认位 `HP(a-1,0,False)` 回退；`parseHP` 解析 `ω^n` 层数
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmegaSquare.hs:312`，`Common.hs:20`）；越界 → 同一文本（`SOmegaSquare.hs:318`）；其余仅 §00 通用三分支

### `ts-2-dbms` · TS2DBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/Ts2Dbms.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Ts2.expandAlter`（`Ts2Dbms.hs:649`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `Ts2.expandText`（`Ts2Dbms.hs:646`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `父^(序数,锚+1)`，序数可写 `w`、数字、`w^...`、`*`、`+` + `--term` → 同形文本
- **term / param 语义**：`term` = 基本序列下标，`term' = max 1 term`（`Ts2Dbms.hs:637`）；`param` 不参与
- **步骤**：
  1. `fromDisplay`（`Ts2Dbms.hs:540`）：`stripWs` 去空白，`closingIdx` 配平括号，`splitTop` 按 `,`/`:` 切段；`parseEntry`（`Ts2Dbms.hs:563`）`matchEntry` 取父位，`parseOrdinal`（`Ts2Dbms.hs:485`）解析序数，`cnfToTs2`（`Ts2Dbms.hs:362`）把 CNF 转回矩阵高度；校验父位向左、条目高度递减。任何内部 `Left` → 边界映射 `errParse`
  2. `null expr` → `""`（`Ts2Dbms.hs:641`）
  3. `fundamentalSeq expr term' alter`（`Ts2Dbms.hs:622`）：`index < 0` → `Left`；`not (isLimit expr)` → 砍末项返回；极限项取末列末项 `(root, h)`，`computeNewHeight`（`Ts2Dbms.hs:370`）判 `StepLimit` / `StepOrdinary`
  4. `StepLimit alpha`（`Ts2Dbms.hs:599`）：`ts2ToCnf` 取替换列基线与 `alpha`，`ordTailAfter`（`Ts2Dbms.hs:110`）求差，`cnfToTs2` 转回，`fundamentalSeq` 递归取基本序列近似，`ordAdd` 拼回新高度
  5. `StepOrdinary mh`（`Ts2Dbms.hs:610`）：`mergeCol` 拼新末列，`addCopy` 按 `term'` 轮次 `copyCol`（`Ts2Dbms.hs:187-189`，`remapCol`/`shiftCol` 重写父位）复制；`shorter` 截末列
  6. `displayExpr`（`Ts2Dbms.hs:433`）：`heightDisplay`（`Ts2Dbms.hs:422`）经 `ts2ToCnf` + `displayOrdinal`（`Ts2Dbms.hs:399`）把高度序数显示回文本
  7. 展开段任何内部 `Left` → 边界映射 `errResource`（`Ts2Dbms.hs:643`）
- **分叉**：高度是 `Height(anchor, level Expr)`，序数走 `OrdO` CNF（`cmpOrd` `ordAdd` `ordMul` `ordPowOmega`）；独有 `StepLimit` 基本序列近似链；`alter` 开关透传给递归 `fundamentalSeq`（`Ts2Dbms.hs:631-632`）
- **错误**：
  - 解析段 `fromDisplay`（`Ts2Dbms.hs:639`）任何 `Left` → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Common.hs:20`）。解析链内部 `Left` 逐字（边界不对外显示原文）：`closingIdx` 的 `"mismatch"`（`Ts2Dbms.hs:456`、`457`、`472`、`474`）、`"unclosed"`（`466`）；`parseOrdinal` 的 `"bad index"`（`480`、`482`）、`"bad ordinal"`（`488`）、`"bad coefficient"`（`502`）、`"bad"`（`520`、`521`、`522`）；`parseEntry`/`fromDisplay` 的 `"bad entry"`（`528`、`532`、`538`）、`"bad column"`（`548`）、`"bad column index"`（`553`）、`"bad height"`（`568`）、`"bad height pair"`（`572`）、`"references must point left"`（`578`）、`"entries must decrease"`（`582`）
  - 展开段 `fundamentalSeq`/`expandF`（`Ts2Dbms.hs:643`）任何 `Left` → `展开超出资源上限（表达式过大或下标过大）`（`Common.hs:24`）。展开链内部 `Left`：`"bad index"`（`588`、`624`）
  - 共用转换助手的 `"negative"`（`ordAdd`，`44`）、`"negative tail"`（`ordTailAfter`，`112`）、`"relative prefix expected"`（`convertMatrixOne`，`249`）、`"invalid prefix"`（`relativeToCnf`，`288`）、`"must start empty"`（`ts2ToCnf`，`357`）两段都可能触发，按发生位置分别映射为 `errP` 或 `errR`
  - 其余仅 §00 通用三分支

### `ts-omega-dbms` · TSωDBMS

- **族 / 模块**：Dbms · `src/OmegaY/Fam/Dbms/TsOmegaDbms.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`TsΩ.expandAlter`（`TsOmegaDbms.hs:808`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `TsΩ.expandText`（`TsOmegaDbms.hs:805`）**未接线**
- **输入 → 输出**：括号列文本，条目形如 `父^(锚+1@序数)` + `--term` → 同形文本
- **term / param 语义**：`term` = 基本序列下标，`term' = max 1 term`（`TsOmegaDbms.hs:796`）；`param` 不参与
- **步骤**：
  1. `fromDisplay`（`TsOmegaDbms.hs:736`）：`closingIdx` 配平，`parseEntry`（`TsOmegaDbms.hs:759`）`stripOuterParens` 剥外层括号，`parseSlot`（`TsOmegaDbms.hs:783`）按 `@` 切锚位与序数，`parseOrdinal`（`TsOmegaDbms.hs:672`）+ `cnfToTsOmega`（`TsOmegaDbms.hs:457`）转回高度；校验高度条目严格递增、条目递减。任何内部 `Left` → 边界映射 `errParse`
  2. `null expr` → `""`（`TsOmegaDbms.hs:800`）
  3. `fundamentalSeq expr term' alter`（`TsOmegaDbms.hs:563`）：`not (isLimit expr)` → 砍末项；极限项看末条目位置是否极限，分别以 `shorter=True` 或 `not alter` 进 `expandF`
  4. `expandF`（`TsOmegaDbms.hs:532`）：末条目位置是极限（`isLimit (positionOf ...)`，`TsOmegaDbms.hs:543`）→ `limitApproximation`（`TsOmegaDbms.hs:492`）：`heightFillDec` 置零位前缀，比较低位前缀，`matrixTail`（`TsOmegaDbms.hs:481`）求差、`fundamentalSeq` 递归近似、`matrixAdd`（`TsOmegaDbms.hs:472`）拼回新高度
  5. 否则 `computeNewHeight`（`TsOmegaDbms.hs:199`）：位置非空 → `predecessor`（`TsOmegaDbms.hs:184`）求前驱后 `heightFillDec` 填根；位置为空且高度多条 → 取次条位置位在根列找首条 ≤ 高度条目；`mergeCol` 拼新末列，`addCopy` 按 `term'` 轮次 `copyCol`（`TsOmegaDbms.hs:230-232`）复制
  6. `displayExpr`（`TsOmegaDbms.hs:620`）：`heightDisplay`（`TsOmegaDbms.hs:607`）逐条 `tsOmegaToCnf`（`TsOmegaDbms.hs:450`）+ `displayOrdinal` 显示
  7. 展开段任何内部 `Left` → 边界映射 `errResource`（`TsOmegaDbms.hs:802`）
- **分叉**：高度是 `Height [HE(anchor, position Expr)]` 条目列表；独有 `predecessor` 前驱判定与 `limitApproximation` 极限近似（`matrixTail`/`matrixAdd`/`rootComponents`）；`convertMatrixOne` 的 `toRel=False` 分支带 `buildRestored` 锚位修正（`TsOmegaDbms.hs:328-351`）
- **错误**：
  - 解析段 `fromDisplay`（`TsOmegaDbms.hs:798`）任何 `Left` → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Common.hs:20`）。解析链内部 `Left` 逐字（边界不对外显示原文）：`closingIdx` 的 `"mismatch"`（`TsOmegaDbms.hs:643`、`644`、`659`、`661`）、`"unclosed"`（`653`）；`parseOrdinal` 的 `"bad index"`（`667`、`669`）、`"bad ordinal"`（`675`）、`"bad coefficient"`（`688`）、`"bad"`（`706`、`707`、`708`）；`parseEntry`/`parseSlot`/`fromDisplay` 的 `"bad entry"`（`714`、`718`、`725`）、`"bad column"`（`744`）、`"bad column index"`（`749`）、`"bad height"`（`764`）、`"height entries must increase"`（`775`）、`"entries must decrease"`（`781`）、`"bad slot"`（`785`）、`"anchor > parent"`（`790`）；`convertMatrixOne`/`relativeToCnf` 的 `"invalid prefix"`（`306`、`390`）
  - 展开段 `fundamentalSeq`/`expandF`（`TsOmegaDbms.hs:802`）任何 `Left` → `展开超出资源上限（表达式过大或下标过大）`（`Common.hs:24`）。展开链内部 `Left`：`"not successor"`（`predecessor`，`186`）、`"empty height"`（`computeNewHeight`，`201`）、`"invalid root"`（`rootComponents`，`465`）、`"neg tail"`（`matrixTail`，`483`）、`"empty h"`（`limitApproximation`，`494`）、`"invalid prefix"`（`503`、`510`、`513`）、`"invalid interval"`（`514`）、`"zero approx"`（`518`、`526`）、`"bad index"`（`534`、`565`）
  - 共用助手的 `"neg"`（`ordAdd`，`42`）、`"neg tail"`（`ordTailAfter`，`109`）、`"must start empty"`（`tsOmegaToCnf`，`452`）两段都可能触发，按发生位置分别映射为 `errP` 或 `errR`
  - 其余仅 §00 通用三分支
- **已知差异（照录 `Family.hs:52-57` 注释口径）**：注释说明 `ts-omega-dbms` 的 `Height=[HeightEntry{anchor,position}]` CNF 互转在 `cnfToTsOmega(omega)` 路径上与 C++ 存在差异——`parseOrdinal "w"` 后 `convert_matrix` 的 `low_column` 判定不通过，导致 `(1^(1@w))` 类输入解析失败；已定位在 `TsOmegaDbms.hs` 的 `convertMatrixOne`/`to_relative=False` 分支对 `suspend` 结果 `[[],[(1,HE 0 [])]]` 的 `pureLow` 判定（anchor=1 而非 0），需对照 C++ `clean_matrix_padding` 的 anchor 重置逻辑修正。该差异是已知 quirk，不影响本 id 在注册表中的存在（`Family.hs:50-51` 已注册）；手册不另编原因，也不替源码修正该判定
