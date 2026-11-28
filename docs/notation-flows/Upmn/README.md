# Upmn（4 个 id：`up1mn` `up2mn-v1a` `up2mn-v1b` `up2mn-v1b-plus`）

**一句话导读**：这族是 MN 序数记号的向上展开版本——把 Expr 列（条目 `(value, sep)`）按父链/竖直序列判定提升项，再按 `term` 复制列；对应 `docs/diagrams/04-kernel-flow/` 的**序数系**内核链（解析 Expr 列 → 比较/求父 → 重写列 → 拼接）。四个 id 全部用 `textAlterNotation` 注册，**活入口是第二个参数 `expandAlter`**；同族复用 `Fam/Upmn/SmnShare.hs`，并跨族复用 `Fam/Dbms/Common.hs` 与 `Fam/Dbms/MnUp2.hs` 的解析/显示/展开助手（Dbms 自己的 id 由另一册展开，本册不写）。

## 共用骨架

1. 解析：`parseExpr`（`src/OmegaY/Fam/Dbms/MnUp2.hs:107`）把文本解析成 0-based `Expr = [Col]`，条目 `(value, sep)`；解析失败 → `errP`（即 `errParse`，`src/OmegaY/Fam/Dbms/Common.hs:19-20`）。
2. 空表达式：`null e` → 直接返回空串 `""`（非错误）。
3. 下标合法性：各 id 自行检查条目值越界（`v < 0 || v >= length e`，或复用 `exprValid`，`MnUp2.hs:112`）；不合法 → `errP`。
4. 末列空：`null (last e)` → 砍末列后 `displayExpr` 输出。
5. 本体展开：`term' = if term < 1 then 1 else term`（各 `expandTextImpl` 首步，把 `term` 夹到 ≥1）；`shorter = not longer`——活入口 `expandAlter` 传 `longer = True`，故 `shorter = False`；展开完按 `shorter` 决定是否再砍末列。
6. 输出：`displayExpr`（`MnUp2.hs:100`）把 `Expr` 拼回 `(v,...)` 文本；四个 id 后缀都是 `""`。

### 跨族复用点（只引用，不展开 Dbms）

- `Dbms.Common`：`errParse` 错误文本、`skipSp` / `parseCol` 等解析助手。
- `Dbms.MnUp2`：`parseExpr`、`displayExpr`、`Expr` / `Col` / `setAt`、`exprValid`、`up2mnExpand`（含 `mnComputeUp1` / `up2mnComputeUp2` / `up2mnCopyCol`）。
- `Fam/Upmn/SmnShare.hs`：`smnExpand`（`SmnShare.hs:210`），填充幽灵条目 → 按 `index` 反复 `extend` → 可选砍末列 → 清幽灵；仅被 `up2mn-v1a` 的有限分支调用。

## 逐记号细目

### `up1mn` · UP1MN

- **族 / 模块**：Upmn · `src/OmegaY/Fam/Upmn/Up1Mn.hs`（注册 `src/OmegaY/Fam/Upmn/Family.hs:10-11`）· 文本型 `TextCaps`（`textAlterNotation`），后缀 `""`
- **入口**：`Up1Mn.expandAlter`（`Up1Mn.hs:138`），由 `expandNotation` 的 `TextCaps` 分支调用；同族 `expandText`（`Up1Mn.hs:135`）被 `textAlterNotation` 丢弃、未接线
- **输入 → 输出**：MN 序列文本（条目不带分隔符）+ `--term` → 同形态序列文本
- **term / param 语义**：`term` 经 `term' = max 1 term`（`Up1Mn.hs:124`）作为复制次数 `index`；`shorter = False`（活入口 `longer = True`，`Up1Mn.hs:139`）；`param` 不参与
- **步骤**：
  1. `parseIntExpr`（`Up1Mn.hs:13`）：先 `parseExpr`，要求所有条目 `sep == 0`（`Up1Mn.hs:16`），否则 `Nothing`；通过后剥掉 sep 得 `IExpr = [[Int]]`
  2. `Nothing` → `errP`（`Up1Mn.hs:126`）；`null e` → `""`（`Up1Mn.hs:128`）
  3. 越界检查：任一 `v < 0 || v >= length e` → `errP`（`Up1Mn.hs:129-130`）
  4. `null (last e)` → `displayIntExpr (init e)`（`Up1Mn.hs:131-132`）
  5. 否则 `expand e term' (not longer)`（`Up1Mn.hs:133` → 定义 `Up1Mn.hs:101`）：`r = last rc`；`upList = computeUp expr r (length rc - 1)`（`Up1Mn.hs:111`，`computeUp` `Up1Mn.hs:23`）；`newCol = init rc ++ drop (length rc - 1) (expr !! r)`（`Up1Mn.hs:113`）；`foldl addCopy` 按 `w ∈ [1..index]`、`i ∈ [r+1..right]` 用 `copyColumn`（`Up1Mn.hs:96`）复制列；`shorter` 为真再 `init`
  6. `displayIntExpr`（`Up1Mn.hs:20`）套回 `displayExpr` 输出
- **分叉**：本 id 独有纯整数 `IExpr` 路径与自实现 `computeUp`；不调 `smnExpand` / `up2mnExpand`
- **错误**：解析失败或含分隔符 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up1Mn.hs:126`，文本定义 `Dbms/Common.hs:20`）；下标越界 → 同一文本（`Up1Mn.hs:129-130`）；仅 §00 通用三分支

### `up2mn-v1a` · UP2MN v1A

- **族 / 模块**：Upmn · `src/OmegaY/Fam/Upmn/Up2MnV1a.hs`（注册 `Family.hs:12-13`）· 文本型 `TextCaps`（`textAlterNotation`），后缀 `""`
- **入口**：`Up2MnV1a.expandAlter`（`Up2MnV1a.hs:164`），由 `expandNotation` 的 `TextCaps` 分支调用；同族 `expandText`（`Up2MnV1a.hs:161`）未接线
- **输入 → 输出**：MN 矩阵文本（条目 `(value, sep)`）+ `--term` → 同形态矩阵文本
- **term / param 语义**：`term' = max 1 term`（`Up2MnV1a.hs:152`）作为复制次数 `index`；`shorter = False`（`Up2MnV1a.hs:165`）；`param` 不参与
- **步骤**：
  1. `parseExpr text`（`Up2MnV1a.hs:153`）；`Nothing` → `errP`（`Up2MnV1a.hs:154`）；`null e` → `""`（`Up2MnV1a.hs:156`）
  2. 越界检查：任一 `v < 0 || v >= length e` → `errP`（`Up2MnV1a.hs:157-158`）
  3. `expand e term' (not longer)`（`Up2MnV1a.hs:159` → 定义 `Up2MnV1a.hs:143`）：`null m` → 原样；`null (last m)` → `init m`；末列**不含无限条目**（`hasInfinite`，`Up2MnV1a.hs:12`）→ `smnExpand m index shorter`（`Up2MnV1a.hs:147`）；含无限条目 → `expandInfinite`（`Up2MnV1a.hs:148` → `Up2MnV1a.hs:76`）
  4. `expandInfinite`：`exprVert` / `parents` 定根 `(ri, rj)`；`upArr` 按有限/无限列分别 `applyUp` 判定提升位（`Up2MnV1a.hs:92-132`）；`subtract1 m` 起底，`foldl addCopy` 按 `w ∈ [1..index]` 用 `copyColumnV1a`（`Up2MnV1a.hs:53`）复制列；`shorter` 为真再 `init`
  5. `displayExpr` 输出（`Up2MnV1a.hs:159`）
- **分叉**：骨架 + 双支路：有限支路走跨族/同族共享的 `smnExpand`，无限支路走本文件自实现的 `expandInfinite`（`copyColumnV1a` 带 `yOffset` 填充）
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up2MnV1a.hs:154`，文本定义 `Dbms/Common.hs:20`）；下标越界 → 同一文本（`Up2MnV1a.hs:157-158`）；仅 §00 通用三分支

### `up2mn-v1b` · UP2MN v1B

- **族 / 模块**：Upmn · `src/OmegaY/Fam/Upmn/Up2MnV1b.hs`（注册 `Family.hs:14-15`）· 文本型 `TextCaps`（`textAlterNotation`），后缀 `""`
- **入口**：`Up2MnV1b.expandAlter False`（`Up2MnV1b.hs:25-26`，注册闭包 `Family.hs:15`），由 `expandNotation` 的 `TextCaps` 分支调用；同族 `expandText False`（`Up2MnV1b.hs:22-23`）未接线
- **输入 → 输出**：MN 矩阵文本（条目 `(value, sep)`）+ `--term` → 同形态矩阵文本
- **term / param 语义**：`term' = max 1 term`（`Up2MnV1b.hs:13`）作为复制次数 `index`；`shorter = False`（`Up2MnV1b.hs:26`）；`plus = False`；`param` 不参与
- **步骤**：
  1. `parseExpr text`（`Up2MnV1b.hs:14`）；`Nothing` → `errP`（`Up2MnV1b.hs:15`）；`null e` → `""`（`Up2MnV1b.hs:17`）
  2. `exprValid e`（`MnUp2.hs:112`）不成立 → `errP`（`Up2MnV1b.hs:18`）
  3. `null (last e)` → `displayExpr (init e)`（`Up2MnV1b.hs:19`）
  4. 否则 `up2mnExpand e term' (not longer) plus`（`Up2MnV1b.hs:20` → 定义 `MnUp2.hs:475`）：`exprVert` / `parents` 定根 `(ri, rj)`；有限列用 `mnComputeUp1`、无限列用 `up2mnComputeUp2 ... plus`（`MnUp2.hs:490-493`）；起底新列后 `foldl addCopy` 按 `w ∈ [1..index]` 用 `up2mnCopyCol`（`MnUp2.hs:414`）复制列；`shorter` 为真再 `init`
  5. `displayExpr` 输出（`Up2MnV1b.hs:20`）
- **分叉**：本体展开全部委托跨族复用的 `up2mnExpand`；本 id 与 `up2mn-v1b-plus` 的唯一区别是 `plus` 实参（见下）
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Up2MnV1b.hs:15`，文本定义 `Dbms/Common.hs:20`）；`exprValid` 不通过 → 同一文本（`Up2MnV1b.hs:18`）；仅 §00 通用三分支

### `up2mn-v1b-plus` · UP2MN v1B+

- **族 / 模块**：Upmn · `src/OmegaY/Fam/Upmn/Up2MnV1b.hs`（注册 `Family.hs:16-17`）· 文本型 `TextCaps`（`textAlterNotation`），后缀 `""`
- **入口**：`Up2MnV1b.expandAlter True`（`Up2MnV1b.hs:25-26`，注册闭包 `Family.hs:17`），由 `expandNotation` 的 `TextCaps` 分支调用；同族 `expandText True`（`Up2MnV1b.hs:22-23`）未接线
- **输入 → 输出**：MN 矩阵文本（条目 `(value, sep)`）+ `--term` → 同形态矩阵文本
- **term / param 语义**：`term' = max 1 term`（`Up2MnV1b.hs:13`）作为复制次数 `index`；`shorter = False`（`Up2MnV1b.hs:26`）；`plus = True`；`param` 不参与
- **步骤**：与 `up2mn-v1b` 完全同骨架（解析 → `exprValid` → 末列空 → `up2mnExpand` → `displayExpr`），仅第 4 步把 `plus = True` 传入 `up2mnExpand`
- **分叉**：与 `up2mn-v1b` 共用同一份 `expandTextImpl` 与 `up2mnExpand`；`plus` 在实现里控制无限列提升位的继承：`up2mnComputeUp2` 中父列 `pi /= ri` 时，`plus` 为真继承父列提升位 `rs !! pi`，为假置 `0`（`MnUp2.hs:447`）
- **错误**：同 `up2mn-v1b`（`Up2MnV1b.hs:15` / `Up2MnV1b.hs:18`，文本定义 `Dbms/Common.hs:20`）；仅 §00 通用三分支
