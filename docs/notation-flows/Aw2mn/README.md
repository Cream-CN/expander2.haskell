# Aw2mn（4 个 id：`a-omega2-mn-2` `weak-a-omega2-mn-2` `a-omega2-mn-3` `weak-a-omega2-mn-3`）

**一句话导读**：这族把括号/逗号/分号写的序数表达式解析成 `Expr`（列 → entry → 分隔 `Sep` 的山结构），按 `term` 轮次做前驱减法与列复制延展，再显示回字符串；它对应
`docs/diagrams/04-kernel-flow/` 的**第四链（序数系，MN/DBMS/TON/AW2MN）**，与 Mn 族的 `omega-mn`、`s-omega2-mn` 同属序数系链，但本族不复用 Mn 的共享模块，解析与比较全在 `Aw2mnShare.hs` 内自实现。

## 共用骨架

1. `expandNotation`（`src/OmegaY/Notation/Types.hs:61`）的 `TextCaps` 分支不做任何校验，直接调注册进 `nCaps` 的展开函数（`Types.hs:64`）。四个 id 都经 `aw2mnNotation`（`src/OmegaY/Fam/Aw2mn/Aw2mn.hs:25`）用 `textAlterNotation` 注册：**第一支 `expandImpl weak kind False` 被构造器丢弃未接线，活入口是第二支 `expandImpl weak kind True`**（`Aw2mn.hs:27-28`，规则见 §00 第 6 节）。
2. `expandImpl weak kind longer text term0`（`Aw2mn.hs:10`）：`term = max 1 term0`（`Aw2mn.hs:11`）。
3. `parse text`（`src/OmegaY/Fam/Aw2mn/Aw2mnShare.hs:117`）：`Nothing` → 返回 `kParseFail`；`Just parsed` 取 `e = pExpr parsed`（`Aw2mnShare.hs:37`）。
4. 空结构短路：`null e` → `""`；`null (last e)` → `mountainDisplay (take (length e - 1) e)`（`Aw2mn.hs:16-19`）；`not (validValueRange e)` → `kParseFail`（`Aw2mn.hs:20-21`，`validValueRange` 见 `Aw2mnShare.hs:565`：每个 entry 值 `>= 1` 且 `<= 列数`）。
5. 否则 `mountainDisplay (expand e term (not longer) weak kind)`（`Aw2mn.hs:22`）。活入口 `longer=True`，故 `shorter = not longer = False`。
6. `expand`（`Aw2mnShare.hs:553`）：`loop` 从 1 到 `index` 每轮 `extend a False weak kind`；`shorter=False` 时末尾再 `extend a1 True weak kind`（`Aw2mnShare.hs:557-560`）。
7. `extend` / `extend'`（`Aw2mnShare.hs:384` / `392`）：`parent`（`Aw2mnShare.hs:261`）定位坏根列行 → 算 `magmaChecksList` → `subtract1`（`Aw2mnShare.hs:284`，按 `kind` 分叉）→ `getReferences`（`Aw2mnShare.hs:364`）取参考列 → 算 `stretchThreshold` / `stretchValue` → `stretched`（`Aw2mnShare.hs:485`，按 `weak` 分叉）→ `buildCol`（`Aw2mnShare.hs:511`）复制延展列。

### `a-omega2-mn-2` · Aω2MN2（idOrder 第 111 位）

- **族 / 模块**：Aw2mn · `src/OmegaY/Fam/Aw2mn/Aw2mn.hs`（装配）+ `src/OmegaY/Fam/Aw2mn/Aw2mnShare.hs`（实现）· 文本型 `TextCaps`，后缀 `""`
- **入口**：`expandImpl False V2 True`（`Aw2mn.hs:10`，第二支 alter；`Aw2mn.hs:27` 那支 `False` 未接线），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：括号/逗号/分号序数表达式文本 + `--term` → 山结构显示串
- **term / param 语义**：`term` = `expand` 的 `index`（展开轮次），先 `max 1 term0`（`Aw2mn.hs:11`）；`param` 不参与（`TextCaps` 不看 `nParam`）
- **步骤**：走 §共用骨架 第 2–7 步；`kind = V2` 进 `subtract1` 的 V2 分支，`weak = False` 进 `stretched` 的强版本判据
- **分叉**：`weak=False`（强）、`kind=V2`。`subtract1` V2 分支（`Aw2mnShare.hs:313-336`）：`mountainIsLimit` 顶部分隔时算 `brSep = sFunc`、`j` 与 `collectEntries`，垂直比较成立才追加 `Entry topRightValue j True`；非 `mountainIsOne` 时截 `init topRightSep`，`alpha == wv'` 才追加带 `truncated` 的非标记 entry；`mountainIsOne` 直接弹出不追加。`stretched`（`Aw2mnShare.hs:487`）因 `weak=False` 跳过 weak 放行判据，按后面三条比较决定是否补 `sv` 个空分隔
- **错误**：`parse` 失败 或 `validValueRange` 不过 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Aw2mnShare.hs:44`，`kParseFail`）；该串在 `TextCaps` 路径下作为普通返回串，不包 `ERR:`，也不是 `Either Left`

### `weak-a-omega2-mn-2` · wAω2MN2（idOrder 第 112 位）

- **族 / 模块**：Aw2mn · `src/OmegaY/Fam/Aw2mn/Aw2mn.hs` + `src/OmegaY/Fam/Aw2mn/Aw2mnShare.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`expandImpl True V2 True`（`Aw2mn.hs:10`，第二支 alter），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：括号/逗号/分号序数表达式文本 + `--term` → 山结构显示串
- **term / param 语义**：同 `a-omega2-mn-2`：`term = max 1 term0` 作 `index`；`param` 不参与
- **步骤**：走 §共用骨架 第 2–7 步；`kind = V2`、`weak = True`
- **分叉**：与 `a-omega2-mn-2` 只差 `weak` 布尔。`subtract1` 仍是 V2 分支（`Aw2mnShare.hs:313-336`）；`stretched` 多一条 weak 放行：`weak && verticalCompare ((v0 !! x) !! y) (topVerticals !! br) >= 0` 成立时直接保留 `entrySep`、不做分隔拉伸（`Aw2mnShare.hs:487`），强版本没有这条短路
- **错误**：同 `a-omega2-mn-2`：解析失败 / 值域不过 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Aw2mnShare.hs:44`）

### `a-omega2-mn-3` · Aω2MN3（idOrder 第 113 位）

- **族 / 模块**：Aw2mn · `src/OmegaY/Fam/Aw2mn/Aw2mn.hs` + `src/OmegaY/Fam/Aw2mn/Aw2mnShare.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`expandImpl False V3 True`（`Aw2mn.hs:10`，第二支 alter），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：括号/逗号/分号序数表达式文本 + `--term` → 山结构显示串
- **term / param 语义**：同前：`term = max 1 term0` 作 `index`；`param` 不参与
- **步骤**：走 §共用骨架 第 2–7 步；`kind = V3` 进 `subtract1` 的 V3 分支，`weak = False`
- **分叉**：`weak=False`（强）、`kind=V3`。`subtract1` V3 分支（`Aw2mnShare.hs:337-359`）：`mountainIsOne` 直接弹出不追加；否则先 `collectEntries`，`mountainIsLimit` 时从 `j0` 起 `loop` 补 `[]` 直到两条垂直序列相等，追加 `Entry topRightValue jFinal True`；非 limit 时截 `truncated`，垂直比较 `> 0` 才追加带 `truncated` 的非标记 entry。V3 与 V2 的差别在 limit/one 的先后顺序、补齐空分隔的循环方式，以及追加 entry 的 mark 条件
- **错误**：同前：解析失败 / 值域不过 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Aw2mnShare.hs:44`）

### `weak-a-omega2-mn-3` · wAω2MN3（idOrder 第 114 位）

- **族 / 模块**：Aw2mn · `src/OmegaY/Fam/Aw2mn/Aw2mn.hs` + `src/OmegaY/Fam/Aw2mn/Aw2mnShare.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`expandImpl True V3 True`（`Aw2mn.hs:10`，第二支 alter），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：括号/逗号/分号序数表达式文本 + `--term` → 山结构显示串
- **term / param 语义**：同前：`term = max 1 term0` 作 `index`；`param` 不参与
- **步骤**：走 §共用骨架 第 2–7 步；`kind = V3`、`weak = True`
- **分叉**：`subtract1` 走 V3 分支（`Aw2mnShare.hs:337-359`），`stretched` 带 weak 放行短路（`Aw2mnShare.hs:487`）：与 `weak-a-omega2-mn-2` 同一条 weak 判据，与 `a-omega2-mn-3` 同一个 V3 减法分支；是 `(weak ∈ {False, True}) × (V2, V3)` 四组合里的最后一格
- **错误**：同前：解析失败 / 值域不过 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Aw2mnShare.hs:44`）

## 本族的「共用骨架 + 变体分叉」判定

- **纯共用骨架 + 两个注册参数分叉**：四个 id 是同一个 `expandImpl weak kind longer`（`Aw2mn.hs:10`），同一份 `expand` / `extend` / `extend'` / `subtract1` / `getReferences` / `stretched` 实现，没有第四份独立内核；差异只由闭包传入的两个参数控制。
- **`weak` 布尔控制哪一步**：只在 `stretched`（`Aw2mnShare.hs:485`）的拉伸判据里——`Aw2mnShare.hs:487` 的 `weak && verticalCompare ((v0 !! x) !! y) (topVerticals !! br) >= 0` 是 weak 版本独有的放行短路；`weak=False` 时该条件不参与，entrySep 要继续过后面三条比较（`Aw2mnShare.hs:488-490`）才决定补不补空分隔。
- **`SubtractKind`（V2 / V3）控制哪一步**：只在 `subtract1`（`Aw2mnShare.hs:284`）的模式分支——V2 分支 `Aw2mnShare.hs:313-336`，V3 分支 `Aw2mnShare.hs:337-359`；两支对 `mountainIsLimit` / `mountainIsOne` 的判定顺序、`truncated` 构造、补齐空分隔的循环与追加 entry 的 `eMark` 条件各不相同。
- **`longer` 不是注册表参数**：它是 `textAlterNotation` 两支闭包各自带的布尔；活入口（第二支）`longer=True`，传到 `expand` 的 `shorter = not longer = False`（`Aw2mn.hs:22`、`Aw2mnShare.hs:558-560`），即末尾不再 `take` 掉最后一列。
- 全族共同点：`TextCaps` 不校验空文本与 `term` 正负；`param` 一律不参与；族内不返回 `Either Left`，解析/值域失败都回 `kParseFail` 普通串。
