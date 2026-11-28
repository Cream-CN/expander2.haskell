# Mn（11 个 id：`e0mn` `strong-e0mn` `omega-mn` `pcf-omega-mn` `t-omega-mn` `1-mn` `2-mn` `3-mn` `s-omega2-mn` `sa-omega2-mn` `s-omega-pow-omega-mn`）

**一句话导读**：这族把列结构表达式解析成内部树/山结构，按 `term` 次定位坏根列、重写列并复制拼接，再序列化回同形文本；
11 个 id 分落在五套骨架上（序数 Expr、整数分隔 Mountain、PCF `p:vertical`、递归 TExpr、多维 Sep Mountain），
对应 `docs/diagrams/04-kernel-flow/` 的**第四链（序数系）**：解析列 → 比较/求前驱 → 重写列 → 拼接输出。

## 共用骨架

1. **Caps 分派**：11 个 id 全部用 `textAlterNotation` 注册，进 `expandNotation` 的 `TextCaps` 分支
   （`src/OmegaY/Notation/Types.hs:64`），该分支不做任何前置校验；`param` 全族不参与，后缀全为 `""`。
2. **活的是第二支**：`textAlterNotation` 丢弃第一个展开参数、保留第二个（`Types.hs:45-46`）。
   各入口闭包第一支 `longer=False` 未接线；活支是第二支 `longer=True`，进实现后 `not longer` 即 `shorter=False`。
3. **term 兜底**：实现内统一 `term = max 1 term0`，CLI 的 `--term 0` 或缺省 0 等价 1。
4. **统一短路顺序**（各骨架解析器不同，顺序一致）：解析失败 → 返回族内 `errText`；表达式为空 → 空串；
   末列为空（非极限）→ 去掉末列；否则进入各骨架的展开函数，最后序列化拼后缀。
5. **族内错误不是 `Left`**：各 `expandImpl` 返回普通 `String`，解析失败/编号越界时把 `errText` 当作结果串返回，
   由 `Main.hs:55` 普通打印，**不加 `ERR:` 前缀**；本族不产生 §00 里 ParamCaps 区间校验与 SeqCaps 两条校验错误。

### 骨架归属（本手册最关键的分叉图）

| 骨架 | 入口文件 | 复用的共享模块 | id |
|---|---|---|---|
| 序数 Expr（`Entry a:Ordinal`） | `src/OmegaY/Fam/Mn/E0Mn.hs` | `src/OmegaY/Fam/Mn/MnOrdShare.hs` | `e0mn` `strong-e0mn` |
| 整数分隔 Mountain（`Entry value:int sep`） | `src/OmegaY/Fam/Mn/NMn.hs`、`src/OmegaY/Fam/Mn/OmegaMn.hs` | `src/OmegaY/Fam/Mn/SmnShare.hs` | `1-mn` `2-mn` `3-mn` `omega-mn` |
| PCF `p:vertical`（`PEntry p:[Int]`） | `src/OmegaY/Fam/Mn/PcfOmegaMn.hs` | 无（文件内自带比较/竖向函数） | `pcf-omega-mn` |
| 递归 TExpr（`TEntry value:TExpr sep`） | `src/OmegaY/Fam/Mn/TOmegaMn.hs` | 无（文件内自带竖向机制） | `t-omega-mn` |
| 多维 Sep Mountain（`SEntry value:[Int] sep`） | `src/OmegaY/Fam/Mn/SOmega2Mn.hs`、`src/OmegaY/Fam/Mn/SAOmega2Mn.hs`、`src/OmegaY/Fam/Mn/SOmegaPowOmegaMn.hs` | 无（三个文件各带一份同构副本） | `s-omega2-mn` `sa-omega2-mn` `s-omega-pow-omega-mn` |

> 注：`src/OmegaY/Fam/Mn/SOmega2Share.hs` 导出一整套 S 骨架，但 `src/` 内**没有任何模块 import 它**；
> 其解析器 `fromDisplay` 是 TODO 桩（`SOmega2Share.hs:132` 直接 `Nothing`），`magmaIndices` 内还有 `error "break"`
> （`SOmega2Share.hs:230`）。三个 S 系 id 走的是各自文件内的副本，不经过该共享模块。

### `e0mn` · e0MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/E0Mn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`E0Mn.expandImpl False True`（`E0Mn.hs:15`，签名 `E0Mn.hs:14`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False False`（`E0Mn.hs:30`）是未接线的第一支
- **输入 → 输出**：括号列文本（列内条目 `序号:序数`，条目间分号分隔）+ `--term` → 同形序数列文本
- **term / param 语义**：`term` = FS 迭代次数，`term = max 1 term0`（`E0Mn.hs:16`）；`param` 不参与
- **步骤**：
  1. `parseExpr` 解析 Expr 列（`MnOrdShare.hs:409`）；`Nothing` → `errText`（`E0Mn.hs:18`）
  2. `null e` → 返回空串（`E0Mn.hs:21-22`）
  3. `isLimitExpr e` 判定（`MnOrdShare.hs:84`）；非极限 → `exprToPlain (predecessor e)`（`E0Mn.hs:24`，`predecessor` 见 `MnOrdShare.hs:87`）
  4. 极限 → `fs e term (not longer) strong`（`E0Mn.hs:25`，`fs` 见 `MnOrdShare.hs:511`）；活支 `longer=True`，故 `shorter=False`，`strong=False`
  5. `exprToPlain` 序列化（`MnOrdShare.hs:347`）
- **分叉**：与 `strong-e0mn` 共用同一份 `expandImpl` 函数体；唯一开关是第 4 步传给 `fs` 的 `strong` 布尔。本 id 固定 `strong=False`：`down` 的 omit 判定走 `ordCompare xnL prevX <= 0`（`MnOrdShare.hs:477`），`shiftOrdNumbers` 走非 strong 分支（`MnOrdShare.hs:156-157`）
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`E0Mn.hs:18`，普通结果串返回，非 `Left`）；无其它自有错误

### `strong-e0mn` · strong e0MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/E0Mn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`E0Mn.expandImpl True True`（`E0Mn.hs:15`，签名 `E0Mn.hs:14`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl True False`（`E0Mn.hs:37`）是未接线的第一支
- **输入 → 输出**：括号列文本（条目 `序号:序数`，分号分隔）+ `--term` → 同形序数列文本
- **term / param 语义**：`term` = FS 迭代次数，`term = max 1 term0`（`E0Mn.hs:16`）；`param` 不参与
- **步骤**：
  1. `parseExpr` 解析 Expr 列（`MnOrdShare.hs:409`）；`Nothing` → `errText`（`E0Mn.hs:18`）
  2. `null e` → 返回空串（`E0Mn.hs:21-22`）
  3. `isLimitExpr e` 判定（`MnOrdShare.hs:84`）；非极限 → `exprToPlain (predecessor e)`（`E0Mn.hs:24`）
  4. 极限 → `fs e term (not longer) strong`（`E0Mn.hs:25`）；活支 `shorter=False`，`strong=True`
  5. `exprToPlain` 序列化（`MnOrdShare.hs:347`）
- **分叉**：与 `e0mn` 同骨架同函数体，只翻 `strong` 布尔：本 id 固定 `strong=True`。`down` 的 omit 判定改走 `ordEq xnL prevX`（`MnOrdShare.hs:476`）；`processRflCol` → `shiftOrdNumbers` 走 strong 递归分支（`MnOrdShare.hs:151-154`），序数指数也按 cutoff 平移
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`E0Mn.hs:18`，普通结果串返回，非 `Left`）；无其它自有错误

### `omega-mn` · ωMN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/OmegaMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`OmegaMn.expandImpl True`（`OmegaMn.hs:239`，签名 `OmegaMn.hs:238`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False`（`OmegaMn.hs:257`）是未接线的第一支
- **输入 → 输出**：Mountain 列文本（条目前导逗号数即分隔层级，可带括号）+ `--term` → 同形 Mountain 文本
- **term / param 语义**：`term` = 展开迭代次数，`term = max 1 term0`（`OmegaMn.hs:240`）；`param` 不参与
- **步骤**：
  1. `trimWS` 去首尾空白（`OmegaMn.hs:252`）；`null raw || raw == "0" || raw == "∅"` → 空串（`OmegaMn.hs:242`）
  2. `fromDisplayOmega` 解析 Mountain（`OmegaMn.hs:12`）；`Nothing` → `errText`（`OmegaMn.hs:245`）
  3. `null m` → 空串（`OmegaMn.hs:247`）；`not (isLimitMountain m)`（`OmegaMn.hs:109`，末列为空）→ `mountainDisplay (init m) False`（`OmegaMn.hs:248`，`mountainDisplay` 来自 `SmnShare.hs:106`）
  4. 极限 → `expandOmega m term (not longer)`（`OmegaMn.hs:249`）；活支 `shorter=False`
  5. `expandOmega`（`OmegaMn.hs:112`）：取末列顶条目，`columnVerticals` 求竖向（复用 `SmnShare.hs:219`），`parent` 定位父列（复用 `SmnShare.hs:249`），`adjustRightmost` 调整末列（`OmegaMn.hs:132`），`magmaChecksList` 算岩浆标记（`OmegaMn.hs:155`），`expandLoop` 按 `index` 次复制列（`OmegaMn.hs:198`）
  6. `mountainDisplay` 序列化（`OmegaMn.hs:249`）
- **分叉**：类型、比较、`parent`、竖向提升、显示复用 `SmnShare`，但**展开体是本地副本**：不走 `SmnShare.expand` 的 `fillGhost` → `iterate extend` → `clearGhost` 链，而是本地 `expandOmega`/`expandLoop`；解析器也是本地 `fromDisplayOmega`，不用 `SmnShare.fromDisplay`。另比 1/2/3-mn 多一步 `"0"`/`"∅"` 空串短路
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`OmegaMn.hs:245`，普通结果串返回，非 `Left`）；无其它自有错误

### `pcf-omega-mn` · PCF's ωMN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/PcfOmegaMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`PcfOmegaMn.expandImpl True`（`PcfOmegaMn.hs:313`，签名 `PcfOmegaMn.hs:312`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False`（`PcfOmegaMn.hs:328`）是未接线的第一支
- **输入 → 输出**：`(p:vertical,...)` 列文本，vertical 为 `ω^m*n+n` 形态序数竖向 + `--term` → 同形 `p:vertical` 列文本
- **term / param 语义**：`term` = 复制轮数 `w ∈ [1..index]`，`term = max 1 term0`（`PcfOmegaMn.hs:314`）；`param` 不参与
- **步骤**：
  1. `trimWS` 去首尾空白（`PcfOmegaMn.hs:310`）；`null raw || raw == "0" || raw == "∅"` → 空串（`PcfOmegaMn.hs:316`）
  2. `fromDisplayPcf` 解析 PExpr（`PcfOmegaMn.hs:298`，先 `normalizeW` 把 `ω/w/W` 归一）；`Nothing` → `errText`（`PcfOmegaMn.hs:319`）
  3. `null e` → 空串（`PcfOmegaMn.hs:321`）；`null (last e)` → `displayExpr (init e)`（`PcfOmegaMn.hs:322`）
  4. 极限 → `expandPcf e term (not longer)`（`PcfOmegaMn.hs:323`）；活支 `shorter=False`
  5. `expandPcf`（`PcfOmegaMn.hs:154`）：取末列顶条目得 `r/top`，`topSep` 取最高分隔位，`baseVertical` 求阈值竖向（`PcfOmegaMn.hs:97`），`normalizeCol` 标准化末列（`PcfOmegaMn.hs:114`），`magmaVerticals` 算各列岩浆竖向（`PcfOmegaMn.hs:134`），`copyColumn` 按 `w ∈ [1..index]`、`j ∈ [r+1..right]` 复制列（`PcfOmegaMn.hs:122`）
  6. `displayExpr` 序列化（`PcfOmegaMn.hs:197`）
- **分叉**：独立骨架，文件内自带一套 `verticalCompare`/`entryCompare`/`columnCompare`（`PcfOmegaMn.hs:36-45`）与竖向加减函数，不复用 `MnOrdShare` 也不复用 `SmnShare`；条目挂的是竖向 `[Int]` 而非整数 sep 或递归 Expr；无 ghost 填充、无递归分隔符展开
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`PcfOmegaMn.hs:319`，普通结果串返回，非 `Left`）；无其它自有错误

### `t-omega-mn` · TωMN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/TOmegaMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`TOmegaMn.expandImpl True`（`TOmegaMn.hs:283`，签名 `TOmegaMn.hs:282`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False`（`TOmegaMn.hs:298`）是未接线的第一支
- **输入 → 输出**：递归列文本（条目 `sep值`，sep 为逗号串或嵌套括号列）+ `--term` → 同形递归列文本
- **term / param 语义**：`term` = 外层展开迭代次数；递归进分隔符时次数由 `threshold` 另算，`term = max 1 term0`（`TOmegaMn.hs:284`）；`param` 不参与
- **步骤**：
  1. `trimWS` 去首尾空白（`TOmegaMn.hs:280`）；`null raw || raw == "0" || raw == "∅"` → 空串（`TOmegaMn.hs:286`）
  2. `fromDisplayT` 解析 TExpr（`TOmegaMn.hs:274`，`parseExpr` 递归吃括号列）；`Nothing` → `errText`（`TOmegaMn.hs:289`）
  3. `null m` → 空串（`TOmegaMn.hs:291`）；`not (isLimit m)`（`TOmegaMn.hs:18`，末列为空）→ `mountainDisplay' (init m) False`（`TOmegaMn.hs:292`）
  4. 极限 → `expand m term (not longer)`（`TOmegaMn.hs:293`）；活支 `shorter=False`
  5. `expand`（`TOmegaMn.hs:112`）：取末列顶条目，`columnVerticals` 求竖向，`parent` 定位父列；先判 `isLimit topRightSep`（`TOmegaMn.hs:124`）——分隔符本身是极限时，用 `threshold`（`TOmegaMn.hs:105`）算出加项后**递归 `expand topRightSep`**（`TOmegaMn.hs:128`）；否则走减一/替换分隔符、拼父列尾部、算 `magmaChecksList`、`stepN` 按 `index` 次建目标列（`TOmegaMn.hs:168-201`）
  6. `mountainDisplay'` 序列化（`TOmegaMn.hs:54`）
- **分叉**：独立骨架，条目分隔符本身是一棵 `TExpr`（可嵌套递归），文件内自带竖向比较/父链/引用函数；这是全族唯一会递归进分隔符内部展开的实现；不复用任何族内共享模块
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`TOmegaMn.hs:289`，普通结果串返回，非 `Left`）；无其它自有错误

### `1-mn` · 1MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/NMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`NMn.expandImpl 1 True`（`NMn.hs:14`，签名 `NMn.hs:13`；闭包由 `nMnNotation 1 "1-mn" "1MN"` 装配，`Family.hs:18`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl 1 False`（`NMn.hs:26`）是未接线的第一支
- **输入 → 输出**：Mountain 列文本（条目前导逗号数即分隔层级，可带括号）+ `--term` → 同形 Mountain 文本
- **term / param 语义**：`term` = 展开迭代次数，`term = max 1 term0`（`NMn.hs:15`）；`param` 不参与。注册传入的整数 `n=1` 被闭包捕获，**函数体未读取**（见 §分叉）
- **步骤**：
  1. `fromDisplay` 解析 Mountain（`SmnShare.hs:112`）；`Nothing` → `errText`（`NMn.hs:17`）
  2. `null m` → 空串（`NMn.hs:19`）；`null (last m)` → `mountainDisplay (init m) False`（`NMn.hs:20`）
  3. 极限 → `expand m term (not longer)`（`NMn.hs:21`）；活支 `shorter=False`
  4. `SmnShare.expand`（`SmnShare.hs:398`）：先 `fillGhost` 补幽灵条目（`SmnShare.hs:286`），`iterate extend` 取第 `index` 次（`SmnShare.hs:379`），`shorter=False` 时 `subtract1 iter Nothing`（`SmnShare.hs:320`），最后 `clearGhost` 过滤 value=0 条目（`SmnShare.hs:317`）
  5. `mountainDisplay` 序列化（`SmnShare.hs:106`）
- **分叉**：与 `2-mn` `3-mn` 是同一函数 `expandImpl n`、同一套 `SmnShare` 骨架；分叉参数是注册时传入的整数 `n`，但函数体（`NMn.hs:14-21`）没有任何分支读取 `n`，三个 id 行为完全一致。与 `omega-mn` 虽同用 `SmnShare` 的类型与显示，但展开走共享 `expand`，不是 `omega-mn` 的本地 `expandOmega`
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`NMn.hs:17`，普通结果串返回，非 `Left`）；无其它自有错误

### `2-mn` · 2MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/NMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`NMn.expandImpl 2 True`（`NMn.hs:14`，签名 `NMn.hs:13`；闭包由 `nMnNotation 2 "2-mn" "2MN"` 装配，`Family.hs:19`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl 2 False`（`NMn.hs:26`）是未接线的第一支
- **输入 → 输出**：Mountain 列文本（条目前导逗号数即分隔层级，可带括号）+ `--term` → 同形 Mountain 文本
- **term / param 语义**：`term` = 展开迭代次数，`term = max 1 term0`（`NMn.hs:15`）；`param` 不参与。注册传入的整数 `n=2` 被闭包捕获，**函数体未读取**
- **步骤**：
  1. `fromDisplay` 解析 Mountain（`SmnShare.hs:112`）；`Nothing` → `errText`（`NMn.hs:17`）
  2. `null m` → 空串（`NMn.hs:19`）；`null (last m)` → `mountainDisplay (init m) False`（`NMn.hs:20`）
  3. 极限 → `expand m term (not longer)`（`NMn.hs:21`）；活支 `shorter=False`
  4. `SmnShare.expand`（`SmnShare.hs:398`）：`fillGhost`（`SmnShare.hs:286`）→ `iterate extend`（`SmnShare.hs:379`）→ `subtract1 iter Nothing`（`SmnShare.hs:320`）→ `clearGhost`（`SmnShare.hs:317`）
  5. `mountainDisplay` 序列化（`SmnShare.hs:106`）
- **分叉**：与 `1-mn` `3-mn` 同骨架同函数体；整数 `n=2` 仅闭包捕获、函数体未读取，行为与另两条一致，无独立判定或重写规则差异
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`NMn.hs:17`，普通结果串返回，非 `Left`）；无其它自有错误

### `3-mn` · 3MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/NMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`NMn.expandImpl 3 True`（`NMn.hs:14`，签名 `NMn.hs:13`；闭包由 `nMnNotation 3 "3-mn" "3MN"` 装配，`Family.hs:20`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl 3 False`（`NMn.hs:26`）是未接线的第一支
- **输入 → 输出**：Mountain 列文本（条目前导逗号数即分隔层级，可带括号）+ `--term` → 同形 Mountain 文本
- **term / param 语义**：`term` = 展开迭代次数，`term = max 1 term0`（`NMn.hs:15`）；`param` 不参与。注册传入的整数 `n=3` 被闭包捕获，**函数体未读取**
- **步骤**：
  1. `fromDisplay` 解析 Mountain（`SmnShare.hs:112`）；`Nothing` → `errText`（`NMn.hs:17`）
  2. `null m` → 空串（`NMn.hs:19`）；`null (last m)` → `mountainDisplay (init m) False`（`NMn.hs:20`）
  3. 极限 → `expand m term (not longer)`（`NMn.hs:21`）；活支 `shorter=False`
  4. `SmnShare.expand`（`SmnShare.hs:398`）：`fillGhost`（`SmnShare.hs:286`）→ `iterate extend`（`SmnShare.hs:379`）→ `subtract1 iter Nothing`（`SmnShare.hs:320`）→ `clearGhost`（`SmnShare.hs:317`）
  5. `mountainDisplay` 序列化（`SmnShare.hs:106`）
- **分叉**：与 `1-mn` `2-mn` 同骨架同函数体；整数 `n=3` 仅闭包捕获、函数体未读取，行为与另两条一致，无独立判定或重写规则差异
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`NMn.hs:17`，普通结果串返回，非 `Left`）；无其它自有错误

### `s-omega2-mn` · Sω2MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/SOmega2Mn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SOmega2Mn.expandImpl True`（`SOmega2Mn.hs:373`，签名 `SOmega2Mn.hs:372`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False`（`SOmega2Mn.hs:386`）是未接线的第一支
- **输入 → 输出**：列文本，条目 `sep值`，sep 为逗号/分号计数串 + `--term` → 同形列文本
- **term / param 语义**：`term` = `extend` 迭代轮数（`iterate extend m !! index`），`term = max 1 term0`（`SOmega2Mn.hs:374`）；`param` 不参与
- **步骤**：
  1. `fromDisplayS` 解析 SMountain（`SOmega2Mn.hs:299`）；`Nothing` → `errText`（`SOmega2Mn.hs:376`）
  2. `null m` → 空串（`SOmega2Mn.hs:378`）；编号越界检查：任一 `SEntry v _` 满足 `v < 0 || v > length m` → `errText`（`SOmega2Mn.hs:379`）
  3. `null (last m)` → `mountainDisplay (init m)`（`SOmega2Mn.hs:380`）
  4. 极限 → `expandS m term (not longer)`（`SOmega2Mn.hs:381`）；活支 `shorter=False`
  5. `expandS`（`SOmega2Mn.hs:287`）：`iterate extend m !! index`；`shorter=False` 时再对结果做一次 `subtract1` + `stretchDataTop`（`SOmega2Mn.hs:293`）。单步 `extend`（`SOmega2Mn.hs:266`）：`columnVerticals` 求竖向、`parentP` 定位父列、`magmaIndices` 算岩浆列、`stretchDataList` 算每列拉伸数据（`SOmega2Mn.hs:196`）、`subtract1` 减一（`SOmega2Mn.hs:214`）、`copyColumn` 复制列（`SOmega2Mn.hs:233`）
  6. `mountainDisplay` 序列化（`SOmega2Mn.hs:64`）
- **分叉**：S 骨架基准版。`sFn`（`SOmega2Mn.hs:159`）按 bound 比较取阈值/拉伸到；`stretchDataTop`（`SOmega2Mn.hs:171`）按 `sepDimension topRightSep > 0` 分两支：高维走 `sFn` 取 threshold/stretchTo，一维则阈值为首项减一；`sdForce` 布尔决定 `subtract1` 是否补插新条目（`SOmega2Mn.hs:221-222`）。与 `sa-omega2-mn` 条目结构不同（本版无 mark 布尔），与 `s-omega-pow-omega-mn` 的 `sFn` 回退规则不同
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmega2Mn.hs:376`）；编号越界（`v < 0 || v > length m`）→ 同一 `errText`（`SOmega2Mn.hs:379`）；均为普通结果串返回，非 `Left`

### `sa-omega2-mn` · SAω2MN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/SAOmega2Mn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SAOmega2Mn.expandImpl True`（`SAOmega2Mn.hs:346`，签名 `SAOmega2Mn.hs:345`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False`（`SAOmega2Mn.hs:359`）是未接线的第一支
- **输入 → 输出**：列文本，条目 `sep值`，带 mark 的条目写作 `sep*值` + `--term` → 同形列文本（mark 随复制保留）
- **term / param 语义**：`term` = `extend` 迭代轮数，`term = max 1 term0`（`SAOmega2Mn.hs:347`）；`param` 不参与
- **步骤**：
  1. `fromDisplayA` 解析 AMountain（`SAOmega2Mn.hs:310`，遇 `*` 记 `aeMark=True`）；`Nothing` → `errText`（`SAOmega2Mn.hs:349`）
  2. `null m` → 空串（`SAOmega2Mn.hs:351`）；编号越界检查：任一 `AEntry v _ _` 满足 `v < 0 || v > length m` → `errText`（`SAOmega2Mn.hs:352`）
  3. `null (last m)` → `mountainDisplay (init m)`（`SAOmega2Mn.hs:353`）
  4. 极限 → `expandA m term (not longer)`（`SAOmega2Mn.hs:354`）；活支 `shorter=False`
  5. `expandA`（`SAOmega2Mn.hs:294`）：`iterate extend m !! index`；`shorter=False` 时 `subtract1 current v (stretchDataTop current v)`（`SAOmega2Mn.hs:304`）。单步 `extend`（`SAOmega2Mn.hs:274`）：`parentP`/`magmaIndices`/`stretchDataList`/`subtract1`/`copyColumn` 同 S 骨架版式
  6. `mountainDisplay` 序列化（`SAOmega2Mn.hs:65`，mark 条目前缀 `*`，`SAOmega2Mn.hs:60`）
- **分叉**：与 `s-omega2-mn` **不是同一 id 的别名**，是 S 骨架的带 mark 变体：条目类型多一个 `aeMark` 布尔（`SAOmega2Mn.hs:13`）。三处判定随之改变——`sFn`（`SAOmega2Mn.hs:159`）只取带 mark 的条目作阈值；`stretchDataTop`（`SAOmega2Mn.hs:169`）返回 `Maybe StretchData`，分隔符首项非 0 时为 `Nothing`；`subtract1` 的 `step2`（`SAOmega2Mn.hs:214-227`）是三分支：`sepIsOne` 直接弹出 / `sepDimension == 0` 减首项后按竖向比较决定补插 / 高维按 `sdForce` 补插带 mark 条目。`entryCompare` 末位再比 `aeMark`（`SAOmega2Mn.hs:41-46`）
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SAOmega2Mn.hs:349`）；编号越界（`v < 0 || v > length m`）→ 同一 `errText`（`SAOmega2Mn.hs:352`）；均为普通结果串返回，非 `Left`

### `s-omega-pow-omega-mn` · Sω^ωMN

- **族 / 模块**：Mn · `src/OmegaY/Fam/Mn/SOmegaPowOmegaMn.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`SOmegaPowOmegaMn.expandImpl True`（`SOmegaPowOmegaMn.hs:391`，签名 `SOmegaPowOmegaMn.hs:390`），由 `expandNotation` 的 `TextCaps` 分支调用；同文件 `expandImpl False`（`SOmegaPowOmegaMn.hs:404`）是未接线的第一支
- **输入 → 输出**：列文本，条目 `sep值`；sep 除逗号/分号串外还支持 `[a,b,...]` 多维写法 + `--term` → 同形列文本（多维 sep 显示为 `[...]`）
- **term / param 语义**：`term` = `extend` 迭代轮数，`term = max 1 term0`（`SOmegaPowOmegaMn.hs:392`）；`param` 不参与
- **步骤**：
  1. `fromDisplayP` 解析 PMountain（`SOmegaPowOmegaMn.hs:305`，`parseSep` 支持 `[...]` 多维列表，`SOmegaPowOmegaMn.hs:324-336`）；`Nothing` → `errText`（`SOmegaPowOmegaMn.hs:394`）
  2. `null m` → 空串（`SOmegaPowOmegaMn.hs:396`）；编号越界检查：任一 `PEntry v _` 满足 `v < 0 || v > length m` → `errText`（`SOmegaPowOmegaMn.hs:397`）
  3. `null (last m)` → `mountainDisplay (init m)`（`SOmegaPowOmegaMn.hs:398`）
  4. 极限 → `expandP m term (not longer)`（`SOmegaPowOmegaMn.hs:399`）；活支 `shorter=False`
  5. `expandP`（`SOmegaPowOmegaMn.hs:289`）：`iterate extend m !! index`；`shorter=False` 时 `subtract1 current v (stretchDataTop current v)`（`SOmegaPowOmegaMn.hs:299`）。单步 `extend`（`SOmegaPowOmegaMn.hs:269`）：`parentP`/`magmaIndices`/`stretchDataList`/`subtract1`/`copyColumn` 同 S 骨架版式
  6. `mountainDisplay` 序列化（`SOmegaPowOmegaMn.hs:63`，多维 sep 走 `[...]` 输出，`SOmegaPowOmegaMn.hs:52-55`）
- **分叉**：S 骨架的多维变体。两处规则与 `s-omega2-mn` 不同：`sFn`（`SOmegaPowOmegaMn.hs:164`）在越界或当前 sep 不低于 bound 时回退 `sDefault bound`（`SOmegaPowOmegaMn.hs:157`，把维度位减一，单维 1 则归空），而不是返回 `[]`；`stretchDataTop`（`SOmegaPowOmegaMn.hs:176`）三分支：`sepIsOne` → 不 force / `sepDimension > 0` → 按 stretchTo 与 `sepIncrease threshold (dim-1)` 比较定 force / 一维 → 按竖向比较定 force
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`SOmegaPowOmegaMn.hs:394`）；编号越界（`v < 0 || v > length m`）→ 同一 `errText`（`SOmegaPowOmegaMn.hs:397`）；均为普通结果串返回，非 `Left`
