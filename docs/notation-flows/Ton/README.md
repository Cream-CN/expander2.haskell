# Ton（9 个 id：`ton-m` `ton-i` `ton-dr` `ton-drc` `ton-drp` `ton-drpc` `ton-ibp` `ton-mc` `ton-mpc`）

**一句话导读**：九 id 共用 `TonCore` 的 TON 树解析 / 生成状态机，差别**只在 `TonConfig` 四个字段**（`tcMainStyle` / `tcIbpLimit` / `tcMark` / `tcStdFn`）；生成器在记忆化状态里逐步 `tonStep` → `expandBeta` 扩层，`stdFn` 做结构与序数判定。对应 `docs/diagrams/04-kernel-flow/` 的**第四链（序数系）**：解析成树 → 比较/判定（本族为 `compareNoraise`/`compareRaise` + 各 `builtq*`）→ 前驱式退化（`tonStep`/`regressRepeated`）→ 重写/扩层（`expandBeta`）→ 拼接显示。

## 共用骨架

9 个 id 都经 `Ton.Family.mkNot`（`src/OmegaY/Fam/Ton/Family.hs:8-13`）注册；`mkNot` 用 `textAlterNotation`，两支 lambda 同体（`\text term -> tonExpandText cfg text term`），按 §00 规则活入口是**第二参数**；9 条全是 `TextCaps`，后缀 `""`，CLI 不校验空文本、不校验 `term` 正负。

1. **入口**：`tonExpandText cfg text term0`（`src/OmegaY/Fam/Ton/TonCore.hs:551-556`）：`term = if term0 < 0 then 0 else term0`（`553`）；`parseImpl text (tcMainStyle cfg)`（`181-212`）；`Nothing` → 返回解析失败 `Left`（文本见 §错误）；`Just t` → `evalState (expand cfg t term) initialTonState`（`556`；初始状态 `410-411`）。
2. **解析**（`parseImpl`，`181-212`）：`0` → 负叶 `negOneLeaf`；`Ω`——`mainStyle` 下读 `<n>` 数字成数值叶，非 mainStyle 成零叶；`C` 弹两叶合成 `TNode`；其余字符 / 括号不匹配 → `Nothing`。
3. **分派两支**（`expand`，`558-581`）：
   - `tcMainStyle = True`：`sys =` 叶值，否则 `maxValue t`（`561-563`）；`sys == kInf` → 直接构造 `n` 层 `TNode negOneLeaf` 树并 `displayMain`（`564-569`）；否则 `raise t sys`（`571`）；`tcMark` 为 `Just markFn` 且 `sys >= 1` 且 raised 与 `markFn sys` 同形（`flattenKey` 相等）→ `displayMain (markFs sys n)`（`572-574`）；否则 `cacheFs raised sys n True (tcStdFn cfg)` → `displayMain out`（`576-577`）。
   - `tcMainStyle = False`：`t' = if isInf t then makeLimit (tcIbpLimit cfg) else t`（`579`；`makeLimit` `543-549`）；`cacheFs t' 0 n False (tcStdFn cfg)` → `displayNoraise out`（`580-581`）。
4. **记忆化生成器**（`cacheFs`，`504-532`）：按 `flattenKey term` 查 `tsCache`，命中则恢复 `GenState`；否则起 `GenState term True (flattenSize term)`；`length vals <= n` 时反复 `tonGenNext` 生成并追加，回填缓存，返回 `vals !! n`。
5. **逐步生成**（`tonGenNext`，`449-488`）：`tonStep`（`415-427`，深链走 `walkSpine` `429-445`）得新 beta 与 `ok`；`flattenSize < gsLen + n*2` 内调 `stdFn`，通过则 `expandBeta`（`490-500`）扩一层；出循环时 mainStyle 先 `regressRepeated`（`223-226`）再输出。
6. **判定链**（`Variants.hs` 各 `standardQ*`）：`stdMemo`（`19-32`）按 `flattenKey` 记忆化 → `structOk`/`structOkN`（`34-48`）校验左右子结构与 `compareRaise` 序条件 → 进入各 `builtq*` 递归判定。
7. **显示**：`displayMain`（`173-177`）叶出 `Ω<v>` / 负叶 `0`，节点后缀 `C`；`displayNoraise`（`167-171`）叶只出 `0` / `Ω`，节点后缀 `C`。

## 配置分叉表（9 id 的唯一差别）

| id | cfg（`Variants.hs`） | `tcMainStyle` | `tcIbpLimit` | `tcMark` | `tcStdFn`（`standardQ*` / `builtq*`） |
|---|---|---|---|---|---|
| `ton-m` | `tonMConfig` `70-76` | True | False | `Just markMain`（`TonCore.hs:230-236`） | `standardQMain` `64-68` / `builtqMain` `52-60` |
| `ton-i` | `tonIConfig` `115-121` | False | True | `Nothing` | `standardQI` `96-107` / `builtqI` `80-91` |
| `ton-dr` | `tonDrConfig` `148-154` | False | False | `Nothing` | `standardQDor` `140-146` / `builtqDor` `125-135` |
| `ton-drc` | `tonDrcConfig` `185-191` | False | False | `Nothing` | `standardQDrc` `177-183` / `builtqDrc` `158-172` |
| `ton-drp` | `tonDrpConfig` `218-224` | False | False | `Nothing` | `standardQDrp` `210-216` / `builtqDrp` `195-205` |
| `ton-drpc` | `tonDrpcConfig` `267-273` | False | False | `Nothing` | `standardQDrpc` `259-265` / `builtqDrpc` `228-257` |
| `ton-ibp` | `tonIbpConfig` `317-323` | False | True | `Nothing` | `standardQIbp` `305-315` / `builtqIbp` `277-303` |
| `ton-mc` | `tonMcConfig` `375-381` | True | False | `Just markMc`（`TonCore.hs:238-244`） | `standardQMc` `369-373` / `builtqMc` `338-367` |
| `ton-mpc` | `tonMpcConfig` `423-429` | True | False | `Just markMc` | `standardQMpc` `416-421` / `builtqMpc` `383-414` |

### `ton-m` · TON（idOrder #72）

- **族 / 模块**：Ton · `src/OmegaY/Fam/Ton/Variants.hs`（`tonMConfig`）+ `src/OmegaY/Fam/Ton/TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`textAlterNotation` 注册（`Family.hs:17`），活入口第二参数
- **入口**：`TonCore.tonExpandText tonMConfig`（`TonCore.hs:551`；注册 lambda `Family.hs:11-12`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：TON main 文本（`0` / `Ω<n>` / `C` 后缀）+ `--term` → `displayMain` 文本
- **term / param 语义**：`term` = 生成序号（`cacheFs` 取 `vals !! n`），`< 0` 兜底为 0（`TonCore.hs:553`）；`param` 不参与
- **步骤**：走 §共用骨架 1–7；mainStyle 支（第 3 步上半）；`tcMark = Just markMain`，`sys >= 1` 且 raised 同形于 `markMain sys` 时走 `markFs sys n`（`TonCore.hs:572-574, 246-253`）；`stdFn = standardQMain` → `builtqMain n a (rightChild a) (rightChild a)`（`Variants.hs:68`），`builtqMain` 按 `n` 递减递归 + `compareRaise` 比较（`52-60`）
- **分叉**：九族里唯一的 `markMain` 标记支；判定用带 `sys` 参数的 `structOkN` / `standardQMain`
- **错误**：解析失败 → `解析失败，请确认输入是 TON 文本（0 / Ω / Ω<n> / C 后缀），且不是全角字符`（`TonCore.hs:555`）；无其它自有错误

### `ton-i` · TON_I（idOrder #73）

- **族 / 模块**：Ton · `src/OmegaY/Fam/Ton/Variants.hs`（`tonIConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:18`
- **入口**：`TonCore.tonExpandText tonIConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON noraise 文本 + `--term` → `displayNoraise` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；noraise 支（第 3 步下半）；`tcIbpLimit = True`，极限树走 `makeLimit True`（`TonCore.hs:579, 545-548`）；`tcMark = Nothing`；`stdFn = standardQI` → `collectSmallindex (rightChild a)` 取小索引路径（`Variants.hs:101`），`getA2`/`getN` 算 `nv`（`105-106`），`builtqI ex a nv ex`（`102`）
- **分叉**：与 `ton-ibp` 同用 `collectSmallindex` 路径采集；`builtqI` 按叶值 `<0`/`==0`/`>0` 三态与 `compareNoraise` 递归（`80-91`）
- **错误**：仅 §共用骨架 第 1 步那条解析失败文本（`TonCore.hs:555`）

### `ton-dr` · TON_DoR（idOrder #74）

- **族 / 模块**：Ton · `Variants.hs`（`tonDrConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:19`
- **入口**：`TonCore.tonExpandText tonDrConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON noraise 文本 + `--term` → `displayNoraise` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；noraise 支；`tcIbpLimit = False`（极限树 `makeLimit False`，`TonCore.hs:549`）；`stdFn = standardQDor` → `collectSmallpart (rightChild a)` 取小子树（`Variants.hs:145`），`builtqDor x a x`（`146`）
- **分叉**：`builtqDor` 叶值 `<0` 时比较 `compareNoraise x b`，小值节点要求左右子树都通过（`125-135`）；不做 `rConfig` 配置比较
- **错误**：仅解析失败文本（`TonCore.hs:555`）

### `ton-drc` · TON_DRC（idOrder #75）

- **族 / 模块**：Ton · `Variants.hs`（`tonDrcConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:20`
- **入口**：`TonCore.tonExpandText tonDrcConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON noraise 文本 + `--term` → `displayNoraise` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；noraise 支；`tcIbpLimit = False`；`stdFn = standardQDrc` → `collectSmallpart` 采集（`Variants.hs:182`），`builtqDrc x a a x a`（`183`）
- **分叉**：小值节点内先 `rConfig x xp` 与 `rConfig a ap` 做 raise 配置比较（`Variants.hs:166-167`；`rConfig` `TonCore.hs:154-163`），再递归左右子树（`168-172`）；这是与 `ton-dr` 的唯一结构差
- **错误**：仅解析失败文本（`TonCore.hs:555`）

### `ton-drp` · TON_DRP（idOrder #76）

- **族 / 模块**：Ton · `Variants.hs`（`tonDrpConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:21`
- **入口**：`TonCore.tonExpandText tonDrpConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON noraise 文本 + `--term` → `displayNoraise` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；noraise 支；`tcIbpLimit = False`；`stdFn = standardQDrp` → `collectSmallpart` 采集（`Variants.hs:215`），`builtqDrp x x a (rightChild a) negOneLeaf`（`216`）
- **分叉**：`builtqDrp` 带 passthrough 通道 `d`（初值 `negOneLeaf`），按 `compareNoraise a d` 决定是否把通道下传（`195-205`）；不做 `rConfig`
- **错误**：仅解析失败文本（`TonCore.hs:555`）

### `ton-drpc` · TON_DRPC（idOrder #77）

- **族 / 模块**：Ton · `Variants.hs`（`tonDrpcConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:22`
- **入口**：`TonCore.tonExpandText tonDrpcConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON noraise 文本 + `--term` → `displayNoraise` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；noraise 支；`tcIbpLimit = False`；`stdFn = standardQDrpc` → `collectSmallpart` 采集（`Variants.hs:264`），`builtqDrpc x a x a (rightChild a) negOneLeaf`（`265`）
- **分叉**：`builtqDrpc` 同时带 `rConfig` 配置比较与 passthrough 通道 `d`：否定支先算 `rConfig a ap`、`rConfig ai b` 比较（`235-237`）；递归支按通道与左右子树分派（`244-257`）；可视为 `ton-drc` 与 `ton-drp` 两支特征的合并
- **错误**：仅解析失败文本（`TonCore.hs:555`）

### `ton-ibp` · TON_IBP（idOrder #78）

- **族 / 模块**：Ton · `Variants.hs`（`tonIbpConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:23`
- **入口**：`TonCore.tonExpandText tonIbpConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON noraise 文本 + `--term` → `displayNoraise` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；noraise 支；`tcIbpLimit = True`（`makeLimit True`）；`stdFn = standardQIbp` → `collectSmallindex` 采集（`Variants.hs:310`），`getA2`/`getN` 算 `nv`（`312-313`），`builtqIbp ex a a2 nv`（`314`）
- **分叉**：`builtqIbp` 遍历全子项路径，除直接递归外还有 `checkY`/`yCheck`/`zCheck` 的前缀路径与子树前缀检查（`282-303`）；与 `ton-i` 共用路径采集，判定范围更大
- **错误**：仅解析失败文本（`TonCore.hs:555`）

### `ton-mc` · TON_MC（idOrder #79）

- **族 / 模块**：Ton · `Variants.hs`（`tonMcConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:24`
- **入口**：`TonCore.tonExpandText tonMcConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON main 文本 + `--term` → `displayMain` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；mainStyle 支；`tcMark = Just markMc`（`TonCore.hs:238-244`）；`stdFn = standardQMc` → `structOkN standardQMc n a`，`builtqMc (rightChild a) a n`（`Variants.hs:371-373`）
- **分叉**：`builtqMc` 对每个子项路径做 `rConfig` 比较，`refreshTotest` 取待检子树、`searchY`/`checkY` 做前缀与待检集判定（`338-367`）；标记函数是 `markMc` 而非 `markMain`
- **错误**：仅解析失败文本（`TonCore.hs:555`）

### `ton-mpc` · TON_MPC（idOrder #80）

- **族 / 模块**：Ton · `Variants.hs`（`tonMpcConfig`）+ `TonCore.hs` · 文本型 `TextCaps`，后缀 `""`；`Family.hs:25`
- **入口**：`TonCore.tonExpandText tonMpcConfig`（`TonCore.hs:551`），`TextCaps` 分支
- **输入 → 输出**：TON main 文本 + `--term` → `displayMain` 文本
- **term / param 语义**：`term` = 生成序号；`param` 不参与
- **步骤**：骨架 1–7；mainStyle 支；`tcMark = Just markMc`；`stdFn = standardQMpc`：先算 `rc = rConfig (rightChild a) a`（`Variants.hs:418`），`structOkN standardQMpc n a`，`builtqMpc (rightChild a) a rc n`（`421`）
- **分叉**：`builtqMpc` 在 `builtqMc` 的 `rConfig`/`refreshTotest`/`searchY` 判定之外，递归出口多一条 `compareRaise (rConfig (extractPath a y) parent) rc == LT` 的 `rc` 比较（`Variants.hs:413`）；这是与 `ton-mc` 的唯一结构差
- **错误**：仅解析失败文本（`TonCore.hs:555`）

## 本族的「共用骨架 + 独有分支」判定

- **纯共用骨架 + 配置分叉**：9 条全部同一份 `tonExpandText` / `expand` / `cacheFs` / `tonGenNext` 状态机；分叉只在 `TonConfig` 四字段——`tcMainStyle` 选 main/noraise 显示支，`tcIbpLimit` 选极限树形状，`tcMark` 选标记函数（或无标记），`tcStdFn` 选判定链。
- **配置簇**：
  - main 支：`ton-m` / `ton-mc` / `ton-mpc`（`markMain` 或 `markMc`）；
  - noraise 支：其余 6 条，其中 `ton-i` / `ton-ibp` 用 `collectSmallindex` + `getA2/getN`，`ton-dr`/`ton-drc`/`ton-drp`/`ton-drpc` 用 `collectSmallpart`。
- **无独有实现分支**：没有任何一条 id 绕过 `TonCore` 状态机另起算法；`builtq*` 之间的差异都收在 `tcStdFn` 之内。
