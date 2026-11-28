# G11242（5 个 id：`srd` `rpd` `ard` `ard2` `ipd`）

**一句话导读**：11242 家族五个缩写都在「列 / 条目图」上做预算受限的展开，但数据结构分三套——`srd` 是父项列表列图（`Srd.hs`），`rpd`/`ard`/`ard2` 是三元边图（`Graph.hs`，差别在合法性判定、移动规则与前驱规则），`ipd` 是节点共享树（hash-cons）条目图（`Ipd.hs`）。五者不直接对应 `docs/diagrams/04-kernel-flow/` 五条链中任何一条；其「取控制条目 → 定 cut/width → 按 block 复制列」的回写骨架与**第一链（矩阵系）**/第五链（基础序列系）的复制动作同构，条目比较与前驱在 `ipd` 内是独立的共享树实现。

## 五个缩写各算什么（以源码为准）

- **SRD**（`Srd.hs`）：父项列表 `[父列…]` 列图；`fs` 按 `span_` 复制坏部列，seam 由末列父项与上一列拼出；预算上限列数 `kMaxColumns = 8192`、单元数 `kMaxCells = 250000`（`Srd.hs:13-15`）。
- **RPD**（`Graph.hs`，`Kind RPD`）：三元边 `(k, p, q)`；规范化时把 `q` 从 0 展开到 `eQ`；移动时只平移 `p`/`q`，`k` 不动；控制条目按 `pairCompare` 取最大。
- **ARD**（`Kind ARD`）：三元边；合法性判定额外允许 `k > p`；seam 走 `predecessor` 前驱；移动时 `k`/`p`/`q` 一起平移。
- **ARD2**（`Kind ARD2`）：移动/规范化同 ARD；合法性判定放宽为按当前列号 `j` 检查 `k > j || q > j`；前驱减层时根标取 `child`。
- **IPD**（`Ipd.hs`）：节点共享树（`make` hash-cons），边 `p:d/t`；`lower` 求前驱条目，`controller` 取控制边，`expandGraph` 按 index 轮次复制；输出节点文本拼接（`gKey`）。

## 共用骨架：State 单子

- `src/OmegaY/Fam/G11242/State.hs`：`M s a = M { unM :: s -> Either String (a, s) }`（`State.hs:11`）；`runM`（`32-35`）跑完全程，`Left e` 直接作为输出文本（`Main` 再套 `ERR:` 前缀）；`get`/`put`/`modify`（`37-44`）；`tick` 工作量计数（`47-48`）。
- 各入口形态一致：`runM (parse … >>= 展开) 初始状态`；`Left e` → 错误串原样返回；`Right` → 序列化。三套实现各自定义自己的 `failM` 与预算 tick（不复用对方的计数状态）。

### `srd` · SRD（idOrder #115）

- **族 / 模块**：G11242 · `src/OmegaY/Fam/G11242/Srd.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Srd.expandSrd`（`Srd.hs:183-187`；注册 `srdNotation` `189-190`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：父项列表 `[][1][2,2]`、纯数字空列数、或 `SRD[n]…` / `SRPD[n]…` 路由文本 + `--term` → `[列…]` 纯列表；空图输出 `0`
- **term / param 语义**：`term` = 坏部复制轮数（`fs` 的 `n`）；`param` 不参与
- **步骤**：
  1. `expandSrd`：`runM (parse text >>= \g -> fs g term) 0`（`185`）
  2. `parse`（`121-181`）：`stripAll`（`105`）去空白；`eatCI`（`108-113`）先匹配 `SRD`、再匹配 `SRPD`，命中进 `route`；否则 `ordinary`
  3. `route`（`130-149`）：`go 128 [[0]] rest`；每段 `[v]` → `put 0` 后 `fs acc v`（`142-144`）再递归；步数耗尽有残留 / 段内缺数字 / 括号不配对 → 对应错误
  4. `ordinary`（`151-156`）：空或 `∅` → `[]`；全数字 → 超 `kMaxColumns` 报「输入过大」，否则 `replicate n []`；否则 `columns` 解析父项列表（`158-181`）后 `check`（`73-87`）
  5. `check`（`73-87`）：列数超 `kMaxColumns` → 错误；累计单元数超 `kMaxCells` → 错误；单元 `p < 0` → 错误；`p >= j+1 || p < length col-1 || (row > 0 && col!!(row-1) < p)` → 父列表合法性错误
  6. `fs graph n`（`28-70`）：`n < 0` → 错误；空图 / `n == 0` / 末列空 → `init graph`（空图得 `[]`）；`span_ = length graph - last (last graph)`（`34`）；`length graph - 1 + n*span_ > kMaxColumns` → 错误；`go` 逐 block：`budget`（`17-22`）计数；取末列 `column` 与 `parent`；`copies = moveCol parent span_ (result!!s)`（`47-49`）；`seam = init column ++ drop (length column - 1) pc`（`51`）；单元数超 `kMaxCells` → 错误；结果 `init result ++ [seam] ++ copies`（`60`）
  7. `moveCol`（`65-70`）：`p >= parent` 的值 `+span_`；长度超过 `parent` 时插入 `span_` 个平移后父值
  8. `plain`（`89-96`）：空 → `0`；每列 `[逗号连接]` 拼接（`joinComma` `98-101`）
- **分叉**：本族唯一父项列表列图；不使用三元边、不建共享树；seam 拼接规则与 RPD/ARD 的 `predecessor` 不同
- **错误**（自有 `Left`，逐字）：
  - `计算预算不足`（`Srd.hs:21`，`budget` 超 `kMaxCells`）
  - `需要非负整数`（`Srd.hs:30`，`fs` 的 `n < 0`；`83`，单元 `p < 0`）
  - `展开列数超出预算`（`Srd.hs:36`）
  - `单元数超出预算`（`Srd.hs:56`；`79`）
  - `列数超出预算`（`Srd.hs:75`）
  - `父列表须非增，且满足 高度−1≤父列<子列`（`Srd.hs:85`）
  - `表达式后有未识别内容`（`Srd.hs:134`；`149`）
  - `路由展开格式错误`（`Srd.hs:139`；`145`）
  - `请输入父项列表，如 [][1][2,2]`（`Srd.hs:161`；`163`；`180`）
  - `输入过大`（`Srd.hs:155`）

### `rpd` · RPD（idOrder #116）

- **族 / 模块**：G11242 · `src/OmegaY/Fam/G11242/Graph.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Graph.expandRpd`（`Graph.hs:286`，= `expand RPD`；注册 `rpdNotation` `291`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：`[(k,p,q)]` 三元边列表 / 纯数字空列数 / `A<n>` 路由文本 + `--term` → `(k,p,q)` 三元边列表；空图 `0`
- **term / param 语义**：`term` = 复制轮数（`fs` 的 `n`）；`param` 不参与
- **步骤**：走 §共用 Graph 骨架（见下「Graph 三 id 共用步骤」）；`Kind = RPD`
- **分叉**：RPD 专属规则——
  - 规范化走 `rpd` 支：每条边把 `q = 0..eQ` 展开插入（`Graph.hs:106-117`）；
  - 控制条目 `maximumBy pairCompare`（`150-155`）；
  - `moveE` 只平移 `p`/`q`，`k` 保持（`170-171`）；
  - seam 走 `goSeam`：按 `eK` 比较截断根，`eK == eK control` 时限到 `cutRoot - 1`（`174-185`）；
  - `seed` 底图为 `[[], [Edge k 0 0 | k <- [index..0]]]`（`137`），层号上限 4096（`134`）
- **错误**：见文末「Graph 三 id 错误表」

### `ard` · ARD（idOrder #117）

- **族 / 模块**：G11242 · `src/OmegaY/Fam/G11242/Graph.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Graph.expandArd`（`Graph.hs:287`，= `expand ARD`；注册 `ardNotation` `292`），`TextCaps` 分支
- **输入 → 输出**：`[(k,p,q)]` 边列表 / 纯数字空列数 / `A<n>` 路由 + `--term` → 三元边列表；空图 `0`
- **term / param 语义**：`term` = 复制轮数；`param` 不参与
- **步骤**：走 §共用 Graph 骨架；`Kind = ARD`
- **分叉**：ARD 专属规则——
  - 合法性判定 `bad = q > p || k > p`（`Graph.hs:92`）；
  - 规范化走 `other` 支：`checkEdge` 后 `skyline` 剪枝（`120-124`）；
  - 控制条目取 `last last_`（`156`）；
  - `moveE` 平移 `k`/`p`/`q` 三者（`172`）；
  - seam = `init last_` 平移结果 + `predecessor kind shiftedControl (x + v*width)`（`186-189`）；
  - `predecessor` 减层时根标取 `eP e`（`129`）；`seed` 的 `v j = j - 1`（`139`）
- **错误**：见文末「Graph 三 id 错误表」

### `ard2` · ARD2（idOrder #118）

- **族 / 模块**：G11242 · `src/OmegaY/Fam/G11242/Graph.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Graph.expandArd2`（`Graph.hs:288`，= `expand ARD2`；注册 `ard2Notation` `293`），`TextCaps` 分支
- **输入 → 输出**：`[(k,p,q)]` 边列表 / 纯数字空列数 / `A<n>` 路由 + `--term` → 三元边列表；空图 `0`
- **term / param 语义**：`term` = 复制轮数；`param` 不参与
- **步骤**：走 §共用 Graph 骨架；`Kind = ARD2`
- **分叉**：与 ARD 的差别只在两处——
  - 合法性判定 `bad = k > j || q > j`（按当前列号 `j`，`Graph.hs:91`）；
  - `predecessor` 减层时根标取 `child`（`Graph.hs:129`）；
  - 其余（`skyline` 规范化、末元素控制、三标同移、`seed` 的 `v j = j`，`139-140`）同 ARD
- **错误**：见文末「Graph 三 id 错误表」

### Graph 三 id 共用步骤（`Graph.hs`）

1. **入口**：`expand kind text term`（`279-283`）：`runM (parse kind text >>= \g -> put (0,0) >> fs kind g term) (0,0)`；`Left e` → `e`，`Right g` → `plain g`。
2. **解析**（`parse`，`205-271`）：去空白；首字母 `a`/`A` 读数字进 `route`（`213-216`）；否则 `ordinary`。`route`：`seed kind v`（`132-140`）起底，`go 128` 每段 `[v]` → `put (0,0)`、`fs kind g v`（`230-232`）递归。`ordinary`：空 / `∅` → `[]`；全数字超 `kWidth = 8192` → 错误，否则 `normalize` 空列；否则 `columns`/`parseCol`（`247-271`）解析 `(k,p,q)` 边后 `normalize`。
3. **规范化**（`normalize`，`95-124`）：列数超 `kWidth` → 错误；RPD 走 `rpd`（见 `rpd` 分叉），ARD/ARD2 走 `other`：`checkEdge` 每条边后 `skyline`（`69-80`）按父列取 `pairCompare` 最大、再按 `p` 降序剪枝；单元数超 `kCells = 250000` → 错误。
4. **`fs` 展开**（`142-192`）：`n < 0` → 错误；`x = length graph - 1`，`x < 0` → `[]`；`n == 0` 或末列空 → 截末列；定控制条目与 `c = eP control`、`width = x - c`；`x + n*width > kWidth` → 错误；`go v`：`tick`；`phi block i = if i < c then i else i + block*width`（`167`）；`moveE` 平移；seam 构造；`fromC` 复制 `graph!!c`、`restCols` 复制 `c+1..x-1` 列（`190-191`）；`v >= n` → `normalize kind result`。
5. **`predecessor`**（`126-130`）：`eQ /= 0` → `Edge k p (q-1)`；否则 `eK /= 0` → `Edge (k-1) p (ARD2 ? child : eP e)`；都 0 → `[]`。
6. **序列化**（`plain`，`194-203`）：空 → `0`；边渲染 `(k,p,q)`，列 `[...]` 逗号连接。

### Graph 三 id 错误表（自有 `Left`，逐字）

- `计算预算不足`（`Graph.hs:41` `tick` 超 `kCells*4`；`49` `moveTick` 超 `kCells`；`108` RPD `eQ >= kCells`；`116` RPD 单元计数；`124` ARD/ARD2 剪枝后单元数）
- `需要非负整数`（`Graph.hs:85` 边标 `< 0`；`144` `fs` 的 `n < 0`）
- `父列、层标或根标超出本记号允许的范围`（`Graph.hs:93`）
- `列数超出预算`（`Graph.hs:97`）
- `输入过大`（`Graph.hs:135` `seed` 层号越界；`240` 纯数字列数超 `kWidth`）
- `展开列数超出预算`（`Graph.hs:159`）
- `表达式后有未识别内容`（`Graph.hs:224`；`234`）
- `路由展开格式错误`（`Graph.hs:233`）
- `请输入 [(层,父列,根)] 列表`（`Graph.hs:249`；`252`；`256`；`262`；`264`；`266`）

### `ipd` · IPD（idOrder #119）

- **族 / 模块**：G11242 · `src/OmegaY/Fam/G11242/Ipd.hs` · 文本型 `TextCaps`，后缀 `""`
- **入口**：`Ipd.expandIpd`（`Ipd.hs:597-601`；注册 `ipdNotation` `603-604`），由 `expandNotation` 的 `TextCaps` 分支调用
- **输入 → 输出**：别名 `A<n>`/`T<n>`（可带 `[index]` 连续展开）、纯数字空列数、或 `[p:d/t;…]` 完整列列表 + `--term` → 节点文本拼接（`gKey`）；空图 `0`
- **term / param 语义**：`term` = 展开轮次（`expandGraph` 的 `index`），`< 0` 兜底为 0（`Ipd.hs:599`）；`param` 不参与
- **步骤**：
  1. `expandIpd`：`runM (parseIpd text >>= \g -> expandGraph g (if term < 0 then 0 else term)) emptyCore`（`599`）；`Left e` → `e`，`Right g` → `gKey g`
  2. `parseIpd`（`456-476`）：`tickC`；去空白；文本超 1000000 字符 → 错误；`isAlias`（`478-482`，首字符 ∈ `"at"`、长度 > 1、次位数字）→ `integerAt` 读层号 `seedGraph v`（`394-397`）后 `expansionLoop`；首字符数字 → `makeGraph (replicate v [])`；否则 `parseColumns`（`498-510`）解析边，空列列表 → 错误；`finish`（`484-485`）残留字符报错
  3. `expansionLoop`（`487-496`）：别名遇 `[`：段数 > 256 → 错误；`integerAt` 读层号、`takeAt ']'`、`expandGraph g v`，递归
  4. `parseEdges`（`512-521`）：`p:d/t`；`p >= j` → 错误；`termAt`（`542-577`）解析项：`d == 0` 支持 `*`（SELF）或 ROOT 引用；`d > 0` 支持 `.`、`^`、`{}`、`()` 子节点；`make`（`102-121`）hash-cons 入共享树
  5. `parseChildren`（`579-595`）：`~n` 重复次数；`r == 0` → 错误；累计元数超 `kArity = 8192` → 错误
  6. `expandGraph g index`（`399-414`）：空图 → `g`；`index == 0` / 末列空 → `makeGraph (init (gCols g))`；`controller`（`297-303`，`edgeOrder` `291-295`、`profile` `191-194`）取控制边；`Nothing` → `g`；`cut = edP control`，`width = x - cut`，`total = x + index*width` 超 `kColumns = 4096` → 错误；`loop`（`416-428`）`block = 0..index`：清 `coMemo`，`phi k = if k < cut then k else k + block*width`；`foldCols`（`429-432`）复制 `cut..x-1` 列；末块外 `foldLast`（`433-444`）对 `edP e == cut` 的边走 `lowerEdge`（`264-273`）求前驱
  7. 前驱与比较：`lower`（`204-262`）共享树前驱搜索，`rounds`（`248-262`）按 `n` 轮取头部 / 低位候选最大值；`cmp`（`130-189`）带缓存比较节点；`blockVisit`（`342-364`）平移访问节点并重建条目
  8. `makeGraph`（`366-384`）：`normalize`（`275-289`）按父列 `p` 保留 `profile` 最大者；边数超 `kEdges = 100000`、文本超 1000000 字符 → 错误；`columnText` 经 `termText`（`305-340`）渲染（`*`/`.`/`^`/`{}`/`~n` 片段），拼成 `gKey`
- **分叉**：本族唯一共享树 / hash-cons 实现；状态 `Core` 含节点表、索引、比较缓存、前驱缓存、文本缓存、记忆化表与工作/深度/槽位计数（`48-58`）；不复用 RPD/ARD 的三元边 list 与 `skyline`
- **错误**（自有 `Left`，逐字）：
  - `计算预算不足`（`Ipd.hs:81`，`tickC` 超 `kMaxWork = 1500000`）
  - `递归深度超出预算`（`Ipd.hs:89`，超 `kDepth = 160`）
  - `节点元数超出预算`（`Ipd.hs:105`；`590`，超 `kArity = 8192`）
  - `共享树容量超出预算`（`Ipd.hs:112`，超 `kNodes = 60000` / `kSlots = 800000`）
  - `共享树规范化内部错误`（`Ipd.hs:173`）
  - `列数超出预算`（`Ipd.hs:368`；`500`，超 `kColumns = 4096`）
  - `边数超出预算`（`Ipd.hs:380`，超 `kEdges = 100000`）
  - `表达式文本过长`（`Ipd.hs:383`，超 1000000 字符）
  - `层号超出预算`（`Ipd.hs:396`，超 `kLevel = 1000000`）
  - `展开列数超出预算`（`Ipd.hs:412`）
  - `输入文本过长`（`Ipd.hs:461`）
  - `请输入 A0、A1[3] 或完整列列表`（`Ipd.hs:474`）
  - `表达式后有未识别内容`（`Ipd.hs:485`）
  - `展开路径过长`（`Ipd.hs:490`，超 256 段）
  - `父列必须严格早于本列`（`Ipd.hs:515`）
  - `需要非负整数`（`Ipd.hs:528`）
  - `整数过长`（`Ipd.hs:529`，超 16 位）
  - `整数范围超出预算`（`Ipd.hs:532`）
  - `格式错误`（`Ipd.hs:539`）
  - `引用必须是父列以前的 ROOT 或当前 SELF`（`Ipd.hs:552`）
  - `重复次数不能为零`（`Ipd.hs:588`）

## 本族的「共用骨架 + 独有分支」判定

- **共用 State 骨架**：5 条都走 `M s a` 单子 + `runM`，`Left` 即输出；但预算状态类型三套分立（`Int` 计数 / `Budget` 二元组 / `Core` 共享树），互不复用。
- **共用算法骨架 + Kind 分叉**：`rpd` / `ard` / `ard2` 同一份 `parse`/`normalize`/`fs`/`predecessor`/`plain`，分叉只在 `Kind` 驱动的合法性判定、规范化支、控制条目选择、移动规则与前驱根标。
- **独有实现**：`srd`（父项列表列图 + `moveCol` seam）与 `ipd`（共享树 + `lower`/`cmp`/`blockVisit`）各自独立，与 Graph 三 id 不共享内核函数。
