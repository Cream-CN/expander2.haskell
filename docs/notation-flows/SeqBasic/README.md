# SeqBasic（9 个 id：`empty` `prss` `pps` `pps4` `wpps4` `tpps4` `ewpps4` `spps4` `2-pps4`）

**一句话导读**：这族算的是最原始的「数列展开」——把序列末项当坏根，砍出坏部、按 `term` 次复制后整体接回，
输出仍是逗号分隔的整数序列（不碰矩阵、不碰序数）；它是 `docs/diagrams/04-kernel-flow/` 里
**第五链（基础序列系）**的全部内容，也是 Bms / Bmlike / Gms 三族矩阵系「取坏列 + 复制坏部」那一步的
零行版本（矩阵系在这套动作之外还多了列标准化与行补零）。

## 共用骨架

1. `expandNotation`（`Types.hs:61`）的 `SeqCaps` 分支先做三件事：`term < 1` 拦下、`parseSequence`（`Parse.hs:16`）切序列、空则拦下。
   这族九个 id 全是 `SeqCaps`，**没有 `TextCaps` 那条「不校验」的路径**。
2. 族内统一的短路顺序：`null seq` → 原样返回；再看「末项是否为 0」或「末项是否越界」→ 直接砍掉末项。
3. 统一取量：`y = length seq`、`x = last seq`、`n = max 1 n0`、`ll = y - x`、`b = seq !! (x - 1)`。
   `x` 是末项值（被当作坏根的 1-based 列号），`ll` 是坏部长度，`b` 是坏根值。
4. 定「新末项」：各 id 用不同判据替换末项（`weak` 判定 / 找列 `findCol` / 找等值列 / 特判）。
5. 复制回写：把坏部整体按 `ll` 位移重复，`v >= x` 的值随复制次数加 `ll`（PPS 系），或按位置外推（TPPS4）。
6. 输出：`seqToString`（`Parse.hs:65`）后拼后缀，九个 id 的后缀都是 `""`（`empty` 除外，见下）。

### `empty` · 空记号

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `"  (尚在重构中)"`
- **入口**：内联匿名展开 `\seq _ -> seq`（`SeqBasic.hs:34`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 原样回显 + 后缀 `"  (尚在重构中)"`
- **term / param 语义**：`term` **不参与计算**，只在 `Types.hs:75` 被要求 `>= 1`；`param` 不参与且不校验（`SeqCaps` 分支不看 `nParam`）
- **步骤**：
  1. `expandNotation` 判 `term < 1`（`Types.hs:75`）
  2. `parseSequence` 切序列（`Parse.hs:16`），空则 `Types.hs:79` 拦下
  3. 调匿名 `\seq _ -> seq`（`SeqBasic.hs:34`）——恒等，不看 `term`
  4. `seqToString` 序列化（`Parse.hs:65`），拼后缀 `"  (尚在重构中)"`
- **分叉**：与 §共用骨架 只共用第 1、6 步；它是全族唯一不做坏根分析、不复制、不改末项的 id，
  也是唯一带非空后缀的 id（后缀与 `term` 无关，恒加）
- **错误**：仅 §00 通用三分支里的 `SeqCaps` 两条（`项数必须为正整数` / `序列不能为空`）

### `prss` · PrSS

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandPrss`（`SeqBasic.hs:52`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 坏部复制次数 `n = max 1 n0`（`SeqBasic.hs:58`）；`param` 不参与
- **步骤**：
  1. `null seq` → 原样返回（`SeqBasic.hs:54`）
  2. `isSuccessor`（`SeqBasic.hs:38`，`last seq == 0`）→ `init seq`（`SeqBasic.hs:55`）
  3. `prssBadRoot`（`SeqBasic.hs:41`）：取 `ak = last seq`，从 `i = length seq - 2` 往下扫，
     **第一个 `seq !! i < ak` 的 i 就是坏根**；扫到 `i < 0` 仍没有则返回 `-1`（`SeqBasic.hs:44-50`）
  4. `r < 0` → `init seq`（`SeqBasic.hs:56`）
  5. `badLen = length seq - 1 - r`、`prefix = take r seq`、`badPart = take badLen (drop r seq)`（`SeqBasic.hs:59-61`）
  6. `prefix ++ concat (replicate n badPart)`（`SeqBasic.hs:62`）
- **分叉**：与 `pps` 的区别在**第 3 步怎么定坏根**：`pps` 把末项值 `x` 直接当 1-based 列号并做
  `weak` 判定、算出 `newLast` 再补列；`prss` 完全不看末项值大小，只用「比末项小的最后一个位置」切一刀，
  然后**整段原样复制** `n` 份，没有 `newLast`、没有 `mvResize` 回写
- **错误**：仅 `SeqCaps` 两条通用错误

### `pps` · PPS

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandPps1`（`SeqBasic.hs:72`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 坏部复制次数，取 `n = max 1 n0`（`SeqBasic.hs:87`），所以 CLI 的 `--term 0` 等价 1；`param` 不参与
- **步骤**：
  1. `null seq` → 原样返回（`SeqBasic.hs:74`）；`isSuccessor`（`SeqBasic.hs:38`，末项为 0）→ `init seq`（`SeqBasic.hs:75`）
  2. 取 `y / x / n / ll`（`SeqBasic.hs:85-88`）
  3. `x <= 0 || x > y` → 原样返回（`SeqBasic.hs:76`）；`ll <= 0` → 原样返回（`SeqBasic.hs:77`）
  4. `b = seq !! (x - 1)`（`SeqBasic.hs:89`）
  5. `weak = any (\i -> seq !! i == b) [x .. y - 2]`（`SeqBasic.hs:90`）——坏根值在后面还出现过
  6. `newLast = if weak then b else last seq - 1`（`SeqBasic.hs:91`）
  7. `runST`：`mvFromList (init seq)` 起底，`mvPush newLast`（`SeqBasic.hs:79-80`）
  8. `targetLen = max y (y + n * ll - 1)`（`SeqBasic.hs:81`）
  9. `go` 从 `i = x + 1` 迭代到 `targetLen - ll`（`SeqBasic.hs:93`）：取源值 `v`（`i < y` 取 `seq !! (i-1)`；`i == y` 取结果末位；否则取结果 `i-1` 位，越界则停），`newVal = if v >= x then v + ll else v`，写入 `i + ll - 1` 位，必要时 `mvResize`（`SeqBasic.hs:108-113`）
  10. `mvToList` 取回（`SeqBasic.hs:83`）
- **分叉**：与 §共用骨架 共用 1、2、3、6 步；「找列」这一步在 PPS 退化成 `weak` 布尔判定（没有 `findCol`），
  且回写是 `mvResize` + 逐位写（`go`），不是 `applyWrites` 批量写
- **错误**：仅 `SeqCaps` 两条通用错误；函数自身所有异常输入都走「原样返回」

### `pps4` · PPS4

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandPps4Variant PPS4`（`SeqBasic.hs:126`，`variant` 由 `SeqBasic.hs:224` 传入），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 坏部复制次数 `n = max 1 n0`（`SeqBasic.hs:136`）；`param` 不参与
- **步骤**：
  1. `null seq` → 原样（`SeqBasic.hs:127`）；`isSuccessor` → `init seq`（`SeqBasic.hs:128`）
  2. 取 `y / x / n / ll`（`SeqBasic.hs:134-137`）；`x <= 0 || x > y` → 原样（`SeqBasic.hs:129`）；`ll <= 0` → 原样（`SeqBasic.hs:130`）
  3. `b = seq !! (x - 1)`、`weak`（`SeqBasic.hs:138-139`）
  4. `findCol`：`weak` 为真 → `Nothing`（`SeqBasic.hs:141-142`）；否则按 `variant` 选循环——PPS4 走 `loopN`（`SeqBasic.hs:161`），从 `x-2` 向 `b` 倒扫，第一个 `seq !! c <= b` 的列返回 `c + 1`
  5. `(newLast, strongExpand)`：`Just fc` → `(fc, variant == TPPS4)`；`Nothing` → `(b, False)`（`SeqBasic.hs:166-169`）；PPS4 的 `strongExpand` 恒 `False`
  6. 走 `normalResult`（`SeqBasic.hs:193`）：`mvFromList (init seq)` + `mvPush newLast`，`targetLen = max y (y + n * ll - 1)`（`SeqBasic.hs:196`），`goN` 从 `x + 1` 到 `targetLen - ll` 逐列补写（`SeqBasic.hs:200-221`），`mvToList`
- **分叉**：与 §共用骨架 共用 1–3、6 步。`pps4` / `wpps4` / `tpps4` / `ewpps4` 四条**同一份函数体**，
  只差 `variant`：`findCol` 的循环选择（`loopN` / `loopW` / `loopEw`，`SeqBasic.hs:143-164`）；
  另 `tpps4` 因 `variant == TPPS4` 会走 `strongResult`（`SeqBasic.hs:172-190`）按位置外推，
  而不是 `normalResult` 的补列回写——这是四个版本唯一的结构级区别
- **错误**：仅 `SeqCaps` 两条通用错误

### `wpps4` · Weak PPS4

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandPps4Variant WPPS4`（`SeqBasic.hs:225` 传入 `variant`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 坏部复制次数 `n = max 1 n0`（`SeqBasic.hs:136`）；`param` 不参与
- **步骤**：
  1. `null seq` → 原样（`SeqBasic.hs:127`）；`isSuccessor` → `init seq`（`SeqBasic.hs:128`）
  2. 取 `y / x / n / ll`；`x <= 0 || x > y` → 原样（`SeqBasic.hs:129`）；`ll <= 0` → 原样（`SeqBasic.hs:130`）
  3. `b`、`weak`（`SeqBasic.hs:138-139`）；`weak` 为真 → `findCol = Nothing`（`SeqBasic.hs:141-142`）
  4. 否则走 `loopW`（`SeqBasic.hs:155-159`）：从 `x-2` 向 `b` 倒扫，**第一个 `seq !! c == b` 的列**返回 `c + 1`；遇到小值也不提前收工，一路扫到 `b`
  5. `(newLast, strongExpand) = (c + 1, False)`（`SeqBasic.hs:166-169`）
  6. `normalResult`：`mvFromList (init seq)` + `mvPush newLast` + `goN` 逐列补写 + `mvToList`（`SeqBasic.hs:193-221`）
- **分叉**：与 `pps4` 只差 `findCol` 的循环：`loopW` 只认「等值」不认「小于」，命中即止。
  `strongExpand` 恒 `False`（只有 `TPPS4` 才置真），所以永远走 `normalResult`
- **错误**：仅 `SeqCaps` 两条通用错误

### `tpps4` · Third PPS4

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandPps4Variant TPPS4`（`SeqBasic.hs:226` 传入 `variant`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 外推复制次数 `n = max 1 n0`（`SeqBasic.hs:136`）；`param` 不参与
- **步骤**：
  1. `null seq` → 原样；`isSuccessor` → `init seq`（`SeqBasic.hs:127-128`）
  2. 取 `y / x / n / ll`；`x <= 0 || x > y` → 原样；`ll <= 0` → 原样（`SeqBasic.hs:129-130`）
  3. `loopN`（`SeqBasic.hs:161-164`）找列，同 `pps4`
  4. `strongExpand = (variant == TPPS4) = True`（`SeqBasic.hs:168`）
  5. 走 `strongResult`（`SeqBasic.hs:172`）：`totalLen = y + n * ll`；`out0 = take (y - 1) seq ++ [newLast]`
  6. `go` 从 `position = y + 1` 外推到 `totalLen`：`isLastCopy`（`position > y` 且 `(position - y) mod ll == 0`）→ 追加 `newLast + copyNumber * ll`；否则取 `sourcePos = position - ll` 处的值 `v`，追加 `if v >= x then v + ll else v`（`SeqBasic.hs:179-190`）
  7. `take totalLen` 后输出（`SeqBasic.hs:174`）
- **分叉**：与 `pps4` 共用 1–3 步，从第 4 步起改走**按位置外推**的 `strongResult`，
  而 `pps4` 走的是「从 `x+1` 到 `targetLen - ll` 补列」的 `normalResult`——两条路径的终止条件与写入顺序都不同
- **错误**：仅 `SeqCaps` 两条通用错误

### `ewpps4` · Ex. Weak PPS4

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandPps4Variant EWPPS4`（`SeqBasic.hs:227` 传入 `variant`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 坏部复制次数 `n = max 1 n0`；`param` 不参与
- **步骤**：
  1. `null seq` → 原样；`isSuccessor` → `init seq`（`SeqBasic.hs:127-128`）
  2. 取 `y / x / n / ll`；两条原样返回的短路（`SeqBasic.hs:129-130`）
  3. `loopEw`（`SeqBasic.hs:149-154`）：从 `x-2` 向 `b` 倒扫——命中 `seq !! c == b` 返回 `Just (c + 1)`；
    **命中 `seq !! c < b` 立刻收工返回 `Nothing`**；命中大值继续扫
  4. `Nothing` → `newLast = b`、`strongExpand = False`（`SeqBasic.hs:169`）
  5. `normalResult` 补列回写（`SeqBasic.hs:193-221`）
- **分叉**：与 `wpps4` 只差 `loopEw` 的「遇小值即停」——`wpps4` 的 `loopW` 会一路扫到底，
  `ewpps4` 把「小于坏根值」当作否定证据直接终止
- **错误**：仅 `SeqCaps` 两条通用错误

### `spps4` · Second PPS4

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandSecondPps4`（`SeqBasic.hs:231`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 复制份数 `count`，**原样使用、不取 `max 1`**（`SeqBasic.hs:244`）；`param` 不参与
- **步骤**：
  1. `null seq` → 原样（`SeqBasic.hs:233`）；`x == 0` → `init seq`（`SeqBasic.hs:234`，用「末项为 0」代替 `isSuccessor`）
  2. `x > y` → 原样（`SeqBasic.hs:235`）；`ll <= 0` → 原样（`SeqBasic.hs:236`）
  3. `totalLength = y + count * ll - 1`；`<= 0` → 返回 `[]`（`SeqBasic.hs:237`）
  4. `foundLessOrEqual = any (\c -> seq !! (c - 1) <= b) [y - 1, y - 2 .. x + 1]`（`SeqBasic.hs:246`）
  5. `(value, strongExpand)`（`SeqBasic.hs:248-259`）：`foundLessOrEqual` → `(b, False)`；否则在 `[x-1, x-2 .. b+1]` 里找 `seq !! (c-1) == b` 的列，取到 → `(c, True)`，没取到 → `(b, False)`
  6. 生成写表 `writes`（`SeqBasic.hs:261-276`）：前半段原样拷贝 `0..x-1`、`x..y-2`，`y-1` 位写 `value`；再对 `i ∈ [x .. y-1]` 逐份复制（末位 `shifts = count - 1`，其余 `count`），位置 `i + copy * ll`，值按 `(i == y-1 && strongExpand) || baseValue >= x` 决定是否 `+ copy * ll`
  7. `applyWrites totalLength writes`（`SeqBasic.hs:25`）：先折进 `IntMap`（同位置后写覆盖），再按 `0 .. totalLength-1` 取值、缺省补 0
- **分叉**：与 `pps4` **不共用回写机制**：走的是 `applyWrites` 的「整表一次成形」，而不是 `runST` + `mvResize` 逐位写；
  找列判据也换成「先看有没有 `<= b` 的列，再看有没有等值列」的两级
- **错误**：仅 `SeqCaps` 两条通用错误

### `2-pps4` · 2-pps4

- **族 / 模块**：SeqBasic · `src/OmegaY/Notation/SeqBasic.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expand2Pps4`（`SeqBasic.hs:289`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 复制份数 `n`，原样使用（`SeqBasic.hs:304`）；`param` 不参与
- **步骤**：
  1. `null seq` → 原样（`SeqBasic.hs:291`）
  2. `n == 0` → `init seq`（`SeqBasic.hs:292`）；`x == 0` → `init seq`（`SeqBasic.hs:293`）
  3. `x > y` → 原样（`SeqBasic.hs:294`）
  4. 特判 `y == 2 && seq !! 0 == 0 && seq !! 1 == 2` → 直接返回 `[0 .. n]`（`SeqBasic.hs:295`）
  5. `ll <= 0` → 原样（`SeqBasic.hs:296`）；`totalLen = y + n * ll - 1`，`<= 0` → `[]`（`SeqBasic.hs:297-304`）
  6. `equalCount = length [() | col <- [x + 1 .. y - 1], seq !! (col - 1) == b]`（`SeqBasic.hs:306`）
  7. `v`（`SeqBasic.hs:308-319`）：`equalCount >= 2` → `b`；否则在 `[x-1, x-2 .. b+1]` 里找 `seq !! (c-1) <= b` 的列，取到 → `c`，没取到 → `b`
  8. `applyWrites totalLen writes`（`SeqBasic.hs:321-334`）：同 `spps4` 的整表写，只是末位份数 `n - 1`、其余 `n`，且没有 `strongExpand` 这一项
- **分叉**：与 `spps4` 共用「整表写」，但找值判据不同（`equalCount >= 2` 的数数法 vs `foundLessOrEqual` 的存在法），
  且多了一条 `[0, 2]` 的两项特判——这是全族唯一的字面特判
- **错误**：仅 `SeqCaps` 两条通用错误

## 本族的「共用骨架 + 变体分叉」判定

- **纯共用骨架 + 变体分叉**：`pps4` / `wpps4` / `tpps4` / `ewpps4` 四条是同一个 `expandPps4Variant`，
  分叉点只有 `findCol` 的循环选择（`loopN` / `loopW` / `loopEw`）与 `TPPS4` 的 `strongResult`。
- **骨架 + 不同找值规则**：`spps4` 与 `2-pps4` 共用「`applyWrites` 整表写」这一套，找值规则各自独立。
- **独有实现**：`empty`（恒等+后缀）、`prss`（`prssBadRoot` 单点坏根，无补列）、`pps`（`weak` 判定 + `go` 逐位写）。
- 全族共同点：只吃整数序列、只吐整数序列，`param` 一律不参与；`term` 只被当成「复制次数」，唯一的兜底是
  `pps` / `pps4` 系的 `max 1 n0`，`spps4` / `2-pps4` 原样使用（其 `n == 0` / `totalLen <= 0` 分支在 CLI 上不可达，
  因为 `SeqCaps` 已在 `Types.hs:75` 拦住 `term < 1`）。
