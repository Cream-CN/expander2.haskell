# Den（3 个 id：`den1` `blp` `iblp`）

**一句话导读**：这族在「带标记条目的行列表」上做坏行复制与 comp 展开——沿父链 `seqFrom` 收集条目、`copy` 迁移标记、`compTo` / `compFrom` 造新行；三 id 全是 `TextCaps` 且经 `textAlterNotation` 注册，活入口是第二参数。算法骨架与 `docs/diagrams/04-kernel-flow/` 第五链（基础序列系）的「取坏部 + 复制 + 回写」同构，但数据结构是行/条目表而非整数序列；`iblp` 是 `blp` 的逆运算（注册标签含「逆」，`src/OmegaY/Fam/Den/Family.hs:16`）。

## 共用骨架

1. **不校验直接展开**：三个 id 全是 `TextCaps`，`expandNotation` 不做区间/空文本校验，直接 `expand text term ++ suffix`（`src/OmegaY/Notation/Types.hs:64`）；入口统一 `term' = max 1 term`。
2. **文本解析**：共用 `src/OmegaY/Fam/Den/Share.hs` 原语（`skipSpaces` `peek` `parseDigits` `atV` `atE` `pleasantUntil` `apValue` 等）；`parse` 失败返回 `Nothing`，各入口坍缩成自己的解析错误串；解析成功得到行列表表达式。
3. **极限 / 短路**：空行表或坏根失效（步长越界、步长值为 0、非极限）→ `cut` 砍末行后直接 `toString`。
4. **展开内核**：`pleasantUntil`（`Share.hs:104`）找第一个「不愉快」行；`flag >= 0` → `copy flag` 复制该段；否则按 `term` 次 `copy (-1)`，再按布尔支 `cut` 或 `copy 1`；随后逐行 `compTo` 收集标记条目，满足短行条件时 `buildT` 取向量、`compFrom` 造新行。
5. **序列化**：`toString` 把行拼回 `(条目…)步长` 串，三 id 后缀全为 `""`。

**term 语义**：`term` = 非极限支下的复制次数（`copy (-1)` 迭代轮数），CLI 缺省 0 被 `max 1` 兜底为 1；`param` 不参与（`TextCaps` 分支不看 `nParam`）。

### `den1` · DEN1

- **族 / 模块**：Den · `src/OmegaY/Fam/Den/Den1.hs` · 文本型 `TextCaps`，后缀 `""`；注册方式 `textAlterNotation`（`Family.hs:12-13`），活入口是第二参数
- **入口**：`Den1.expandTextAlter`（`Den1.hs:323`），由 `expandNotation` 的 `TextCaps` 分支调用；`Den1.expandText`（`Den1.hs:320`）**未接线**（`Types.hs:45-46` 丢弃 `_expand`）
- **输入 → 输出**：`(行值…)步长` 多行，尾部 `;父标号列表`；输出 `(…)步长` 串再接 `;父串`（`toString` `Den1.hs:23`）
- **term / param 语义**：`term' = max 1 term`（`Den1.hs:313`）；`param` 不参与
- **步骤**：
  1. `parse text`（`Den1.hs:314`；`parse` `Den1.hs:66`，`parseRow` `Den1.hs:42`，`parseParents` `Den1.hs:87`）；`Nothing` → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Den1.hs:315`）
  2. 行数 `<= 1` → `toString (Den1Expr [[]])`（`Den1.hs:317`）
  3. `expand e term' longer`（`Den1.hs:318`；`expand` `Den1.hs:272`，活支 `longer = True`）：空行表 / 坏根失效 → `cut`（`Den1.hs:98`）；`pleasantUntil` 得 `flag`（`Den1.hs:286`）；`flag >= 0` → `copy flag`（`Den1.hs:137`）；否则 `extend` 迭代 `fsTerm` 次（`Den1.hs:158`，折叠见 `Den1.hs:292`），`longer` → `copy e 1`，否则 `cut e`（`Den1.hs:293`）；复制区逐行 `fullcomp`（`Den1.hs:253` → `comp` `Den1.hs:178`，批处理 `Den1.hs:296-304`）
  4. `toString`（`Den1.hs:23`）
- **分叉**：三族里唯一带 `;` 父标号首行的结构；comp 走 `comp` 单行展开 + `fullcomp` 回溯 `build`（`Den1.hs:262`）；`expandText`（`longer = False`）是注册表里没接上的残枝
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号，分号`（`Den1.hs:315`）；无其它自有错误；仅 `TextCaps` 不校验（§00 通用）

### `blp` · blp

- **族 / 模块**：Den · `src/OmegaY/Fam/Den/Blp.hs` · 文本型 `TextCaps`，后缀 `""`；注册方式 `textAlterNotation`（`Family.hs:14-15`），活入口是第二参数
- **入口**：`Blp.expandTextAlter`（`Blp.hs:386`），由 `expandNotation` 的 `TextCaps` 分支调用；`Blp.expandText`（`Blp.hs:383`）**未接线**
- **输入 → 输出**：`(带 * 标记条目, …)步长` 多行表达式 → 同形态串（`toString` `Blp.hs:23`）
- **term / param 语义**：`term' = max 1 term`（`Blp.hs:375`）；`param` 不参与
- **步骤**：
  1. `parse`（`Blp.hs:376`；`parse` `Blp.hs:34`，`parseRow` `Blp.hs:53`，`parseEntries` `Blp.hs:61`）；`Nothing` → `解析失败，请确认你没有使用全角括号，逗号`（`Blp.hs:377`）
  2. 空行表 → `toString (DenExpr [])`（`Blp.hs:379`）；`not isLimit`（`Blp.hs:354`）→ `toString (cut e)`（`Blp.hs:380`；`cut` `Blp.hs:72`）
  3. `expand e term' shorter`（`Blp.hs:381`；`expand` `Blp.hs:274`，活支 `shorter = False`）：坏根失效 → `cut`（`Blp.hs:281`）；`pleasantUntil` 得 `flag`（`Blp.hs:289`）；`flag >= 0` → `copy flag`（`Blp.hs:118`，含 `migrate` 标记迁移 `Blp.hs:146` 与 `seqFrom` `Blp.hs:83`）；否则 `copy (-1)` 迭代 `index` 次（`Blp.hs:295`），`shorter` → `cut e`，否则 `copy e 1`（`Blp.hs:296`）；逐行 `compTo`（`Blp.hs:186`），满足短行条件再 `buildT`（`Blp.hs:332`）→ `compFrom`（`Blp.hs:217`），`already` 记录 t 向量（`Blp.hs:297-329`）
  4. `toString`（`Blp.hs:23`）
- **分叉**：行结构 `DenRow { drStep, drEntries }`，步长是独立字段；`seqFrom` 返回链列表（非 `Maybe`）；标记迁移在 `copy` 内按 `seqFrom` 判定；`expandText`（`shorter = True`）是残枝
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Blp.hs:377`，不含「分号」）；无其它自有错误

### `iblp` · iblp

- **族 / 模块**：Den · `src/OmegaY/Fam/Den/Iblp.hs` · 文本型 `TextCaps`，后缀 `""`；注册方式 `textAlterNotation`（`Family.hs:16-17`），活入口是第二参数
- **入口**：`Iblp.expandTextAlter`（`Iblp.hs:377`），由 `expandNotation` 的 `TextCaps` 分支调用；`Iblp.expandText`（`Iblp.hs:374`）**未接线**
- **输入 → 输出**：同 `blp` 形态（表头条目即步长）→ 同形态串（`toString` `Iblp.hs:20`）
- **term / param 语义**：`term' = max 1 term`（`Iblp.hs:367`）；`param` 不参与
- **逆运算关系（以源码为准）**：`iblp` 是 `blp` 的逆运算，结构上逐条镜像 `blp` 的行 / `seqFrom` / `applyRow` / `copy` / `compTo` / `compFrom`，实现差异：① 每行步长收进表头条目（`Den3Row = [DenEntry]`，表头即 step，`Iblp.hs:11`）；② `seqFrom` 改返回 `Maybe`，无定义时给 `Nothing`（`Iblp.hs:77`）；③ 解析括号内允许为空（`Iblp.hs:54`）；④ `copy` 的 `migrate` 在新表达式上重跑 `seqFrom`，按 `activeMarked` 判定是否回迁标记（`Iblp.hs:152-193`）；⑤ 展开主循环 `compTo` / `compFrom` 之后行下标按 `|t|` 前进（`Iblp.hs:343`）；⑥ `longer` 支最后按记录长度 `len0` 回切（`Iblp.hs:346-348`）
- **步骤**：
  1. `parse`（`Iblp.hs:368`；`parse` `Iblp.hs:31`，`parseEntries` `Iblp.hs:53`）；`Nothing` → `解析失败，请确认你没有使用全角括号，逗号`（`Iblp.hs:369`）
  2. 空行表 → `toString (Den3Expr [])`（`Iblp.hs:371`）
  3. `expand e term' longer`（`Iblp.hs:372`；`expand` `Iblp.hs:287`，活支 `longer = True`）：坏根失效 → `cut`（`Iblp.hs:294`）；`pleasantUntil` 得 `flag`（`Iblp.hs:302`）；`flag >= 0` → `copy flag`（`Iblp.hs:124`）；否则 `copy (-1)` 迭代 `fsTerm` 次（`Iblp.hs:308`），`longer` → `copy e0 1` 并记 `len0`（`Iblp.hs:310`），否则 `cut e0`（`Iblp.hs:311`）；`loop` 逐行 `compTo`（`Iblp.hs:195`）→ `buildT`（`Iblp.hs:351`）→ `compFrom`（`Iblp.hs:230`），`r += |t|`（`Iblp.hs:343`）；`longer` 且 `len0 >= 0` → `take len0` 回切（`Iblp.hs:347`）
  4. `toString`（`Iblp.hs:20`）
- **分叉**：逆运算支；`seqFrom` 可空、允许空括号、循环步长与末尾回切为 `blp` 所无；`expandText`（`longer = False`）是残枝
- **错误**：解析失败 → `解析失败，请确认你没有使用全角括号，逗号`（`Iblp.hs:369`）；无其它自有错误
