# Yseq（2 个 id：`epsilon-y` `g-y`）

**一句话导读**：这两个记号把整数序列建成自包含的 STRef entry 山（parent/head/foot/ref 链），画山后取坏根 `b` 与顶 `t`，按 `term` 轮次复制列并回吐各列顶值；它们与 `Fam.Mrss.OmegaYSeq`（ω-Y sequence）同属
`docs/diagrams/04-kernel-flow/` 的**第二链（ω-Y / MrSS 系）**——节点池 → 画山 → 参考/弱强收集 → 复制边列 → `toSequence`；区别是本族不经过 `EntryGraph`，节点池由各自模块内的 `EEnt` / `GEnt` STRef 实现。

**注册表归属说明（已核实）**：`src/OmegaY/Fam/Mrss/EpsilonY.hs` 与 `src/OmegaY/Fam/Mrss/GY.hs` 里也有 `"epsilon-y"` / `"g-y"` 的字面 `seqNotation` 定义，但那两份是恒等残枝（`expandEpsilonY seq _term = seq`、`expandGY seq _term = seq`），且未被 `Fam.Mrss.Family` 导入；`Registry.notationPool` 装配顺序是 Mrss 在前、Yseq 在后（`src/OmegaY/Registry.hs:45`、`Registry.hs:56`），`poolMap = M.fromList` 后写覆盖先写，注册表实际生效的是 `Fam.Yseq` 这两份。

## 共用骨架

1. `expandNotation`（`src/OmegaY/Notation/Types.hs:61`）的 `SeqCaps` 分支先拦 `term < 1`（`Types.hs:75`）、`parseSequence`（`src/OmegaY/Parse.hs:16`）切序列、空则拦下（`Types.hs:79`）。两个 id 全是 `SeqCaps`。
2. 入口守门：`null seq || term < 1` → 原样回显；否则 `runST` 进入模块内节点池。
3. 序列 → 节点池：各自模块的 `toSequence`（`EpsilonY.hs:220` / `GY.hs:151`），parent 指向左侧最近较小元素。
4. 展开内核：`drawMountain` 画山 → 选 `t`（末列顶值为 1 时取其 head）→ `b = gP t` → `setElementRefrence` 定 ref、`setElementNo` 编号 → 跨维/脚部分支 → 末列减一 → 复制 b 列 → 按轮复制列 → 输出各列顶值。两份模块各写一份内核，只在山结构与字段集上有差异。

### `epsilon-y` · ε-Y（idOrder 第 24 位）

- **族 / 模块**：Yseq · `src/OmegaY/Fam/Yseq/EpsilonY.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandEpsilonY`（`EpsilonY.hs:733`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 展开轮次，原样传入 `expandImpl`（无 `max 1` 兜底，但 `SeqCaps` 已在 `Types.hs:75` 拦住 `term < 1`）；`param` 不参与
- **步骤**：
  1. 守门 `term < 1 || null seq = seq`（`EpsilonY.hs:734`）
  2. `toSequence`（`EpsilonY.hs:220`）把序列建成 `EEnt` 节点序列
  3. `expandImpl s term [1] False`（`EpsilonY.hs:629`，`d=[1]`、`f=False`）：
     - `null s` → `[]`（`EpsilonY.hs:630-631`）；末值 `<= 1` → 取 init 各值（`EpsilonY.hs:633-635`）
     - `drawMountainImpl`（`EpsilonY.hs:249`）画山；`t` 按末列顶值是否为 1 取 head（`EpsilonY.hs:638-643`）；`b <- gP t`（`EpsilonY.hs:644`）
     - `setElementRefrence`（`EpsilonY.hs:374`）定 `eref`；`setElementNo`（`EpsilonY.hs:420`，`b`）编号
     - 跨维分支：`tVal - bVal > 1 && length (proc d) == 1`（`EpsilonY.hs:652`）→ `getOdsImpl`（`EpsilonY.hs:335`）取外层山，`newD` 由 `divide` / `merge`（`EpsilonY.hs:118` / `133`）算出，递归 `expandImpl`（`EpsilonY.hs:654-659`）；否则看 `tFoot`：有脚则弹末列顶、`sF t Nothing`、parent 上移，`ex = []`（`EpsilonY.hs:661-673`），无脚直接 `ex = []`
     - 末列全部值减一（`EpsilonY.hs:675`）；`copyRange` 从 `bNo` 起把 b 列元素复制进末列（`EpsilonY.hs:686-702`）
     - `loopN` 跑 `n` 轮：`f=False` 分支只对 `tCol` 调 `copyCloumn`（`EpsilonY.hs:593`，调用点 `EpsilonY.hs:726`）；最后输出各列顶值（`EpsilonY.hs:730`）
- **分叉**：与 §共用骨架 共用第 1–3 步。本 id 的 `d=[1]`、`f=False`；`getFootRow`（`EpsilonY.hs:183`）在 `rowStandardization`（`EpsilonY.hs:170`）里递归试算标准化行；`copyElementImpl`（`EpsilonY.hs:459`）按 `f` 选列定位公式（`EpsilonY.hs:488-490`）。字段集只有 v/row/col/idx/no/id/par/head/foot/ref
- **错误**：仅 §00 通用三分支里的 `SeqCaps` 两条（`项数必须为正整数` / `序列不能为空`）；族内无自有 `Left` 或运行时错误

### `g-y` · G-Y（idOrder 第 65 位）

- **族 / 模块**：Yseq · `src/OmegaY/Fam/Yseq/GY.hs` · 序列型 `SeqCaps`，后缀 `""`
- **入口**：`expandGY`（`GY.hs:830`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 展开轮次，原样传入；`param` 不参与
- **步骤**：
  1. 守门 `null seq || term < 1 = seq`（`GY.hs:831`）
  2. `expandImpl seq term [0] True`（`GY.hs:825`，`d=[0]`、`f=True`）：先 `toSequence`（`GY.hs:151`）建 `GEnt` 节点序列，再 `expandElements`（`GY.hs:747`）
  3. `expandElements` 主体：
     - `null s` → `[]`（`GY.hs:748-749`）；末值 `<= 1` → 取 init 各值（`GY.hs:751-753`）
     - `drawMountain`（`GY.hs:180`）画山；`t` 按末列顶值是否为 1 取 head（`GY.hs:756-760`）；`bMay <- gP t`，**`bMay = Nothing` → `error "gy: null b"`**（`GY.hs:762`）
     - `setElementRefrence`（`GY.hs:297`）；`setElementNo`（`GY.hs:317`，`b`）
     - `expandDimensionSequnece`（`GY.hs:371`，源码拼写）：`pass1` / `pass2` 两遍扫列，经 `getTpElement`（`GY.hs:346`）、`getBootIndex`（`GY.hs:359`）记录 `gbs`/`gbr`/`goff`/`goffs`/`gmaxr`/`gex`/`gn` 等参考与偏移字段（`GY.hs:385-525`）
     - `tFoot` 有脚 → 弹末列顶、`sF t Nothing`、parent 上移（`GY.hs:767-778`）；末列值减一（`GY.hs:780`）
     - `copyRange` 从 `bNo` 起把 b 列元素复制进末列，新节点带 `gmaxr`/`gref`（`GY.hs:785-810`）
     - `loopN` 跑 `n` 轮：`loopJ` 从 `bCol+1` 到 `tCol` 逐列调 `copyCloumnImpl`（`GY.hs:707`，调用点 `GY.hs:817`，`ex` 传 `[]`）；输出各列顶值（`GY.hs:822`）
- **分叉**：与 ε-Y 共用山结构大框架；本 id 的 `d=[0]`、`f=True`（复制循环扫 `bCol+1..tCol` 整段列，不只 `tCol`）。独有内容：`getFootRow`（`GY.hs:260`）内部反复调 `expandImpl` 试算候选行；`expandDimensionSequnece` 的两遍参考/偏移计算；节点比 ε-Y 多 `bs`/`br`/`off`/`offs`/`maxR`/`ex`/`n` 一组字段（`GY.hs:26-36`）。`copyElementImpl`（`GY.hs:540`）的 `maxRow1` 按 `hasMax`/`hasOff`/`brMatch`/`itBs` 多条分支取行（`GY.hs:587-658`）
- **错误**：`SeqCaps` 两条通用错误之外，另有运行时崩溃：`gP t == Nothing` → `error "gy: null b"`（`GY.hs:762`）。这是 `error` 硬崩，不是 `Either Left`，`expandNotation` 不会把它包成 `ERR:` 文本

## 本族的「共用骨架 + 变体分叉」判定

- **共用骨架 + 两份独立实现**：`epsilon-y` 与 `g-y` 走同一段「守门 → `toSequence` → 画山选 t/b → 定 ref/编号 → 跨维或脚部分支 → 末列减一 → 复制 b 列 → 按轮复制列 → 取顶值」流程，但 `EpsilonY.hs` 与 `GY.hs` 各写一套节点类型与内核，源码里没有共享的展开函数。
- **参数差异（闭包传入，非注册表参数）**：`d`（ε-Y `[1]`，g-y `[0]`）与 `f`（ε-Y `False`，g-y `True`）；`f` 直接决定复制循环是只补 `tCol` 还是扫 `bCol+1..tCol`（`EpsilonY.hs:707-728` / `GY.hs:813-819`）。
- **g-y 独有**：两遍 `expandDimensionSequnece` 参考/偏移记录、`getTpElement`/`getBootIndex` 参与定列、`copyElementImpl` 的多分支 `maxRow1`、`error "gy: null b"` 崩溃点；ε-Y 独有的是 `rowStandardization` 递归标准化行与 `divide`/`merge` 跨维 `newD` 计算。
- 全族共同点：只吃整数序列、只吐整数序列，`param` 一律不参与；两份模块内部都不返回 `Left`（g-y 的 `error` 除外）。
