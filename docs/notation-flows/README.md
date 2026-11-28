# 逐记号流程手册（119 个 id，一个不漏）

这份手册把 `expander2-hs` 注册表里的 **119 个记号**逐个拆到「函数级步骤」：
每个 id 一节，写清它属于哪族、`Caps` 是文本型还是参数化还是序列型、入口函数在哪一行、
按真实执行顺序的每一步调了哪个函数、`term` / `param` 各自是什么量、以及所有错误分支的逐字文本。

- 通用骨架（CLI → 分派 → 解析 → 输出、错误文本、构造器死代码规则）：[`00-common.md`](00-common.md)
- 单条 id 的字段规格：[`_TEMPLATE.md`](_TEMPLATE.md)
- 覆盖核对表（119 条逐 id 打勾 + 按族汇总）：[`coverage.md`](coverage.md)
- 既有结构图（模块调用 / 主流程 / 五条内核链）：[`../diagrams/README.md`](../diagrams/README.md)
- 六张族流程图目录下另有一份原生 Visio 文件 `diagram.vsdx`（生成器 [`../diagrams/make_vsdx.ps1`](../diagrams/make_vsdx.ps1)）

**一句话结论**：119 个 id 只有三种外壳——`TextCaps`（**94** 个，不做前置校验）、`SeqCaps`（**17** 个，
查 `term ≥ 1` 与空序列）、`ParamCaps`（**8** 个，只查 `param` 区间）；真正的差别全在族实现函数里，
而这些实现函数大量是「一份骨架 + 一个开关/一张配置表」，真正独立的算法只有三十来条。

Caps 的实际分布（由 `coverage.py` 的同款正则从定义处统计）：`SeqCaps` 只出现在
SeqBasic 9 + YMagma 2 + Mrss 1（`omega-y-sequence`）+ Yseq 2 + Xy 3 = 17；
`ParamCaps` 只在 M1ynss 8；其余 14 个族全是 `TextCaps`（含 `mrss121` 这一个例外——它在 Mrss 族里是文本型）。

## 总表：族 → id 数 → 入口函数 → 一句话骨架

| 族 | id 数 | 入口函数（族） | 一句话算法骨架 | 对应内核链 | 族手册（含图） |
|---|---|---|---|---|---|
| SeqBasic | 9 | `expandPrss` / `expandPps1` / `expandPps4Variant` / `expandSecondPps4` / `expand2Pps4`（`empty` 是恒等） | 末项定坏根 → 砍出坏部 → 按 `term` 复制并位移接回 → `seqToString` | 五 基础序列系 | 小族速览 |
| YMagma | 2 | `OmegaYSeq.expandOmegaYMedium` / `expandOmegaYStrong` | `EntryGraph` 建节点池 → 量山取 b/t → 按 `Kind` 收弱/强边 → 逐轮复制 → `toSequence` | 二 ω-Y · MrSS | 小族速览 |
| Mrss | 2 | `Mrss.mrssExpandText` / `OmegaYSeq.expandOmegaYSeq` | `Expr` 树找坏根 → 复制平移坏部；`omega-y-sequence` 走图展开 + `KindSeq` | 二 | 小族速览 |
| Bms | 11 | `BT.bmsExpandTextWith ver` ×10 / `BT.upmsExpandText` | `parseMatrix` → 标准形检查 → `getBadSequence` → `postProcess` → `copyBadSequence` → `matrixToString` | 一 矩阵系 | [Bms](Bms/README.md) |
| Bmlike | 9 | `Bsm/Cms/Blm.expandText`；`Bhm/Dsm/Wmm.expandAlter` | `parseBm` → 选根 → 算 delta / expansion → 补列 → normalize | 一 | 小族速览 |
| Gms | 17 | `GmsCore.runGms Sys Proj`（15 个）/ `LpmsCore.runLpms` | `parseBm` → `fromMatrix` → `fs` 序号比较 → 递归下一轮 → 按投影裁零行 | 一 | [Gms](Gms/README.md) |
| Tree | 5 | `Tbm.expandText` / `Tupms.expandText` / `Btbm` 三支 | 超限多层 `Expr`：解析 → `isLimit` 分流 → 父链阈值复制列 | 一（超限变体） | 小族速览 |
| M1ynss | 8 | `Flat.expand` / `Tm1.expandTm1` / `Bm1.expandAbs｜Rel` / `Bt.expandBt` / `Bt3.expandBt3Alter` / `Btl.expandBtl` | 父表定位根 → 提升向量/阈值 → 复制波 → `render` | 四 序数系（树结构旁支） | [M1ynss](M1ynss/README.md) |
| Den | 3 | `Den1/Blp/Iblp.expandTextAlter` | 解析行表 → 极限短路 → `pleasantUntil` 复制 → `comp` 造行 | 四（行表变体） | 小族速览 |
| Ton | 9 | `TonCore.tonExpandText cfg`（9 张 cfg） | 解析 → `standardQ*` 判定 → `cacheFs` / `tonGenNext` 状态机 → 输出 | 四 | 小族速览 |
| Mn | 11 | `E0Mn.expandImpl` / `OmegaMn` / `PcfOmegaMn` / `TOmegaMn` / `NMn.expandImpl` / S 系三份 | 解析 Mountain → `isLimit` → `fs` 迭代 `rfl` / `extend` → `display` | 四 | [Mn](Mn/README.md) |
| Upmn | 4 | `Up1Mn` / `Up2MnV1a` / `Up2MnV1b.expandAlter` | 解析 + 值域校验 → 末列空砍列 → `computeUp` 定提升位 → 复制列 | 四 | 小族速览 |
| Dbms | 15 | 15 个 `expandAlter` | 解析 `Expr` → 越界/末列空短路 → `mergeCol` → `addCopy` 复制 → `display` | 四 | [Dbms](Dbms/README.md) |
| Aw2mn | 4 | `Aw2mn.expandImpl weak kind` | 解析 + 值域校验 → `subtract1`（V2/V3）→ `stretched`（weak 放行）→ 延展 `n` 轮 | 四 | 小族速览 |
| Yseq | 2 | `expandEpsilonY` / `expandGY` | 建山选 t/b → 跨维或弹脚 → 复制列 → 取各列顶值 | 二 | 小族速览 |
| Xy | 3 | `expandXyV1` / `expandXyV2` / `expandXyV3` | `World` arena 建树 → 画山 → 参考链/记忆化 → 复制列 → 取顶值 | 三 X-Y 系 | 小族速览 |
| G11242 | 5 | `Srd.expandSrd` / `Graph.expandRpd｜Ard｜Ard2` / `Ipd.expandIpd` | `M s a` 单子跑图：取控制边 → 求前驱 → 复制坏部 → 拼 seam | 三/四（图条目） | 小族速览 |
| **合计** | **119** | | | | |

## 三条贯穿全表的结构事实

1. **校验只发生在 `Caps` 分派那一层**（`Types.hs:61-80`）。`SeqCaps` 拦 `term < 1` 与空序列；
   `ParamCaps` 只拦 `param` 越界；`TextCaps` 什么都不拦——所以 94 个文本型 id 在 CLI 上
   收到空表达式、`--term 0` 都不会报错，而是被各自的实现当成「空串」或「1 次」处理。
2. **`textAlterNotation` / `paramAlterNotation` 会丢弃第一个展开函数**（`Types.hs:43-46`、`54-58`）。
   凡是这么注册的 id，手册里的「入口」必须是第二个那支；同文件里的 `expandText` / `expandFs` /
   `expandNFs` / `expandBt3` / `expandText`（Dbms、Upmn）**都没接线**，是 C++ 侧 `FS_alter` 移除后
   留下的残枝。受影响的 id：`bhm` `1/2/3-bm-bhm` `dsm` `wmm`（Bmlike）、`den1` `blp` `iblp`、
   `up1mn` `up2mn-*`、Dbms 全 15 个、`bt3ynss`、`btbw` 之外的 Mn 族 11 个。
3. **实现里的 `term` 兜底各自负责**：`max 1 term` 是几乎所有族的共同写法，因此 CLI 的
   `--term 0` 在文本型族里等价于 1；例外是 `2-pps4`、`spps4` 原样使用 `term`（它们的
   `n == 0` / `totalLen <= 0` 分支在 CLI 上不可达）。

## 共用骨架 vs 参数/版本分叉（谁只是换了个开关，谁真的有独立分支）

**A 类｜同一函数体 + 一个参数/开关/配置表**（改一行注册就换版本）
- `e0mn` / `strong-e0mn`：同一 `E0Mn.expandImpl`，只差 `strong :: Bool`。
- `1-mn` / `2-mn` / `3-mn`：同一 `NMn.expandImpl n`，而 `n` **在函数体里未被读取**——三个 id 行为完全一致。
- `pps4` / `wpps4` / `tpps4` / `ewpps4`：同一 `expandPps4Variant`，只差 `findCol` 的循环（`loopN`/`loopW`/`loopEw`）与 `TPPS4` 的 `strongResult`。
- Ton 全 9 个：同一 `tonExpandText`，只差 `TonConfig` 的四个字段。
- Gms 前 15 个：同一 `runGms`，是 3 个系统 × 5 个投影的笛卡尔积；其中 `ProjOmegaP` / `PQSS` / `QSS` 三者行为无差别，只有 `Full`（裁零行）与 `WeirdlyFull`（行数优先比较）真正不同。
- `lpms` / `lptss`：同一 `runLpms`，`isLptss` **在函数体里未被读取**——两个 id 行为一致。
- `up2mn-v1b` / `up2mn-v1b-plus`：同一 `Up2MnV1b.expandAlter plus`，只差一个 `Bool`。
- Aw2mn 全 4 个：`weak :: Bool` × `SubtractKind`（V2/V3），两个维度各控制一个函数（`stretched` 的放行短路、`subtract1` 的减法分支）。
- Bms 的 10 个版本：外壳与 `bmsExpand` 主流程共用，分叉只在 `getBadSequence` / `postProcess` / `copyBadSequence` 三处按 `verCode` 分派。
- Bmlike 的 `1/2/3-bm-bhm`：同一 `bmBhmExpand`，只差阈值 `top < n` 走 `bm4Expand` 还是 `bhmExpand`。

**B 类｜同一版式骨架 + 各自的重写规则**（结构像，但要逐条读）
- Dbms 的 σ 系列（`s1dbms`→`s2dbms`→`s-omega-dbms`→`-v2`→`-v3`→`s-omega-p1`→`s-omega2`→`s-omega-square`）：复制恒按 `v >= r` 偏移，分叉全集中在「高度类型 + `computeNewH` / `computeNewHeight` / `findLowerHP`」。
- Dbms 的 `up2dbms-v1` / `v1b-plus` / `v1c`：共用 `expandWith` + `dbmsExpand`，只差 `v1flag` / `v1c` 两个开关。
- Mn 的三个 S 变体（`s-omega2-mn` / `sa-omega2-mn` / `s-omega-pow-omega-mn`）：版式同构但各自文件内带副本，差异在条目是否带 `aeMark`、`sFn` 取阈值规则、`stretchDataTop` 回退。
- Yseq 的 `epsilon-y` / `g-y`：同骨架，分叉参数是 `d`（`[1]` vs `[0]`）与 `f`（只补 tCol vs 扫整段）。
- Tree 的 `btbm` / `btbm-weak` / `bbm`：同一 `runNotation` 外壳，`wFs` / `isSpecial` / `bIsSpecial` 各一支。

**C 类｜真正独立的实现（没有共用骨架可套）**
`upms`（独立解析器 + `findBadRoot` + `generateBh`）、`mrss121`（`Expr` 树，不碰 `EntryGraph`）、`omega-mn`、
`pcf-omega-mn`、`t-omega-mn`、`up1dbms`、`up2dbms-v2`、`ts-2-dbms`、`ts-omega-dbms`、`srd`、`ipd`、
`bsm`、`cms`、`dsm`、`blm`、`wmm`、`tbm`、`tupms`、`den1`、`blp`、`iblp`、`xy-v1`/`v2`/`v3`（三份独立 arena）、
`empty`、`prss`、`pps`、`spps4`、`2-pps4`。

## 图

| 图 | 规模 | 内容 |
|---|---|---|
| [Bms/diagram](Bms/diagram.svg) | 38 节点 / 46 边 | 10 个 BMS 版本靠 `getBadSequence` / `postProcess` / `copyBadSequence` 三处按 `verCode` 分派，`upms` 另起一条独立链 |
| [Gms/diagram](Gms/diagram.svg) | 32 节点 / 40 边 | 两级判断：先选 System（GBMS/UPMS/LPMS2），再选 Proj（裁零行 / 行数优先）；`lpms`/`lptss` 走另一条引擎 |
| [Dbms/diagram](Dbms/diagram.svg) | 33 节点 / 48 边 | up 组 / σ 组 / ts 组三条分组，15 个叶子节点各标自己的重写规则 |
| [Mn/diagram](Mn/diagram.svg) | 40 节点 / 59 边 | 五套骨架（Expr / Mountain / Pcf / TExpr / SMountain）并列，各自带解析失败与空/去末列出口 |
| [M1ynss/diagram](M1ynss/diagram.svg) | 22 节点 / 29 边 | 参数区间校验 → 8 个入口 → 递归嵌套判定 → 复制波 |
| [小族速览](00-small-families/diagram.svg) | 22 节点 / 34 边 | 其余 12 族 55 个 id 的骨架速览（按 `Caps` 型分三条lane，标出共享模块 `EntryGraph` / `Share.parseBm` 的接入口） |

每张图都是 `nodes.csv` + `edges.csv` 可再生，`diagram.drawio` 是主件、`diagram.svg` 是正交布线预览；
全部 `validate_csv.py` 输出 `OK`，且经 [`qc_diagrams.py`](qc_diagrams.py) 复核（≤40 节点、label ≤20 字、
判断边带标签、判断出边 ≤3）。

图的画法与既有 `docs/diagrams/` 一致：**不重复画 CLI 段**（那是 `03-main-flow` 的 22 节点），
每张图从「族实现函数被调用」开始；`04-kernel-flow` 的五条链在这里被还原成具体函数名。

## 怎么复查

```powershell
cd D:\expander\expander2.haskell
python docs\notation-flows\coverage.py --write   # 重新生成 coverage.md（119 条逐 id 打勾）
python docs\notation-flows\qc_diagrams.py        # 复核 6 张图的 CSV 与生成物
```

单张图重建（以 Bms 为例）：

```powershell
$skill="C:\Users\Administrator\AppData\Local\Doubao\User Data\Default\.doubao\agent_mode\workspace\.user_skills\visio-csv-diagram\scripts"
python "$skill\validate_csv.py"      docs\notation-flows\Bms
python "$skill\build_visio_source.py" docs\notation-flows\Bms --title "BMS 逐记号流程"
python docs\diagrams\render_preview.py docs\notation-flows\Bms
```

`coverage.py` 的判定方式：id 清单现场从 `src\OmegaY\Registry.hs` 的 `idOrder` 读（不写死 119 行），
族归属从每个 id 的**定义位置**（`seqNotation "id"` / `nId = "id"` 等构造点所在的模块）反推，
再与各族手册里的 ``### `id` `` 小标题对表——缺漏、多余、重复、族归属不符任一非零即失败退出。

## 与旧文档不一致、已按源码更正的三处

1. `docs/diagrams/README.md` 末尾称「`Fam.Yseq` 与 `Fam.Mrss` 的 id 集合有重叠，注册表按 `poolMap` 后者覆盖前者」。
   实际：`Fam/Mrss/EpsilonY.hs`、`Fam/Mrss/GY.hs`（以及 `Fam/Mrss/XyV1..3.hs`）里那几份是**恒等残枝**、
   且**没有被 `Mrss.Family.familyNotations` 导入**，根本没进 `notationPool`。生效的只有 `Fam.Yseq` / `Fam.Xy` 那两份。
2. `omega-y-medium` / `omega-y-strong` 的**接线实现不在 `src/OmegaY/Notation/YMagma.hs`**。
   那两个 id 运行时打到 `Fam/Mrss/OmegaYSeq.hs:604-608` 的 `expandOmegaYMedium` / `expandOmegaYStrong`；
   `Notation/YMagma.hs:443-455` 是未被引用的并行副本（它那份 `[maxBound] → [1,1+t]` 特判运行时不可达）。
   手册 [`YMagma/README.md`](YMagma/README.md) 把副本与接线版的差异并列写明。
3. `docs/CONTRACT.md:73` 称 `OmegaY.Matrix` 是「行优先」，而 `src/OmegaY/Matrix.hs:1` 自己写的是**列主序**。
   读矩阵族（Bms/Gms/Bmlike/Tree）的 `matRows` / `matCols` / `matAt` 时以源码为准。

另外两处源码自身的「看起来像 bug、但照搬」的事实，已在各条目如实记录：
`Mrss.mrssExpandText` 解析失败时返回 `"[]"` 而非报错；Mn 族的错误串是作为**普通返回串**打印的，
不带 `ERR:` 前缀（`br*`/`S*` 的 `errText` 走 `Main.hs:55` 的 `Right` 分支）。
