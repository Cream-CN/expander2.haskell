# expander2.haskell 结构与调用关系图

三张图对应三种视角；每张图的 `nodes.csv` / `edges.csv` 是可再生源，`diagram.drawio` 是 Visio 主件，
`diagram.svg` 是预览。

| 目录 | 内容 | 规模 | 结构特征 |
|---|---|---|---|
| `01-module-calls/` | 模块调用关系：`app/Main.hs` → `Registry` → 17 个记号提供模块 | 19 节点 / 18 边 | 3 层星形装配，无判断、无环 |
| `03-main-flow/` | 主程序执行逻辑链（CLI 一次调用的全过程） | 22 节点 / 21 边 | 7 个判断分支，无环，3 类出口（参数错、id 未注册、Caps 校验） |
| `04-kernel-flow/` | 展开算法内核逻辑链（五类内核各自的算法步骤） | 31 节点 / 30 边 | 1 个判断（矩阵标准形），5 条并行链 |

## 一句话结论

程序是「一个入口 + 一张注册表 + 15 个记号族 + 6 个共享基础模块」的结构：
`Main` 只认 `Registry.byId` 与 `Notation.Types.expandNotation`，具体算法全在 `src/OmegaY/Fam/*` 里；
所有族共用同一套 `Caps` 分派（文本型 / 参数化 / 序列型），展开完再拼后缀、打印。

## 01 模块调用关系

- `app/Main.hs` 是唯一入口，只调用 `Registry.allNotations`（`--list`）与 `Registry.byId`（`--id`）。
- `Registry` 把 119 个 id 的 `Notation` 收进 `poolMap`，按 `idOrder` 排序；`--list` 再按显示名首码位排序。
- 17 个模块向注册表供记号：15 个 `Fam/*/Family.hs` + `Notation/SeqBasic.hs`（empty/PrSS/PPS/PPS4 共 9 个）
  + `Notation/YMagma.hs`（ω-Y medium/strong）。

### id 分布

| 提供者 | id 数 | 代表 id | 核心实现模块 |
|---|---|---|---|
| `Notation.SeqBasic` | 9 | `empty` `prss` `pps` `pps4` `2-pps4` | 同文件（ST + MVec） |
| `Notation.YMagma` | 2 | `omega-y-medium` `omega-y-strong` | 同文件（EntryGraph） |
| `Fam.Bms` | 11 | `bms-v1`…`bms-v4` `upms` | `BmsCore` `BmsText` `UpmsCore` |
| `Fam.Bmlike` | 9 | `bsm` `bhm` `1..3-bm-bhm` `cms` `dsm` `blm` `wmm` | `Share` `Bsm/Bhm/Cms/Dsm/Blm/Wmm` |
| `Fam.Gms` | 17 | `gms-gbms-*` `gms-upms-*` `gms-lpms2-*` `lpms` `lptss` | `Common` `GmsCore` `LpmsCore`（复用 `Bmlike.Share.parseBm`） |
| `Fam.Tree` | 5 | `tbm` `tupms` `btbm` `btbm-weak` `bbm` | `Tbm` `Btbm` `Tupms` |
| `Fam.M1ynss` | 8 | `-1y-(n+1)ss` `bt1/2/3/lynss` `abs/rel-bm1ynss` | `PP` `Tm1` `Bm1` `Bt` `Bt3` `Btl` `Flat`（唯一的参数化族） |
| `Fam.Den` | 3 | `den1` `blp` `iblp` | `Share` `Den1` `Blp` `Iblp` |
| `Fam.Ton` | 9 | `ton-m` `ton-i` `ton-dr*` `ton-ibp` `ton-mc` `ton-mpc` | `TonCore` `Variants` |
| `Fam.Mn` | 11 | `e0mn` `omega-mn` `t-omega-mn` `1..3-mn` `s-omega*` | `MnOrdShare` `SmnShare` `OmegaMn` 等 |
| `Fam.Upmn` | 4 | `up1mn` `up2mn-v1a/v1b/v1b-plus` | `SmnShare` `Up1Mn` `Up2MnV1a/V1b`（复用 `Dbms.Common/MnUp2`） |
| `Fam.Dbms` | 15 | `up1dbms` `up2dbms-*` `s-omega*-dbms` `ts-2/omega-dbms` | `Common` `MnUp2` `SOmega*` `Ts*Dbms` `Up*`（19 个模块） |
| `Fam.Aw2mn` | 4 | `a-omega2-mn-2/3` 及 weak 变体 | `Aw2mn` `Aw2mnShare` |
| `Fam.Mrss` | 2 | `mrss121` `omega-y-sequence` | `Mrss121` `OmegaYSeq`（走 `Mrss` + `EntryGraph`） |
| `Fam.Yseq` | 2 | `epsilon-y` `g-y` | `EpsilonY` `GY` |
| `Fam.Xy` | 3 | `xy-v1` `xy-v2` `xy-v3` | `Common`（World arena）`XyV1/V2/V3` |
| `Fam.G11242` | 5 | `srd` `rpd` `ard` `ard2` `ipd` | `State` `Srd` `Ipd` `Graph` |

## 共享基础模块（谁依赖谁）

| 模块 | 职责 | 被谁依赖 |
|---|---|---|
| `Matrix` | 列主序二维 `Int` 矩阵 | `Parse`、`Bms`、`Bmlike`、`Gms` |
| `Parse` | 序列 / 矩阵文本互转（`parseSequence` `parseMatrix` 等） | `Notation.Types`（序列）、`Bms.BmsText`（矩阵） |
| `MVec` | ST 可变 `Int` 向量（对齐 `std::vector<int>`） | `Notation.SeqBasic` 的 PPS 家族 |
| `Mrss` | `Expr` 树、`mrssParse/Expand` | `Fam.Mrss.Mrss121` |
| `EntryGraph` | 条目池 + y 向量序（`verticalCompare` 等） | `Fam.Mrss.OmegaYSeq`、`Notation.YMagma` |
| `Notation.Types` | `Notation` 记录、`Caps` 三型、`expandNotation` / `uiExpand` | `Main`、`Registry`、15 个族、`SeqBasic`、`YMagma` |

`Dbms.Common/MnUp2` 是族内共享：`Upmn` 直接复用（`Up1Mn`、`Up2MnV1a/V1b`）；
`Gms.Common` 复用 `Bmlike.Share.parseBm`。跨族的复用只有这两处，其余族各自独立。

## 逻辑链要点

主链（`03-main-flow`）：`getArgs` → `collect` 具名参数 → `--help`/`--list` → 读 `--file`/stdin →
`byId` 查表 → `expandNotation n text term param` → 按 `Caps` 三分支校验 → 族内核算法 → 拼后缀 → 打印。
错误一律以 `ERR:<文本>` 或特定的 `ERR_NO_NOTATION` 收尾（`exitFailure`）。

内核链（`04-kernel-flow`）五条并行：

1. 矩阵系（BMS/GMS/BMLIKE）：`parseMatrix` → 标准形检查 → `getBadSequence`（按版本选分支）→
   `copyBadSequence` → `bmsExpand` 迭代 `term` 次 → `matrixToString`。
2. ω-Y / MrSS 系：`EntryGraph` 节点池 → `fromSequence` + `drawMountain` → `findLower/findHigherEqual` →
   `collectWeak/Strong` → `copySingleEdge/fillMagmaEdge` → `sortCol/rewire` → `toSequence`。
3. X-Y 系：`Xy.Common` 的 `World` arena → `toSequenceV12` 建树 → `getReferenceChain` 取主维 →
   `expandImpl` 记忆化展开 → `toSequence`。
4. 序数系（MN/DBMS/TON/AW2MN）：`parseCol` 解析成 `Expr` 列 → `cmpOrd/cmpEntry` → `predecessor` 求前驱 →
   `remapCol/shiftCol` 重写列 → 拼接（`suspend/unsuspend`）。
5. 基础序列系（PrSS/PPS/XY v1）：`parseSequence` → `findBadRoot/difference` → `replaceLast/applyWrites` →
   按 `term` 迭代 → `seqToString`。

## 图怎么改、怎么进 Visio

CSV 可手改，改完重跑两条命令（`docs/diagrams/` 下）：

```sh
# build_visio_source.py 在 visio-csv-diagram/scripts/ 下
python build_visio_source.py 04-kernel-flow --title "展开算法内核逻辑链"
python render_preview.py 04-kernel-flow      # 只重画正交布线的 SVG 预览
```

## 已经生成好的 Visio 文件

九张图都已由本机 Visio 直接生成原生 `.vsdx`（不是 draw.io 转出来的）：

- 结构图：`01-module-calls/diagram.vsdx`、`03-main-flow/diagram.vsdx`、`04-kernel-flow/diagram.vsdx`
- 族流程图：`docs/notation-flows/` 下 `Bms`、`Gms`、`Dbms`、`Mn`、`M1ynss`、`00-small-families` 各自的 `diagram.vsdx`

生成器是同目录的 `make_vsdx.ps1`：读 `nodes.csv`/`edges.csv` → 最长路径分层 + 前驱重心排序 → 从 Visio 基本模具
（`BASIC_M.VSSX`）落原生矩形/圆角矩形/菱形/椭圆/平行四边形 → 自己算正交折线连线（水平进列间通道、通道内竖直、
水平进目标左边）→ 边上标签用白底小文本框 → `SaveAs` vsdx。

```powershell
# 单张重建（PowerShell 7）
& .\make_vsdx.ps1 -Dirs @("04-kernel-flow") -Verify
# 九张全量重建
& .\make_vsdx.ps1 -Dirs @("01-module-calls","03-main-flow","04-kernel-flow",
    "..\notation-flows\Bms","..\notation-flows\Gms","..\notation-flows\Dbms",
    "..\notation-flows\Mn","..\notation-flows\M1ynss","..\notation-flows\00-small-families") -Verify
```

`-Verify` 会重新打开 vsdx 点形状数：`shapes2D` 应等于节点数，`lineSegments` 应等于折线段总数。

进 Visio 后：形状和连线都是原生对象，可改文字/改色/拖动；连线是独立的一维线段（箭头在末段），
不是动态连接符，所以移动方框后需要手动挪线。若想要自动跟随的连接符，可在 Visio 里改用「连接线」工具重连。

只想要图片：`diagram.svg` 用 Visio「文件 → 打开 → SVG」得到矢量图（不可编辑连线）；
`diagram.png` 是同一张图的位图预览。

## 说明与已做的简化

- 三张图都由源码 `import` 关系与函数调用顺序直接抽取，没有臆造模块；族内多版本差异压缩为「核心模块」粒度。
- 流程图把 Haskell 的 `Either`/`IO` 细节压平：`expandNotation` 的 `Left` 统一按「打印 ERR 并结束」表示，
  `putStrLn` 归入各分支的「打印」动作。
- `Fam.Yseq` 与 `Fam.Mrss` 的 id 集合有重叠（`epsilon-y` / `g-y` 两边都定义），注册表按 `poolMap` 后者覆盖前者，
  图上按 `Registry` 实际引用的顺序绘制。
