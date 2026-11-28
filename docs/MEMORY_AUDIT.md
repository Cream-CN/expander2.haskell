# MEMORY_AUDIT —— 全族内存审计汇总

> 口径说明：标「实测」的条目是真正跑过基线/改后探针的；标「静态」的只做了代码结构分析，**没有实测数字**，不许当成结论引用。
> 所有实测都在 `-O0`（用户近期要求）下进行，唯一例外是 `Xy` 族那组早期 `-O1` 数据，已单独注明、不与其它族混排。

## 结论摘要

- `xy-v3` 是本轮样板：arena 从 `IntMap` 改成槽位数组 + 水位归还，节点数 3.00→2.00/输出项，堆分配 −24%（`-O1`，5999 例逐字节一致）。
- 底座 `EntryGraph`（ω-Y 序列/magma 共用）同款病灶已修：`IntMap` → 槽位数组，`pushUpL` 的 `++ [child]` 改成 O(1) 前插；`-O0` 下 Mrss 2784 例、Yseq 1200 例逐字节一致，含 4 例崩溃用例。
- 真正「持有死对象」的 arena 只存在于 6 个文件（见下表）；其余 11 族是**纯持久结构**，浪费形态是列表重建带来的临时分配，属时间问题。
- 剩下最大的两块：`Fam\Xy\Common.hs`（xy-v1/v2 共用同一个 `IntMap` arena，未改）与 `Mrss\OmegaYSeq.hs` 的 `Mtn`（`IntMap [Int]` + `pushCol` 的 O(k²) 追加）。

## 已实测的收益

| 族/模块 | 改动 | 正确性证据 | 内存/结构证据 | 编译口径 |
|---|---|---|---|---|
| Xy（xy-v3） | `IntMap V3` → `STArray` 槽位 + `markArena`/`releaseTo`；删死字段 `vOffset`/`offsets` | `test/corpus/xy3.txt` 857 行 × term 0..6 = **5999 例逐字节一致**（含 1212 例 `ERR` 负下标） | 节点数 4806→3204（@n=400/term3，3.00→2.00 每输出项）；堆分配 96.26MB→73.35MB（20 次重复×2571 例） | `-O1` |
| Core（EntryGraph） | `IntMap EntryG` → 槽位数组；`pushUpL` 前插 + `upLsOf` | Mrss `384+2400` 例、Yseq `1200` 例，`mismatch=0`（Yseq 含 4 例崩溃） | 未测（`+RTS -s` 因机器提交内存不足未跑） | `-O0` |
| Bmlike | `Bhm` 阈值改前向填充；`Cms` 父项缓存整轮复用 | `cases=2880 ok=2880 mismatch=0` | bhm −0.92% / cms +0.90%（噪声内，判为时间收益而非内存收益） | `-O0` |
| Den | 无改动（无可安全落地项） | `cases=2880 ok=2880 mismatch=0` | n=8/16/32 缩放 `maximum residency` 恒 51,616 B，不随输入增长 | `-O0` |

## 全族静态地图

| 族 | 是否存在只增不减的池/可变状态 | 主要浪费形态 | 状态 |
|---|---|---|---|
| Xy | 是（`Common.hs` v1/v2；`XyV3` 已改） | v3：arena 钉死 + `[[Int]]` 矩阵逐列重建；v1/v2：`IntMap` 路径复制 | v3 已修实测；v1/v2 已改 `Common.hs`（槽位数组），**未实测** |
| Mrss（ω-Y） | 是（`EntryGraph` + `Mtn`） | 图已修；`Mtn.pushCol` 每压一条重建整列（O(k²)）+ `IntMap` 插入 | 图已修；`Mtn` 待改 |
| Yseq（ε-Y/G-Y） | 是（走 `EntryGraph` + `Notation.YMagma`） | 同上，`egUpL` 追加已修 | 已随底座修复，待补全量实测 |
| Dbms | 是（`SOmega2`/`SOmegaSquare`/`SOmegaP1` 各 6 处 `IntMap`） | 待逐个确认是否节点池；`TsOmegaDbms` 23 处 `++ [` 属重建 | 静态待查 |
| Bms | 否（纯函数 + 不可变 `Array`/`Map`） | 每格重算全矩阵 `maxVal`；从不读的 `scratch` 数组 | 静态，零改动 |
| Bmlike | 否 | 每格新建 `PCache`/memo（已改两处） | 已改（2 处） |
| Tree | 否 | `parents` 尾插（已改）；`++ [` 共 14 处 | 已改 1 处，全量 `-O0` 差分未跑完 |
| Den | 否（纯持久列表，无 `STRef`） | `setAt` 整表重建（O(n²) 时间） | 静态，零改动 |
| Ton | 否 | `TonCore`/`Variants` 少量追加 | 未查 |
| Mn | 否 | 18 处追加在 `SOmegaPowOmegaMn`、`SAOmega2Mn`、`SOmega2Mn` | 未查 |
| Upmn | 否 | `SmnShare` 10 处追加 | 未查 |
| M1ynss | 否 | `Bt`/`Bt3`/`Bml` 各 12–17 处追加 | 未查 |
| Aw2mn | 否 | `Aw2mnShare` 13 处追加 | 未查 |
| Gms | 否 | `LpmsCore`/`GmsCore` 各 8–10 处追加 | 未查 |

## 三个系统性病灶

1. **`IntMap` 当节点池**（6 个文件）：`Fam\Xy\Common.hs`、`Fam\Xy\XyV3.hs`（已改）、`Fam\Mrss\OmegaYSeq.hs` 的 `Mtn`、`Fam\Dbms\SOmega2.hs` / `SOmegaSquare.hs` / `SOmegaP1.hs`。
   症状一致：每分配/修改一次就 `readSTRef` 整表 + `insert`（O(log n) 路径复制），旧版本随即变垃圾；`findMax` 求 id 时还会把整表拉起来。
   修法一致：连续 id → 可增长槽位数组 + 水位；只有"能证明后续不再读"的临时节点才允许回退水位。

2. **只增不减、没有归还点**：`XyV3` 已加 `markArena`/`releaseTo`（两处：画山前、取 s 后）；`EntryGraph` 目前只做了"去掉路径复制"，**没有归还点**（ω-Y 的图在整轮扩张内都可能有节点被回读，贸然回退会出错，需要逐调用点证明）。

3. **列表追加 `++ [`（473 处 / 59 文件）**：绝大多数是 `xs ++ [x]` 形式的累积，属 **O(n²) 时间 + 每步全量重建的临时分配**，严格说是时间问题不是内存问题。判定规则：若旧列表在下一步就死 → 只算时间账；若旧列表被闭包/`STRef` 钉住 → 才算内存账。本轮只对确认被钉住的做了改动（`Tbm.parents`、`Bhm`、`Cms`）。

## 优先建议

| 优先级 | 目标 | 预期 | 风险 |
|---|---|---|---|
| P0 | `Fam\Xy\Common.hs`：`IntMap E` → 槽位数组（含 `getE`/`modE`/`setE`），并评估 v1/v2 是否有可归还的临时节点 | 与 xy-v3 同款收益（v1/v2 共用此池） | 中：v1/v2 语义差异需逐点核对。**代码已改，未编译未实测** |
| P0 | `Mrss\OmegaYSeq.hs`：`Mtn` 改列槽位数组 + 每列可增长 `STUArray`，`pushCol` O(1) 摊还 | ω-Y strong 长列的 O(k²) 追加消失 | 中高：读点 47 处，需全量差分 |
| P1 | `EntryGraph` 加归还点：在 `drawMountain`/`createEntry` 的临时子图边界打 `markArena`/`releaseTo` | 图节点常驻量下降 | 高：需证明无回读，逐点论证 |
| P2 | `Dbms` 三个 `SΩ` 模块的 `IntMap` 确认与改造 | 视确认结果 | 中 |
| P3 | 各族的 `++ [` 列表重建（时间问题） | 时间/临时分配，不计内存账 | 低 |

## 未完成与机器限制

- `Tree` 的 `-O0` 全量差分（5 id × 208 行 × 2 模式 × term 0..3 = 8320 例）跑到一半被本机提交内存耗尽打断，**尚无最终数字**；已修的只有 `Tbm.parents` 一处。
- `Yseq` 的 `base_seq.txt` 全量差分同样未跑完（`mrss_seq.txt` 那 1200 例已完成）。
- `EntryGraph` 的 `+RTS -s` 内存数字未取。
- 本机规格：物理内存 7.9GB，提交上限约 19.5GB；并发编译/差分超过 ~4 个进程就会 `WinError 1455`。后续必须单并发或双并发。

## 非法输入说明

*`test/corpus/xy3.txt` 的 5999 例中有 1212 例会以 `Prelude.!!: negative index` 崩溃，这些输入本身不合法，按用户要求**保持原样、不修**，也不加任何防御性下标或 clamp。*
*`Tree` 语料的 96 例 `Prelude.!!: index too large` 同理：改动前后异常类型、消息、回溯位置逐字一致。*
*`Yseq` 的 4 例崩溃同样逐字一致（`mismatch=0` 的统计里已包含它们）。*
