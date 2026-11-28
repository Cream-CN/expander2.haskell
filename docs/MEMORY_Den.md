# Den 族内存审计报告

## 结论摘要

- 通读 `src\OmegaY\Fam\Den\` 全部 5 个模块（Blp / Iblp / Den1 / Share / Family）及依赖的共享底座（EntryGraph、Matrix、MVec、Xy Common、Notation/*）。Den 族**没有** arena、`IntMap`、`STRef`/`STArray`、`MVec` 或任何只增不减的节点池；全部状态都是纯持久列表（`[[Int]]` / `[DenRow]` / `[DenEntry]`），与 C++ 侧 `std::vector` 一一对应。
- 未发现"已死对象被钉住"：自造缩放输入 n=8 / 16 / 32 三档，三个 id 的 maximum residency 恒定为 51,616 字节，存活堆不随行数增长；`copy` / `compFrom` / `comp` 返回后，旧表达式 `raw` 与中间列表均不可达，可被 GC 回收。
- 本轮**未改动任何 Den 族源码**（5 个模块与 `.bench\base_tree` 副本 MD5 逐一相等）；全量差分 2880 例 `mismatch=0`。
- 现存分配压力来自持久列表"逐行写整表重建"（O(n²) 的 cons 复制），产生的是当轮即释放的临时垃圾，不是被钉住的死对象；根除需要把行列表换成可变数组后端，已列入提案，本轮不动手。
- 风险评估：无代码改动，输出与基线逐字节一致；非法输入行为原样保留。

## 族结构地图

| 模块 | 职责 | 节点 / 临时结构存在哪种容器 |
|---|---|---|
| `Family.hs` | 注册 den1 / blp / iblp 三个 id | 顶层静态 `[Notation]`（CAF，程序级常量，与单次展开无关）；无 per-call 状态 |
| `Share.hs` | `DenEntry{deValue !Int, deMarked !Bool}`、文本解析与共用工具 | 纯列表 `[DenEntry]` / `[Int]`；无可变容器 |
| `Den1.hs` (`Den1Expr{d1Rows :: [[Int]]}`) | den1：扁平行，`row[0]` 为 step，`row[1..]` 为值；首行为父标号列表 | 行列表 `[[Int]]`；`copy`/`extend` 的 `copied`、`parentsNew`；`comp` 的 `batch1`/`batch2`（`[(位置, 行)]` 元组列表）、`placed`、`parentsOld`/`parentsFinal`；`fullcomp` 的 `t`（cons 前插）；`expand` 中供 `pleasantUntil` 用的 `rows`/`tcheck`（瞬态） |
| `Blp.hs` (`DenExpr{deRows :: [DenRow]}`, `DenRow{drStep !Int, drEntries :: [DenEntry]}`, `SeqItem{siRow, siIdx}`) | blp（DEN2）：带标记行序列 | 行列表 `[DenRow]`；`seqFrom` 的链 `rec :: [SeqItem]`；`copy` 的 `copied`、`rows1`/`rows2`、`migrate` 累加 `acc :: [DenEntry]`；`compTo` 的 `newEs` 累加列表；`compFrom` 的 `newRows`/`origRow`/`rows1`/`rows2`/`rows3`；`expand` 的记忆表 `already :: [Maybe [Int]]`；`buildT` 的 `t`（cons 前插） |
| `Iblp.hs` (`Den3Expr{d3Rows :: [Den3Row]}`, `Den3Row = [DenEntry]`，首元素为 step) | iblp（DEN3）：逆序平行实现，结构与 Blp 同构 | 同 Blp：行列表 `[Den3Row]`；`seqFrom` 链 `rec`；`copy`/`compTo`/`compFrom` 各阶段列表；记忆表 `rec :: [Maybe [Int]]`；`buildT` 前插列表 |

依赖关系：Den 三个模块只 import 本族 `Share`；`Share` 只依赖 `Data.Char` / `Data.List` / `Data.Ord`。**未使用** `EntryGraph`（IntMap arena）、`Matrix`、`MVec`、`Xy.Common` 的任何类型；`Notation.Types` 仅在分派层调用 `mrssTruncate`（对输出字符串做一次性解析/截断，瞬态）。

## 发现的内存问题

| 文件:行 | 结构 | 为什么钉住死对象（核查结论） | 可修/不可修 | 预估量级 |
|---|---|---|---|---|
| `Blp.hs:297`、`Iblp.hs:337-341` | 记忆表 `already` / `rec :: [Maybe [Int]]` | 压缩阶段按行下标缓存该行构造的 T 向量，供后续 `compTo` 反查；这是 C++ `Already` / `Rec`（`vector<optional<vector<int>>>`）的等价物，条目在 `expand` 返回前**确实需要存活**，不是死对象。返回后随表达式一起不可达 | 不可修（语义要求；删条目会改变反查结果） | O(行数) 个 T 向量，与 C++ 持平 |
| `Blp.hs:131-140,167-184`、`Iblp.hs:136-145,152-193`、`Den1.hs:147-153,166-172` | `copy` 闭包捕获 `raw` | 逐行核查：返回的新行只含拷贝出的 `DenEntry`/`Int`，闭包内对 `raw` 的引用不进入结果结构；函数返回后 `raw` 不可达。**未钉住** | 安全，无需修 | — |
| `Blp.hs:118-184`、`Iblp.hs:124-193`、`Den1.hs:178-251` | 持久行列表的 `setAt` 逐行整表重建 | 每次写一行都用列表推导重建整个行列表（`[if k==idx then v else x ...]`）；旧表当轮即可回收，不跨阶段保留，但每次复制产生一整批临时 cons。这是 den1 分配超线性的主因，属"大结构整块重建"，但根除需可变数组后端 | 本轮不可修（提案 A）；在纯列表语义内只做局部替换收益有限且风险高 | den1 n=32 约 1.10MB 分配中大头在此 |
| `Blp.hs:94-108`、`Iblp.hs:99-113` | `seqFrom` 递归链 `init rec ++ [x]`、`rec1 ++ [y]` | 每走一步复制整条链；产物 `SeqItem` 链当轮使用后释放。语料中链长 ≤3，影响很小；O(k²) 列表复制 | 可优化（提案 B），本轮未动 | 链长 k，官方语料 k≤3 |
| `Blp.hs:67`、`Iblp.hs:61`、`Den1.hs:62,86,94` | 解析器 `acc ++ [x]` 尾追加 | 一次性解析，累加器解析完即成为结果列表；中间副本当轮释放 | 时间问题（提案 B），内存影响可忽略 | 输入条目数 |
| `Den1.hs:190-223` | `comp` 中 `batch1 ++ batch2` 元组列表与 `placed` 短暂共存 | 两批新行先以 `(位置, 行)` 元组收集，再折叠进 `placed`；`comp` 返回后元组列表释放，不跨阶段保留 | 安全，无需修 | O(新行数) |
| `Blp.hs:349-352`、`Share.hs:150-155`、`Den1.hs:18-21`、`Share.hs:44-47,78-79` | 死代码：`limitRow`、`limitRowEntries`、`toShortRow`、`matchLit`、`rowAtE` | 全 src/app 检索均无调用方；但它们是顶层代码而非堆数据，不产生 per-call 堆分配，也不持有任何表达式结果 | 未删（保持与 C++ 头文件对齐、避免格式扰动）；可在后续清理 | 无堆影响 |
| 全局 | 无 arena / Map / 全局缓存 | 未发现把单次展开结果挂在 CAF 或跨调用闭包里的结构；`familyNotations` 是静态注册表 | — | — |

## 已实施的改动

无（见第 3 节：核查过的结构要么安全、要么属语义必需或需数组后端提案；本轮没有落代码改动）。

`src\OmegaY\Fam\Den\` 5 个模块与 `.bench\base_tree` 对应副本 MD5 相等：

| 文件 | MD5（src 与 base_tree 一致） |
|---|---|
| `Blp.hs` | `96D2B88F92C20E125D8D08B015D64554` |
| `Iblp.hs` | `E2AA53C62FCBFA94B8DBAFE4A0DB030E` |
| `Den1.hs` | `F31F0E8E3FD0D093759EC0AA407C98FE` |
| `Share.hs` | `49C2CA94C372E7E0E160EC4161D38C17` |
| `Family.hs` | `BC8AB6240CD9D06A0477FC8EEC5A71AE` |

## 正确性证据

差分汇总行（`.bench\reportDen.txt` 原文）：

```
family cases=2880 ok=2880 mismatch=0 nonzero_exit=0
  den1         ok=960 bad=0
  blp          ok=960 bad=0
  iblp         ok=960 bad=0
```

完整命令（仓库根 `D:\expander\expander2.haskell`）：

```powershell
python .bench\diff_fam.py --base .bench\probeBaseDen.exe --new .bench\probeDen.exe `
  --ids "den1,blp,iblp" --corpus test\corpus\den.txt `
  --terms 0,1,2,3 --modes fs,alter --params 0 --workers 4 --out .bench\reportDen.txt
```

- 用例数：120 行 × 3 id × 2 模式（fs/alter）× 4 term（0..3）× 1 param = 2880。
- 比较口径：退出码 + stdout 原始字节 + stderr 原始字节；非法输入的崩溃/错误文本被逐字节保护。
- 报告文件：`.bench\reportDen.txt`。
- 探针构建：`D:\sdk\ghc\ghcup\bin\ghc.exe -O1 -isrc -iapp -rtsopts --make -outputdir .bench\objDen -o .bench\probeDen.exe app\ProbeDen.hs`。

## 内存证据

测量手段：`+RTS -s`，每例 3 次取中位数；`GHC.Stats.allocated_bytes` 在 9.14 不可用，未使用；未做逐例 `performGC`。本轮未改源码，"改后"列与基线相同，变化 0.0%。

官方语料代表例（term=1，干净字节喂入，无尾随 CRLF）：

| 用例 | 改前 bytes allocated | 改后 bytes allocated | 变化 | 改前/改后 max residency | MUT 耗时(elapsed) |
|---|---:|---:|---:|---:|---:|
| den1 official（line116, fs） | 122,272 | 122,272 | 0.0% | 51,616 / 51,616 | <0.005s |
| den1 official（alter） | 128,688 | 128,688 | 0.0% | 51,616 / 51,616 | <0.005s |
| blp official | 93,824 | 93,824 | 0.0% | 51,616 / 51,616 | <0.005s |
| iblp official | 93,872 | 93,872 | 0.0% | 51,616 / 51,616 | <0.005s |

自造缩放例（行数 n，观测 residency 是否随行数堆积）：

| 用例 | n | bytes allocated | max residency | MUT elapsed | GC elapsed |
|---|---:|---:|---:|---:|---:|
| den1 | 8 | 134,064 | 51,616 | 0.001s | 0.000s |
| den1 | 16 | 325,712 | 51,616 | 0.001s | 0.001s |
| den1 | 32 | 1,099,152 | 51,616 | 0.002s | 0.001s |
| blp | 8 | 104,272 | 51,616 | 0.002s | 0.001s |
| blp | 16 | 145,808 | 51,616 | 0.001s | 0.001s |
| blp | 32 | 275,792 | 51,616 | 0.001s | 0.001s |
| iblp | 8 | 104,744 | 51,616 | 0.001s | 0.001s |
| iblp | 16 | 146,408 | 51,616 | 0.001s | 0.001s |
| iblp | 32 | 276,648 | 51,616 | 0.004s | 0.001s |

判读：

- 51,616 字节是 RTS 运行时 + nursery 的驻留下限，三档输入完全相同；业务存活数据远小于此。**residency 不随 n 增长**，排除了"节点/列表被跨阶段钉住"的泄漏特征（对照 xy 样板那种 residency 随行数爬升的现象）。
- bytes allocated 是累计分配量（含当轮释放的临时垃圾）：blp/iblp 近线性，den1 超线性（n 8→32 分配约 8.2 倍），根因见第 8 节时间问题。
- 缩放例为自造长输入，仅用于观测趋势；正确性验收只认官方语料 `test\corpus\den.txt`。

## 未做项与提案

- **提案 A（本族内，需专项重构，未落笔）**：把 `copy` / `compTo` / `compFrom` / `comp` / `expand` 中的行列表换成 `Data.Vector` 或 `runST` 内的 `STArray`，`setAt` 改为 O(1) 下标写；预期消除 den1 路径上 O(n²) 的 cons 整表复制，显著降低 bytes allocated；residency 预计基本不变（存活集合未变）。改动面覆盖 Blp/Iblp/Den1 三个模块的核心数据结构，需逐例差分验证，本轮 Corpus 规模小、收益主要在长输入，故未在本轮实施。
- **提案 B（局部清理，未落笔）**：`seqFrom` 链改反向累加器（替换 `init rec ++ [x]` / `rec1 ++ [y]`）；解析器尾追加改"反向构造再 reverse"。属时间 + 临时分配双重收益，语义等价但需差分复核。
- **死代码清理（可选，无堆收益）**：`limitRow`、`limitRowEntries`、`toShortRow`、`matchLit`、`rowAtE` 无调用方；后续可连同头文件对齐情况一并删除。
- **共享底座**：Den 未引用 `EntryGraph` / `Matrix` / `MVec` / `Xy.Common`，本轮无需对底座提改动；`Share.hs` 属本族模块，上述清理在本族权限内。

## 时间问题（不计入内存结论）

- 持久列表 `setAt` 每次写一行整表重建：Blp `copy`/`compFrom`、Iblp `copy`/`compFrom`、Den1 `comp`/`parentsFinal`，O(n²) 列表复制（提案 A）。
- `!!` 线性下标在循环内重复访问：Den1 `applyRow`（`s !! k`）、各 `seqFrom` / `migrate` / `pleasantUntil` 内的 `!!`；时间问题。
- 字符串拼接：`toString` 的 `joinComma` 递归 `x ++ "," ++ xs`，逐字符累积；输出规模小，列为已知 O(n²) 字符串构造。
- `compTo` 每处理一个标记条目就对整行 `sortEntriesDesc` 一次（对齐 C++ 每轮 `stable_sort`），属算法固有顺序，非内存泄漏。
- 分配趋势：blp/iblp 近线性；den1 超线性（n=8→32：134K→326K→1,099K），与上述列表重建/`!!` 站点对应。

## 非法输入说明

- 本语料 2880 例中 `nonzero_exit=0`：**未触发** `Prelude.!!` 越界崩溃或其它非零退出；基线与新探针在全部用例上退出码一致。
- 解析失败类用例（如 *`xxx`*、*`（0）1`*（全角括号）、*`()1`*、*`(1)1`*、*`;abc`*、*`(1,0)1(2,1,0)1;0,0,0;extra`*、含多余空格/逗号的畸形行）按原样返回固定提示串、退出码 0：
  - den1：`解析失败，请确认你没有使用全角括号，逗号，分号`
  - blp / iblp：`解析失败，请确认你没有使用全角括号，逗号`
- 未新增任何防御性索引、未做 clamp、未把越界读改成返回 0 以外的行为；代码中既有的 `atV` / `atE` / 长度判断是移植自 C++ 越界读 0 的既有语义，非本轮添加。ERR 集合与文本逐字不变。
