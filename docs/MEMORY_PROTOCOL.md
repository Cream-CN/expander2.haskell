# 内存审计协议（MEMORY_PROTOCOL）

本文件是 14 族并行内存审计的统一口径。**任何族都必须按本文件施工与交付**，
不得自创验收方式。所有路径相对仓库根 `D:\expander\expander2.haskell`。

---

## 0. 背景与已完成的工作

本项目是 C++ 版 `D:\expander\Expander-Dev` 的 Haskell 移植（GC 语言）。用户原始诉求：

> 有些结构会浪费内存，以 xyv3 为例子，它的代码会持有大量已经没用的对象阻止 gc 执行，
> 先修这个问题，然后研究别的进行内存优化。

`Xy` 族的 `xy-v3` 已修完并通过验证，作为本轮的**样板**（不要回退它）：

- 把 arena 从 `STRef s (IntMap V3)` 换成可增长槽位数组 `STArray s Int V3` + 水位 `wNext`；
- 新增 `markArena` / `releaseTo`，在两处回退临时节点（`expand_impl` 画山前、
  `expandDimensionSequnece` 取 s 后）；
- 删掉死字段 `vOffset` 与 `offsets` 构造。
- 效果：5999 例语料快照逐字节一致；节点数 3.00→2.00 每输出项
  （4806→3204 @n=400/term3）；堆分配 96.26MB→73.35MB（−24%）。
- 仍未解决：`[[Int]]` 矩阵（`drawMountain`/`copyElement`/`copyCloumn`/
  `expandDimensionSequnece` 用 `init xs ++ [last xs ++ [eid]]`、`++ [[]]`、`++ [[first]]`
  逐列重建 → O(n²)），n=800/term3 仍 1.4s。这是**时间问题**，不是内存问题，报告里要分开写。

---

## 1. 不可违反的约束（违反即返工）

1. **非法输入保持原样（最重要）**。有些输入本身不合法（如 `1,4×…` 触发
   `Prelude.!!: negative index`），这类用例必须**继续崩溃、报同样的错**。
   *不改* 是用户明确要求。禁止加防御性 `atI`、禁止 clamp、禁止改成返回 0、
   禁止把 ERR 变成正常输出。判据是 "ERR 集合与文本逐字不变"，不是 "消灭 ERR"。
   报告里凡引用此类用例，**用斜体标注并说明不修**。
2. **只优化内存/分配，不改扩张语义**。任何 stdout/stderr/退出码变化一律视为失败。
3. 禁止 `git commit` / `stash` / `reset` / `checkout` / `clean`；禁止改 `.bench\base_tree`；
   禁止全文件重排格式；文件保持 **CRLF、无 BOM**；禁止新增依赖包。
4. **共享文件只允许「底座」子代理改**：`src\OmegaY\EntryGraph.hs`、`src\OmegaY\Matrix.hs`、
   `src\OmegaY\MVec.hs`、`src\OmegaY\Fam\Xy\Common.hs`、`src\OmegaY\Notation\*.hs`。
   其余族若认为必须改共享底座，**只能提案**（写进自己报告的"提案"一节），不得落笔。
5. 禁止修改 `app\Probe<族>.hs`（它是差分基线的一部分）；需要统计插桩请**新建**文件
   （如 `app\Probe<族>Stats.hs` 或 `.bench\Stats<族>.hs`）。
6. 编译目录必须独享：`-outputdir .bench\obj<族>`，不得与别的族共用。
7. 只改自己族的 `src\OmegaY\Fam\<族>\` 下的模块，以及自己那份文档。

---

## 2. 基线（已经替你建好，直接用）

基线 = `.bench\base_tree\`（本轮开工前的 src/app/test 完整副本），
14 个基线探针二进制已构建完成，**与本次改动无关，是唯一对照物**：

| 族 | 基线探针 | id 数 |
|---|---|---|
| Aw2mn | `.bench\probeBaseAw2mn.exe` | 4 |
| Bmlike | `.bench\probeBaseBmlike.exe` | 9 |
| Bms | `.bench\probeBaseBms.exe` | 11 |
| Dbms | `.bench\probeBaseDbms.exe` | 15 |
| Den | `.bench\probeBaseDen.exe` | 3 |
| Gms | `.bench\probeBaseGms.exe` | 17 |
| M1ynss | `.bench\probeBaseM1ynss.exe` | 8 |
| Mn | `.bench\probeBaseMn.exe` | 11 |
| Mrss | `.bench\probeBaseMrss.exe` | 4 |
| Ton | `.bench\probeBaseTon.exe` | 9 |
| Tree | `.bench\probeBaseTree.exe` | 5 |
| Upmn | `.bench\probeBaseUpmn.exe` | 4 |
| Xy | `.bench\probeBaseXy.exe` | 3 |
| Yseq | `.bench\probeBaseYseq.exe` | 2 |

用 `& .\.bench\probeBase<族>.exe --list` 取该族 id（id 清单也已汇总在 `.bench\ids.txt`）。
id 全集（务必全用，不要挑）：

- Aw2mn：`a-omega2-mn-2,weak-a-omega2-mn-2,a-omega2-mn-3,weak-a-omega2-mn-3`
- Bmlike：`bsm,bhm,1-bm-bhm,2-bm-bhm,3-bm-bhm,cms,dsm,blm,wmm`
- Bms：`bms-v1,bms-v2,bms-v21,bms-v22,bms-v23,bms-v3,bms-v31,bms-v32,bms-v33,bms-v4,upms`
- Dbms：`up1dbms,s1dbms,s2dbms,up2dbms-v1,up2dbms-v1b-plus,up2dbms-v1c,up2dbms-v2,s-omega-dbms,s-omega-dbms-v2,s-omega-dbms-v3,s-omega-p1-dbms,s-omega2-dbms,s-omega-square-dbms,ts-2-dbms,ts-omega-dbms`
- Den：`den1,blp,iblp`
- Gms：`gms-gbms-omega-p,gms-gbms-pqss,gms-gbms-qss,gms-gbms-full,gms-gbms-weird,gms-upms-omega-p,gms-upms-pqss,gms-upms-qss,gms-upms-full,gms-upms-weird,gms-lpms2-omega-p,gms-lpms2-pqss,gms-lpms2-qss,gms-lpms2-full,gms-lpms2-weird,lpms,lptss`
- M1ynss：`-1y-(n+1)ss,t--1y-(n+1)ss,abs-bm1ynss,rel-bm1ynss,bt1ynss,bt2ynss,bt3ynss,btlynss`
- Mn：`e0mn,strong-e0mn,omega-mn,1-mn,2-mn,3-mn,pcf-omega-mn,t-omega-mn,s-omega2-mn,sa-omega2-mn,s-omega-pow-omega-mn`
- Mrss：`mrss121,omega-y-sequence,omega-y-medium,omega-y-strong`
- Ton：`ton-m,ton-i,ton-dr,ton-drc,ton-drp,ton-drpc,ton-ibp,ton-mc,ton-mpc`
- Tree：`tbm,tupms,btbm,btbm-weak,bbm`
- Upmn：`up1mn,up2mn-v1a,up2mn-v1b,up2mn-v1b-plus`
- Xy：`xy-v1,xy-v2,xy-v3`
- Yseq：`epsilon-y,g-y`

语料（`test\corpus\`）：默认同名小写 `<族>.txt`。补充：
- Mrss → `mrss.txt` + `mrss_seq.txt`
- Yseq → `base_seq.txt` + `mrss_seq.txt`
- Xy → `xy3.txt`
- 若 `test\omegaY.sh` 里指定了别的组合，按它为准并在报告里写明你实际用了哪些语料文件。

---

## 3. 施工与验收步骤（每族照做）

### 3.1 构建新探针

```powershell
cd D:\expander\expander2.haskell
D:\sdk\ghc\ghcup\bin\ghc.exe -O0 -isrc -iapp -rtsopts --make `
  -outputdir .bench\obj<族> -o .bench\probe<族>.exe app\Probe<族>.hs
```

> 用户要求：**一律 `-O0`**，不要 `-O1`/`-O2`，也不要套 `ghc-flags.mk` 的 extreme profile；
> 基线探针与改后探针必须同一套参数，报告里注明"在 -O0 下测得"。

（各族一次性调用，不要并发起两个 GHC。）

### 3.2 逐例二进制差分（唯一验收方式）

仓库里已备好 `.bench\diff_fam.py`：同一 stdin 分别喂基线与新探针，
比较 **(退出码, stdout 原始字节, stderr 原始字节)**，因此"非法输入依旧崩溃"是被自动保护的条件。

```powershell
python .bench\diff_fam.py --base .bench\probeBase<族>.exe --new .bench\probe<族>.exe `
  --ids "<第 2 节的该族 id 全集，逗号分隔>" --corpus test\corpus\<语料> `
  --terms 0,1,2,3 --modes fs,alter --params 0 --workers 4 --out .bench\report<族>.txt
```

- M1ynss 的 `--params` 用 `0,1,2,25,26`；其余族 `--params 0`。
- 输出末行形如 `family cases=NNN ok=NNN mismatch=0`。**mismatch 必须为 0**，
  报告里必须原样贴这个汇总行，并把 `.bench\report<族>.txt` 留存。
- 报三个数：`cases`（总数）、`ok`、`mismatch`。
- 若你自己又新增了插桩探针（`app\Probe<族>Stats.hs`），验收仍然只用
  `app\Probe<族>.hs` 这一条链路，插桩不得影响它。

### 3.3 内存测量

- `GHC.Stats.allocated_bytes` 在 GHC 9.14 恒为 1216，**不可用**，别浪费时间。
- 逐例 `performGC` 会把 GC time 抬到秒级，是测量假象，**不要用**。
- 唯一可信手段：`+RTS -s` 输出的 "bytes allocated in the heap" 与 "maximum residency"。
  代表性用例各跑 3 次取中位数，报绝对值与百分比。
- 若要数"存活节点/输出项"（样板里用过的方式），参考 `.bench\NodeXy.hs`、
  `.bench\ScaleXy.hs` 的写法：**新建**自己的文件，用 `-outputdir .bench\obj<族>Stats` 编译。
- 缩放实验允许自造长输入（如把语料里的片段重复拼长），但**正确性验收只认官方语料**。

---

## 4. 判读要求：内存问题 vs 时间问题

必须分开写，不要把纯算法复杂度混进"内存结论"：

- **内存问题**：保留已死对象（arena/Map/列表钉住临时节点）、路径复制、装箱副本、
  大结构整块重建、无谓的 `IntMap` 插入/删除、把整个中间结果留在闭包里。
- **时间问题**：`init xs ++ [x]`、`++ [[x]]` 逐列重建导致的 O(n²)、重复遍历、重复格式化。
- 只优化内存的改动若同时顺带改了复杂度，要明确标注"顺带的时间收益"，不要算进内存账。

---

## 5. 交付物：`docs\MEMORY_<族>.md`（中文，Markdown，CRLF 无 BOM）

**固定章节**，缺一节算未完成：

1. `## 结论摘要` —— 3~5 行：查了什么、改了什么、收益多少、风险多大。
2. `## 族结构地图` —— 表：模块 → 职责 → 节点/临时结构存在哪（`STRef`? `IntMap`? 列表? 元组?）。
3. `## 发现的内存问题` —— 表：`文件:行` → 结构 → 为什么钉住死对象 → 可修/不可修 → 预估量级。
4. `## 已实施的改动` —— 逐处：改前/改后要点 + 一行"为什么语义等价"的论证。
   没改代码就写"无（见第 3 节，全部为不可修/提案）"。
5. `## 正确性证据` —— 原样贴 `cases/ok/mismatch` 汇总行 + 完整命令 + 报告文件路径。
6. `## 内存证据` —— 表：用例 → 改前 bytes allocated → 改后 → 变化% → 改前/改后 max residency → 耗时。
7. `## 未做项与提案` —— 尤其需要动共享底座的提案（写清具体改法与预期收益，别落笔）。
8. `## 时间问题（不计入内存结论）` —— 若有，列出来。
9. `## 非法输入说明` —— *斜体标注哪些用例是非法的、为什么保持不修*。

每个数字都要能追到一条命令；**不许估算**，测不出来就写"未测"并说明原因。
若结论是"该族没有可优化点"，第 3 节也要给出"查过哪些结构、为何安全"，
**不许为了凑数改代码**。
