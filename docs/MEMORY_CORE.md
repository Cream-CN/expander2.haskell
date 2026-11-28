# MEMORY_CORE —— 底座（EntryGraph / ω-Y 序列）

## 结论摘要

`src\OmegaY\EntryGraph.hs`（ω-Y 序列与 ω-Y magma 共用的可变节点图）原来用 `IntMap EntryG` 做节点池：
每分配一个节点都要 `readSTRef` 整张表 + `IM.findMax`（O(log n) 且把整表拉起来）+ `IM.insert`（路径复制），
每次改节点又是"查一次 + 插一次"，而 `pushUpL` 用 `egUpL e ++ [child]` 追加子节点，单节点 k 个子节点就是 O(k²)。
这三点都是"结构本身在制造无用的中间对象"。

已改为**可增长槽位数组 + 分配水位**，`pushUpL` 改为 O(1) 前插（对外顺序由 `upLsOf` 还原）。
语义不变已用 Haskell 自比（-O0）验证：Mrss/ω-Y 四记号共 2784 例逐字节一致（stdout+stderr+退出码；该族语料本身 `nonzero_exit=0`，即没有崩溃用例）。
**未测内存数字**：本机提交内存（pagefile）已被占满，跑 `+RTS -s` 与后续 Yseq 全量差分时会 `WinError 1455`，详见"未做项"。

## 族结构地图

| 模块 | 职责 | 节点/临时结构存放处 |
|---|---|---|
| `src\OmegaY\EntryGraph.hs` | 可变节点图（`newNode`/`gGet`/`gMod`/`pushUpL`） | 改造前：`STRef s (IntMap EntryG)`；改造后：`STRef s (STArray s Int EntryG)` + `gCap` + `gNext` |
| `src\OmegaY\Fam\Mrss\OmegaYSeq.hs` | ω-Y sequence / medium / strong | 图用 EntryGraph；山峰列 `Mtn = STRef s (IntMap [Int])`（未改，见提案） |
| `src\OmegaY\Notation\YMagma.hs` | ω-Y magma（epsilon-y / g-y 走它） | 图用 EntryGraph |

## 发现的内存问题

| 位置 | 结构 | 为什么浪费/钉住死对象 | 状态 |
|---|---|---|---|
| `EntryGraph.hs` `newNode`（原 40-45 行） | `IM.findMax` + `IM.insert` | 每节点一次全表读取 + O(log n) 路径复制；旧表版本随 GC 变垃圾，节点多时是主要 churn | 已修 |
| `EntryGraph.hs` `gPut`/`gMod` | 查一次再插一次 | 同一节点两次路径复制 | 已修（原地读写槽位） |
| `EntryGraph.hs` `pushUpL` | `egUpL e ++ [child]` | 每次追加重建整条子节点列表，k 个子节点 O(k²) 且每次留下全长的旧列表垃圾 | 已修（前插 + `upLsOf`） |
| `OmegaYSeq.hs` `pushCol`（50 行） | `mWrite m i =<< (++ [v]) <$> mRead m i` | 每压一个条目重建整列 + `IntMap` 插入路径复制，列长 k 时 O(k²)；ω-Y strong 的列很长，这是"序列 arena 严重"的另一半 | **提案**（未落笔） |
| `OmegaYSeq.hs` `mRead`（28 行） | `IM.findWithDefault []` | 每读一次都要穿过整张 `IntMap`；列数据本身是 `[Int]`，读出来还要再遍历 | 提案 |

## 已实施的改动

1. `data Graph s = Graph { gArr, gCap, gNext }`：
   - `newGraph` 分配 64 槽初始数组；
   - `ensureCap` 需要时翻倍扩容并搬运已有槽位（`newArray` → `writeArray` 逐槽复制）；
   - `newNode` 在 `gNext` 处原地写、水位 +1，O(1)，无路径复制；
   - `gGet` 用 `gNext` 做边界判断（越界返回 `Nothing`，与原 `IM.lookup` 语义一致）；
   - `gPut` 原地写，`i >= gNext` 时把水位推到 `i+1`（与原 `IM.insert` 的 `findMax+1` 语义一致）；
   - `gSize = gNext`（原为 `IM.size`，两者都等于节点数）。
   **语义等价论证**：id 是连续递增分配的，`IM.insert` 建出的表在 `[0, next)` 上恰好满，故 `IM.size == findMax+1 == next`；查/改/增三种操作在两种表示下对相同 id 序列给出相同结果。

2. `pushUpL g i child = gMod g i (\e -> e { egUpL = child : egUpL e })`，
   新增 `upLsOf = reverse . egUpL`；`egUpL` 字段注释改为"逆序存放"。
   消费方改为 `upLs = upLsOf`：`src\OmegaY\Fam\Mrss\OmegaYSeq.hs:61`、`src\OmegaY\Notation\YMagma.hs:64`。
   **语义等价论证**：`child :` 与 `++ [child]` 产生同一条列表的不同朝向，`upLsOf` 把朝向还原；
   两个消费方都只通过 `upLs` 读取，没有别的代码直接读 `egUpL`（已全仓 grep 确认）。

## 正确性证据

- 基线：`.bench\base_tree\`（本轮开工前副本）编译的 `-O0` 探针；改后：当前树 `-O0` 探针。
- 命令（每条独立跑，避免并发）：
  ```powershell
  python .bench\diff_fam.py --base .bench\probeBaseMrssO0.exe --new .bench\probeMrssO0.exe `
    --ids "mrss121,omega-y-sequence,omega-y-medium,omega-y-strong" `
    --corpus test\corpus\mrss.txt --terms 0,1,2,3 --modes fs,alter --params 0 --workers 2 --out .bench\reportMrssO0_a.txt
  # 同上，--corpus test\corpus\mrss_seq.txt --out .bench\reportMrssO0_b.txt
  ```
- 结果原文：
  - `family cases=384 ok=384 mismatch=0`
  - `family cases=2400 ok=2400 mismatch=0`
- 差分器 `.bench\diff_fam.py` 已做两类噪声归一化：可执行文件名前缀、编译目录前缀（`.bench\base_tree\` 与 `src\`），
  否则崩溃用例的 HasCallStack 回溯会因构建路径不同产生假 mismatch（已实测并修掉）。

## 内存证据

未测。原因：本机提交内存已耗尽，任何新的探针进程创建都返回 `WinError 1455（页面文件太小）`，
`+RTS -s` 需要的额外进程无法保证。**这一栏不许估算**，等机器恢复后按下面命令补：
```powershell
"2,3,4,5" | .\.bench\probeBaseMrssO0.exe omega-y-medium alter 2 0 +RTS -s
"2,3,4,5" | .\.bench\probeMrssO0.exe      omega-y-medium alter 2 0 +RTS -s
```
取 `bytes allocated in the heap` 与 `maximum residency`，各跑 3 次取中位数。

## 未做项与提案

1. **`Mtn`（ω-Y 山峰列）改造**：`STRef s (IntMap [Int])` → 列槽位数组 + 每列可增长 `STUArray s Int Int`，
   `pushCol` 改 O(1) 摊还、`mRead` 改 O(1) 随机访问（读列时按需转 `[Int]` 供现有调用点使用）。
   预期收益：ω-Y strong 长序列的列追加从 O(k²) 降到 O(k)，且不再每压一条就留下整列垃圾。**未落笔**（本轮机器不可用）。
2. **`gMod` 的整记录复制**：每次改节点都写回整个 `EntryG`（含 `egY` 列表），高频修改时会产生大量中间记录。
   若把 `y`、`upL` 拆成独立槽位数组（`STUArray`/`STRef`），可只写要改的字段；但会牵动所有读点，需单独一轮。
3. **`Xy` 族的 `[[Int]]` 矩阵**：`drawMountain`/`copyElement`/`copyCloumn` 的逐列重建是 O(n²)，
   属时间问题，不计入内存账；根治需可变矩阵后端（列为可增长数组）。仍待做。

## 时间问题（不计入内存结论）

- `pushUpL` 的 O(k²) 已顺带消除（这是结构变更的直接后果，不是额外的时间优化）。
- `Mtn.pushCol` 的 O(k²) 未动。

## 非法输入说明

*本轮未新增任何非法输入处理分支。* ω-Y 与 xy-v3 一样存在"越界/负下标"类输入，按用户要求**保持原样崩溃**：
差分器把 `(退出码, stdout, stderr)` 一起比较，任何"把 ERR 变成正常输出"或"加防御性下标"的改动都会立刻暴露。
本轮 Mrss 语料 2784 例全部正常退出，未出现崩溃用例，因此"崩溃行为不变"这一条在本族无法直接取证；
Tree 族语料里有 96 例崩溃用例（`Prelude.!!: index too large`），其异常类型、消息、回溯位置在改造前后逐字一致。
