# YMagma（2 个 id：`omega-y-medium` `omega-y-strong`）

**一句话导读**：这族在 `EntryGraph` 条目池上做 ω-Y magma 展开——把整数序列建成山脉图、定位坏根列（BR）与列宽、收集提升条目、按 `term` 轮次复制补边、重排回写后输出整数序列；它与 Mrss 族的 `omega-y-sequence` 共用同一份 `src/OmegaY/Fam/Mrss/OmegaYSeq.hs` 流水和同一套 `src/OmegaY/EntryGraph.hs` 节点池，对应 `docs/diagrams/04-kernel-flow/` 的**第二链（ω-Y / MrSS 系）**：`EntryGraph` 节点池 → `fromSequence` + `drawMountain` → `findLower`/`findHigherEqual` → `collectWeak`/`collectStrong`（本支为 `collectUsual`/`collect1D`/`collectStrong`）→ `copySingleEdge`/`fillMagmaEdge` → `sortByY`/`rewire` → `toSequenceG`。

## 注册路径与目录名（先读这条）

- 本目录名 `YMagma` 是 `docs/notation-flows/coverage.py:21-23` 声明式指定的归类（「唯一一处声明式例外」），**不等于**注册源码所在目录。
- 注册表装配：`Registry.notationPool`（`src/OmegaY/Registry.hs:33-58`）经 `Mrss.familyNotations`（`Registry.hs:45`）收进记号；`src/OmegaY/Fam/Mrss/Family.hs:4-7` 从 `src/OmegaY/Fam/Mrss/OmegaYSeq.hs` 引入 `omegaYMediumNotation` / `omegaYStrongNotation`，并在 `Family.hs:15-16` 放进族列表。
- **运行时接线的展开函数**是 `OmegaYSeq.expandOmegaYMedium` / `expandOmegaYStrong`（`OmegaYSeq.hs:604-608`），底层流水 `expandOmegaY`（`OmegaYSeq.hs:589`）→ `omegaYLimit`（`OmegaYSeq.hs:378`）。本手册的「步骤」一节按这条实际接线书写。
- 并行副本（未接线）：`src/OmegaY/Notation/YMagma.hs:443-455` 还存在第二份 `seqNotation` 定义，展开走本文件自己的 `magmaExpand`（`YMagma.hs:280`）；这两个 Notation 值没有被 `notationPool` 引用（`Registry.hs:33-58` 装配清单未出现其名），运行时不可达。两份副本的差异见文末「未接线副本」一节。

## 共用骨架

下列步骤两条 id 共用，且与 Mrss 族 `omega-y-sequence` 共用（同一实现模块 `OmegaYSeq.hs`）。

1. **SeqCaps 外壳**：`expandNotation` 的 `SeqCaps` 分支先拦 `term < 1`（`src/OmegaY/Notation/Types.hs:75`）、`parseSequence`（`src/OmegaY/Parse.hs:16`）切序列、空则拦（`Types.hs:79`）。
2. **入口短路**（`expandOmegaY`，`OmegaYSeq.hs:589-599`）：`null seq` → `[]`；`last seq == 1` → `init seq`；`length seq == 1` → `seq`；否则算 `eff`（Medium/Strong 支 `eff = term`，仅 `KindSeq` 支 `eff = term + length seq`，`OmegaYSeq.hs:597`），调 `omegaYLimitKind`（`OmegaYSeq.hs:375-376`）→ `omegaYLimit`。
3. **建图与退化守卫**（`omegaYLimit`，`OmegaYSeq.hs:378-426`）：`phantomVal = if kind == KindSeq then kInfinity else 0`（`380`；`kInfinity = 2147483647`，`19-20`）；`fromSequence`（`84-99`）逐列建底节点 + phantom 节点并挂父链；`drawMountain`（`123-164`）沿父链上爬补父项条目。以下情形原样返回 `seq`：`n0 == 0`（`384`）、末列空（`388`）、`br < 0`（`394`）、`brx` 越界或 `width <= 0`（`400`）、`br` 不在本列（`404`）、`br` 已是本列末位（`407`）。
4. **重画坏部**：取 `top`（`410`）；用 `init seq ++ [last seq - 1]` 建第二座山 `m1`（`411-414`），再次 `drawMountain`；`brx` 越界（`416`）、`br2 < 0`（`421`）守卫后，`collectMagma`（`438-474`）收集条目表，`loopN`（`476-488`）按轮次展开，`toSequenceG`（`101-110`）回收序列。
5. **条目收集**（`collectMagma`，`438-474`）：按列宽 `0..width` 起空表；`collect`（`287-302`）分派：`KindStrong` → `collectStrong`（`262-285`）；否则判 `use1D`——`verticalCompare (yOf we) [1] > 0 && dr >= 0 && dimensionDifference(yOf we, yOf de) == 0`（`295-299`）→ `collect1D`（`237-260`），否则 `collectUsual`（`212-235`）。`add`（`467-472`）只收 `dx ∈ (0, width]` 的条目；沿 `downR` 递归。
6. **轮次复制**（`loopN`，`476-488`；`loopDx` `496-514`）：轮次 `n = 1..term`；取末列，`findLower`（`168-178`）对每个 `top` 节点在末列定位参考行得 `ref`；`ref` 空 → 停。逐 `dx = 1..width`：列号 `colx = brx + n*width + dx`（`499`），`mResize` 后清空；`oneEntry`（`516-536`）对每个条目：
   - `copySingleEdge`（`338-371`，偏移 `n*width`）复制节点；
   - `findHigherEqual`（`180-190`）在 `ref` 上找参考行 `he`；`he < 0` 跳过该条目（`520-521`）；
   - `loopUp`（`538-549`）沿 `rightleg_up` 逐级 `copySingleEdge` 复制到顶；
   - `yslice`（`192-205`）在左父列切片，`fillMagmaEdge`（`306-336`）按维数自增补边。
7. **排序与回写**：`sortByY`（`551-557`）按 `verticalCompare` 降序排本列；`rewire`（`559-583`）相邻互挂 `downR`/`upR`，首节点值置 1；中间节点值 = 上游值 + 上游 `leftleg_down` 的值（`571-583`，上游或左父缺失则跳过该节点）。
8. **序列化**：`toSequenceG`（`101-110`）每列取倒数第二个节点的值，不足两项补 0；`seqToString`（`src/OmegaY/Parse.hs:65`）后拼后缀，两条 id 后缀均为 `""`。

### `omega-y-medium` · ω-Y (medium)（idOrder #10）

- **族 / 模块**：YMagma · `src/OmegaY/Fam/Mrss/OmegaYSeq.hs`（注册路径经 `src/OmegaY/Fam/Mrss/Family.hs:15`）· 序列型 `SeqCaps`，后缀 `""`
- **入口**：`OmegaYSeq.expandOmegaYMedium`（`OmegaYSeq.hs:604-605`，注册对象 `omegaYMediumNotation` `OmegaYSeq.hs:615-618`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = magma 展开轮次（`loopN` 的 `n = 1..term`）；Medium 支 `eff = term`（`OmegaYSeq.hs:597`）；`param` 不参与（`SeqCaps` 分支不看 `nParam`）
- **步骤**：
  1. 走 §共用骨架 第 1 步外壳校验
  2. `expandOmegaY KindMedium`（`OmegaYSeq.hs:590`）进入第 2 步短路判定
  3. `omegaYLimit KindMedium`（`379`）：建图、守卫、重画、`collectMagma`、`loopN`、`toSequenceG`（第 3–7 步）
  4. 序列化输出（第 8 步）
- **分叉**：与 `omega-y-strong` 共用全部骨架，唯一差别是 `Kind` 取 `KindMedium`：条目收集走非 Strong 支（`collect` 的 `use1D` 判定 → `collect1D`/`collectUsual`，`OmegaYSeq.hs:290-302`）；无 `KindStrong` 的 phantom 回填分支；结果不截末项（与 `omega-y-sequence` 的差别见 Mrss 族手册）
- **错误**：仅 §00 通用 `SeqCaps` 两条（`项数必须为正整数` / `序列不能为空`）；族内不产生自有 `Left`

### `omega-y-strong` · ω-Y (strong)（idOrder #11）

- **族 / 模块**：YMagma · `src/OmegaY/Fam/Mrss/OmegaYSeq.hs`（注册路径经 `src/OmegaY/Fam/Mrss/Family.hs:16`）· 序列型 `SeqCaps`，后缀 `""`
- **入口**：`OmegaYSeq.expandOmegaYStrong`（`OmegaYSeq.hs:607-608`，注册对象 `omegaYStrongNotation` `OmegaYSeq.hs:620-623`），由 `expandNotation` 的 `SeqCaps` 分支调用
- **输入 → 输出**：逗号分隔整数序列 + `--term` → 逗号分隔整数序列
- **term / param 语义**：`term` = 展开轮次；Strong 支 `eff = term`（`OmegaYSeq.hs:597`）；`param` 不参与
- **步骤**：
  1. 走 §共用骨架 第 1 步外壳校验
  2. `expandOmegaY KindStrong`（`590`）短路判定
  3. `omegaYLimit KindStrong`（`379`）→ `collectMagma` 的 Strong 分派（第 3–7 步）
  4. 序列化输出（第 8 步）
- **分叉**：与 medium 同骨架，`Kind = KindStrong`：`collectMagma` 在当前节点 `y` 为空时走 phantom 回填分支（`OmegaYSeq.hs:447-459`，右侧每列取 `last col` 入表）；否则 `collect` 直入 `collectStrong`（`290`）；结果不截末项
- **错误**：仅 §00 通用 `SeqCaps` 两条；族内不产生自有 `Left`

## 未接线副本：`src/OmegaY/Notation/YMagma.hs`

下列内容属于未进入 `notationPool` 的并行副本，**运行时不可达**；列出仅为对照源码时不致混淆。

- 注册对象：`omegaYMediumNotation`（`YMagma.hs:443-448`）、`omegaYStrongNotation`（`YMagma.hs:450-455`），均为 `seqNotation`，后缀 `""`。内联 lambda 的短路顺序：`null seq` → `[]`；`seq == [maxBound :: Int]` → `[1, 1 + t]`；`last seq == 1` → `init seq`；否则 `magmaExpand Medium/Strong seq (max 1 t)`。
- `magmaExpand`（`YMagma.hs:280-312`）流水与 OmegaYSeq 版同构：`newGraph`/`newMtn` → `fromSequence`（`76-92`）→ `drawMountain`（`120-155`）→ 定位 BR/宽度/`top` → `m1` 重画 → `findSameRow`（`314-322`）→ `buildEntries`（`324-368`）→ `loopN`（`370-415`）→ `toSequence`（`94-105`）。
- 收集分派：`Medium` → `mediumLoop` + `collectWeak`（`YMagma.hs:338-347, 200-218`）；`Strong` → `strongLoop` + `collectStrong`（`349-365, 220-240`），phantom 回填取右侧各列 `last col`。
- 回写：`sortCol`（`417-423`）排序，`rewire`（`425-441`）回写；补边 `copySingleEdge`（`244-259`）/ `fillMagmaEdge`（`261-276`）。
- 与接线版的可观察差异：副本有 `[maxBound]` 特判、单元素序列不短路、`term` 取 `max 1 t`；接线版（OmegaYSeq）单元素原样返回、无 `maxBound` 特判、`eff` 按 Kind 计。

## 本族的「共用骨架 + 独有分支」判定

- **共用骨架 + 参数分叉**：`omega-y-medium` / `omega-y-strong` 同一份 `expandOmegaY` / `omegaYLimit` / `collectMagma` / `loopN`，分叉只在 `Kind`（`KindMedium` vs `KindStrong`）驱动的收集分派与 phantom 回填。
- **无独有分支实现**：两条 id 都没有独立于骨架的算法段。
- 跨族共用：本族两条与 Mrss 族 `omega-y-sequence` 共用 `OmegaYSeq.hs` 全部骨架；`EntryGraph.hs` 的节点池、`verticalCompare`/`verticalIncrease`/`dimensionDifference` 序运算子为三者共享。
