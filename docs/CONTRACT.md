# expander2 → Haskell 计算层移植 · 契约（所有族共用）

本文件是各记号族唯一的行为约定。**严格遵守，不要自行更改。**

## 0. 目标与边界

- 把 `cream-cn/expander2` 的**计算层**（`Header/common`、`Header/core`、`Header/notation`）
  忠实移植为 Haskell；MFC UI（`Expander.cpp`、`.rc`、`.vcxproj`）**不在范围**。
- 参考仓库：`cream-cn/expander2`（**只读，禁止修改**；原 C++ 工程不随本仓库分发）。
- Haskell 工程：本仓库根目录。
- 唯一验收标准：**与 C++ 原实现逐字节一致**（输出文本、FS 截断、后缀、错误文本、
  以及原实现自身的 quirk 全部照搬）。不要"改进"算法，不要"修正"看似 bug 的行为。

## 1. 只碰自己族的文件

- 你**只能**在 `src/OmegaY/Fam/<Group>/` 下新增模块，并修改 `src/OmegaY/Fam/<Group>/Family.hs`
  的 `familyNotations` 列表。
- **禁止**修改共享模块：`src/OmegaY/{Matrix,Parse,MVec,Mrss,EntryGraph,Registry}.hs`、
  `src/OmegaY/Notation/{Types,SeqBasic,YMagma}.hs`、`app/Main.hs`、`app/Probe*.hs`、
  `test/*`。若确实需要新的共享能力，在你自己的族目录里写新模块。
- 每个族用**自己的编译输出目录**（见下），避免与其他族并发编译踩踏。

## 2. 构建与运行

```sh
./test/build_group.sh Mrss          # -> .build-mrss/probe
printf '1,2,3' | ./.build-mrss/probe omega-y-sequence fs 2
```

Windows 上产物是 `.build-<group>\probe.exe`；没有 sh 时直接用 GHC：

```powershell
ghc -O1 -isrc -iapp -outputdir .build-mrss -o .build-mrss\probe.exe app\ProbeMrss.hs
```

探针命令行与 C++ 侧完全同构：

```
probe <id> <fs|alter> <term> [param]      # 表达式从 stdin 读
probe --list                              # 输出 "<id>\t<display_name>"
```

工具链：GHC 9.14.1；**仅**用随 GHC 分发的包（base/containers/text/array/process/time），
不得引入 vector / cabal 依赖 / 联网安装。

## 3. 差分预言机

`test/oracle/ref` 是 C++ 计算层的 CLI（源码 `test/oracle/ref.cpp`），命令行与本工程同构。
若丢失可重建（约 35s，需要 `../expander2` 存在）：

```sh
./test/build_oracle.sh
```

差分脚本（逐字节比较**完整 stdout**）：

```sh
python3 test/diff.py --bin .build-mrss/probe --ids mrss121,g-y \
    --corpus test/corpus/mrss.txt --terms 1,2,3 --modes fs,alter --params 0,1
```

语料文件：每行一条表达式。**你必须自己按 C++ 语法构造覆盖面足够的有意义语料**，
至少包含：正常项、退化/边界项、以及会触发错误提示的非法输入（例如非标准矩阵、
全角括号、空输入）。参数化记号还要覆盖 `param` 的上下界与越界（越界必须给出
`ERR:参数必须是 <min> 到 <max> 之间的整数`）。

## 4. 共享基础模块（已实现，直接复用）

| 模块 | 内容 |
|---|---|
| `OmegaY.Matrix` | `Matrix`（行优先二维 Int）、`buildMatrix/fromColumns/toColumns/matAt/matSet/...` |
| `OmegaY.Parse` | `parseSequence`（逗号切分、去空白、`stoi` 静默丢弃非法项）、`seqToString`、`parseMatrix`、`matrixToString` |
| `OmegaY.MVec` | `ST` 可变 Int 向量（对应 C++ `std::vector<int>` 的原地操作） |
| `OmegaY.Mrss` | MrSS 表达式树（`Expr`）的解析/打印/展开，即 `Header/common/mrssshare.hpp` |
| `OmegaY.EntryGraph` | 条目图（BMS/UPMS 系 `Entry` 图的等价结构） |
| `OmegaY.Notation.Types` | `Notation` 记录 + `expandNotation`（复刻 `INotation::expand` 的 FS/alter/参数/序列分派与 FS 截断） |

`expandNotation` 已复刻这些语义，**不要绕过它**：
- 文本型：`fs` 且无 `text_alter` → 先展开再 `nTruncate`（默认 MrSS 末元素裁剪）；`alter` → 走 alter（无则走原函数）；最后拼 `suffix`。
- 参数化：先查 `param` 范围，越界返回错误文本；再按 fs/alter 分派；拼 `suffix`。
- 序列型：`term<1` → "项数必须为正整数"；空序列 → "序列不能为空"；`fs` 且结果长度>1 → `init`；`seqToString` 拼 `suffix`。

## 5. 忠实度要点

1. **逐字节**：空白、全半角、换行数量都属于输出的一部分。矩阵型输出按 `matrixToString`。
2. **错误文本照抄**：C++ 返回的"解析失败，请确认你没有使用全角括号，逗号"等字符串
   必须原样（UTF-8）出现在 Haskell 输出里。
3. **quirk 照搬**：越界索引、整数除法、负数取模、`std::stoi` 的截断行为、遍历顺序
   都要一致。若 C++ 有未定义行为，以**预言机实测行为**为准。
4. **整数语义**：C++ `int` 是 32 位。可能出现溢出的地方用 `Int`（64 位）并在需要时
   模拟 32 位截断——以预言机为准验证。
5. **性能**：C++ 用原地 `std::vector` 与 `std::map/set` 的地方，Haskell 相应使用
   `ST`+`MVec`、`Data.Map/Data.Set`，避免对长序列做二次方级别的 `++`/`!!`。

## 6. 验收（不达标不算完成）

对**该族每个 id**：
1. `probe --list` 的 id 集合与该族 id 清单完全一致；
2. `fs` 与 `alter` 两种模式都跑；参数化记号覆盖 `param` 的 min/max/越界；
3. `term` 至少覆盖 1..4（含 `term=0` 的序列型报错路径）；
4. 语料量：每族至少 `40 × id 数` 条输入（含非法输入），总计不少于 300 例；
5. `python3 test/diff.py ...` 输出 `mismatch=0`，并把该族语料存到
   `test/corpus/<group>.txt`（供集成后复跑）。

## 7. 交付回执（必须写清）

- 新增/修改的文件绝对路径清单；
- 每个 id 的模块名与实现要点（1 行）；
- 语料路径 + 实际用例数 + `mismatch` 数（原文粘贴 diff.py 最后一行）；
- 任何仍未覆盖的语法分支/已知差异（必须明说，不要隐瞒）。
