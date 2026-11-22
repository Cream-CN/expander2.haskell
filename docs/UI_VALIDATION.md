# UI 级输入校验（照抄原 `Expander.cpp`）

原版是 MFC 程序：**先由界面校验输入，再调用展开**。不合法输入不会进入展开，
而是弹 `MessageBoxW` 显示固定文案。Haskell 侧把这层逻辑单独实现为
`OmegaY.Notation.Types.uiExpand`，CLI 用 `--ui` 走这条路径。

## 1. 界面层的三条分支（按记号能力分派）

| 分支 | 适用记号 | 检查顺序 | 提示文本（逐字） |
|---|---|---|---|
| 文本型 `has_text` | BMS 族、BM-like、GMS、MrSS121、TON 族、Den、DBMS/MN 矩阵文本… | 项数能否 `stoi` → 项数 `<1` → 文本为空 | `项数必须是整数，\n所以说你为什么要这样输入` / `项数必须为正整数，\n你难道要尝试向内存反方向写入序列吗` / `序列不能为空` |
| 参数化 `has_text_n` | `-1y-(n+1)ss`、`t--1y-…`、`bt1/2/3ynss`、`btlynss`、`abs/rel-bm1ynss` | 项数与参数能否 `stoi` → 项数 `<1` → 参数越界 → 文本为空 | `项数与参数都必须是整数，\n所以说你为什么要这样输入` / 同上长效文案 / `参数必须是 {min} 到 {max} 之间的整数` / `序列不能为空` |
| 序列型 `else` | `omega-y-medium/strong/sequence`、`mrss121` 等序列记号 | 项数能否 `stoi` → 项数 `<1` → `parse_sequence` 结果为空 | `项数必须是整数` / `项数必须为正整数` / `序列不能为空` |

注意：文本型与参数化分支用的是**带换行和吐槽的长文案**，序列型用的是**短文案**，
这是原程序的既有差异，照抄不统一。

`std::stoi` 语义：允许前导空白与 `+`/`-`，遇非数字停止（`12abc` → 12），
无数字或越出 `int` 范围则抛异常 → 走"必须是整数"文案。

## 2. 记号内部的合法性规则（各记号不同）

界面层通过后，展开函数自己还会判合法：

| 记号族 | 规则 | 不合法时的输出 |
|---|---|---|
| `bms-v1`…`bms-v4` | `parse_matrix` 能否解析；再按版本做 `is_standard` 判定 | `解析失败，请确认你没有使用全角括号，逗号`；`输入的矩阵为非标准表达式` |
| `upms` | 省略尾随零的列写法，逐字符扫描（`(` 切列、`)` 收列，数字位跨非数字字符继续累加，`-` 仅在未读数字时生效，`,` 处重置） | 空矩阵 → `解析失败，请确认你没有使用全角括号，逗号`；展开成空矩阵 → `后继形展开为空矩阵，或未能定位坏根` |
| BM-like / GMS / Tree 等矩阵文本 | `bmlike::parse` 失败 | `解析失败，请确认你没有使用全角括号，逗号`（GMS 的 `GmsError` 也收敛到这句） |
| TON 族（`ton-*`） | TON 文法：`0` / `Ω` / `Ω<n>` / `C` 后缀 | `解析失败，请确认输入是 TON 文本（0 / Ω / Ω<n> / C 后缀），且不是全角字符` |
| `den1` / `blp` / `iblp` | Den 文本文法 | `解析失败，请确认你没有使用全角括号，逗号`（`den1` 用 `…逗号，分号`） |
| MN / DBMS / Aw2mn 族 | 矩阵文本（`ord` 记法） | `解析失败，请确认你没有使用全角括号，逗号，分号` |
| `mrss121`、Y 序列族 | MrSS 表达式文法：原子数字、`(…,…)` 列、`,` 分隔；**逗号后必须还有元素** | 解析失败 → 输出 `[]`（原实现不报错，只返回空） |
| 序列型记号 | `parse_sequence`：按 `,` 切分、段内空白剔除、非数字段丢弃 | 结果为空 → `序列不能为空` |
| 参数化记号 | 参数范围来自元数据（`0–25` 或 `1–25`） | 越界 → `参数必须是 {min} 到 {max} 之间的整数` |

## 3. 对拍方式

`test/oracle/refui.cpp` 是新增的 **UI 级预言机**：完全复刻上面的三条分支
（含 `MessageBoxW` 文案与判定顺序），再调用 `INotation::expand`。
错误文本里的换行统一转义成字面 `\n`，便于单行逐字节比较。

```sh
sh test/build_oracle.sh                      # 同时产出 ref 与 refui
python3 test/diff_ui.py --bin .build/expander2-hs --terms 1,0,abc,-1 --params 0,26
./.build/expander2-hs --ui bms-v4 fs 0      # 界面语义：项数非法 → ERR:项数必须为正整数…
```

`test/diff_ui.py` 的比较口径：C++ 侧非 0 退出码记为 `skipped_ub`（原实现 UB）。

## 4. 当前对拍结果

114 个记号全部接入，`coverage.json` 逐记号 `bad=0`（口径即 §1–§3 的界面分支 + 各记号内部规则）。
原版 C++ 自身越界 / 崩溃的输入记为 `skipped_ub`，没有参考输出，不参与比较。
批量筛选用例见下一节。

## 5. 两处容易踩的解析口径

1. **MrSS 尾逗号**：`1,2,` 解析失败，输出 `[]`（文法要求逗号后必须还有元素）。
2. **UPMS 逐字符扫描**：`((1),(2))` 是单列 `[1,2]`，不是两列；`(1,*)` 是 `[1]`，不是 `[1,0]`。

## 用语料过滤脚本批量筛掉不合法输入

`test/filter_legal.py` 把上面的判定批量应用到语料文件：Haskell `--ui` 输出 `ERR:` 即不合法；
`test/oracle/refui` 非零退出或超时视为原实现 UB，同样筛掉。**Haskell 展开慢不是不合法**，
默认保留并写进 `.slow.txt`，只有加 `--drop-slow` 才筛掉。

```sh
python3 test/filter_legal.py --bin .build/expander2-hs \
    --ids g-y,epsilon-y,xy-v1,xy-v2 --corpus test/corpus/base_seq.txt \
    --out test/corpus/legal/base_seq.txt --terms 1,2 --timeout 4 --jobs 6
./test/filter_all.sh                 # 12 族语料批量过滤
TERMS=1,2 TIMEOUT=6 ./test/filter_all.sh Ton Dbms
```

产物：`<out>` 为保留输入（原顺序），`<out>.dropped.txt` 为筛除明细
（`行号<TAB>记号<TAB>原因<TAB>原文`），`<out>.slow.txt` 为 Haskell 侧超时但合法的输入。

首轮实测：

| 语料 | 保留 | 筛掉 | 备注 |
|---|---|---|---|
| `base_seq.txt`（467 行，ids g-y/epsilon-y/xy-v1/xy-v2） | 349 | 118 | 另有 94 行 Haskell 展开超时但合法，已保留；去掉它们得到 `base_seq.diffsafe.txt`（254 行），Yseq/Xy 借此首次完成复跑 |
| `tree.txt` | 203 | 6 | |
| `aw2mn.txt` | 115 | 2 | 另 6 行 Haskell 超时保留 |
| 其余 9 族语料（bms/mrss/bmlike/gms/m1ynss/den/ton/mn/upmn/dbms） | 全部保留 | 0 | |

CLI 另加 `--params`：输出 `id<TAB>参数范围/默认值<TAB>seq|text|text_n`，过滤脚本据此给
参数化记号（如 `bt1ynss` 要求 1–25）选择合法默认参数，避免把合法输入误判成"参数越界"。
