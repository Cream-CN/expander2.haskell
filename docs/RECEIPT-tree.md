# 交付回执 — 树形矩阵族（Tree family）

移植目标：`tbm`、`tupms`、`btbm`、`btbm-weak`、`bbm` 共 5 个 id，C++ 参考逐字节对齐（含错误文本 `解析失败，请确认你没有使用全角括号，逗号`、FS 截断对本族为 no-op、空后缀、UB/崩溃输入按约定剔除）。

## 1. 新增/修改文件

仅在 `src/OmegaY/Fam/Tree/` 下新增/填写，未触碰共享模块、app/、test/ 与其它族：

- `src/OmegaY/Fam/Tree/Tbm.hs`
- `src/OmegaY/Fam/Tree/Tupms.hs`
- `src/OmegaY/Fam/Tree/Btbm.hs`
- `src/OmegaY/Fam/Tree/Family.hs`（注册 5 个 id）
- 语料：`test/corpus/tree.txt`

## 2. 每个 id 的模块与实现要点（1 行）

- **tbm**（`Tbm.hs`）：`Expr=Column[]=Entry[]=(value,height:Expr)`，复刻 TBM 超限树升格的 cmp 链/父母图/vertical→offset 升格、columnTruncate 截断、`Limit`/`ω`/`w` 解析与显示；`oneExpr=[]`、`kInfinity=2^30`、`omega=[[],[[1,one]]]`。
- **tupms**（`Tupms.hs`）：复用 TBM 原语，仅替换升格为延迟堆（ascension_threshold）：按列内 entry 数做线性最小提取的延迟 UPMS 升格。
- **btbm**（`Btbm.hs`）：分层矩阵，内部 0 基显示 +1，`(0)`=空列、`:index` 跳层、尾 `-1` 弹出；`infinityFs`、`verticalIncrease`、`ascendReplace`（`concatMap` 尾列替换）、`expandSpecial`。
- **btbm-weak**（`Btbm.hs`，`btbwExpandText`）：与 btbm 共用原语，分支规则改为 `w_ascend_replace` + 关键竖列（critical vertical）替换；`newCol0` 循环排除末 entry。
- **bbm**（`Btbm.hs`，`bbmExpandText`）：分支变体，特殊层 = 下一层末列置空；`bExpandSpecial` 在 `t_layer>0`（而非 >1）递归。

## 3. 语料与差分

- 语料路径：`test/corpus/tree.txt`
- 实际用例数：209 行表达式（覆盖正常项、`Limit`/`ω`/`w`、退化空列/尾逗号、全角括号/非法字符/未闭合括号等错误输入）。
- 命令：`python3 test/diff.py --bin .build-tree/probe --ids tbm,tupms,btbm,btbm-weak,bbm --corpus test/corpus/tree.txt --terms 0,1,2,3 --modes fs,alter`
- diff.py 最后一行原文：

```
cases=8208 ok=8208 mismatch=0 skipped_ub=112
```

每个注册 id 的 mismatch=0。

## 4. 仍未覆盖 / 已知差异（明说）

- **UB/崩溃输入已按约定从语料剔除**（不试图复刻崩溃）：下列输入会让 C++ 预言机非确定性 segfault（rc=139）或让 Haskell `!!` 崩溃，已从 `tree.txt` 排除，例如 `(1)(1)`、`(0)(1^(2))(0)`、`(1)(2^(3^(4)))`、`(1)(2,3)`、`(0)(1,2,3)`、`(1,2)(2,3)`、`(2)(3)`、`(0)(1^(2))` 等。这些把 btbm 风格（1 基/嵌套 `^n`）喂给 tbm/tupms 或触发单列/越界父母图，属原实现 UB。
- **一个 lenient-parse 边例未复刻**：`()(1^)`（`^` 后直接 `)`，C++ 把空 height 当空 Expr 并输出 `()()`）在 Haskell 侧 expansion 触发 `!!` 越界；已从语料剔除，未单独适配该空-height 边例。
- FS 截断：本族 MrSS 解析父串失败，`expandNotation` 的 FS 末元素截断为 no-op（已实证），与 C++ 一致。
- alter 模式对这些 FS-only 文本记号与 fs 同路径，已一并差分覆盖。
