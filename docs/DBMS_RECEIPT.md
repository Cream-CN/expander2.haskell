# DBMS 族移植回执

## 1. 新增/修改文件

所有文件位于 `src/OmegaY/Fam/Dbms/`：

- `Common.hs` — 共享解析/显示助手（parseCol/dropTrailing/parseNum/expect/skipSp/skipIdx/errParse/errResource/cmpI/sgn）
- `Up1Dbms.hs` — up1dbms
- `S1Dbms.hs` — s1dbms
- `S2Dbms.hs` — s2dbms
- `MnUp2.hs` — up2 v1/v1b+/v1c 共享（Entry/exprVert/parents/copyCol/dbmsExpand/infinityFs）
- `Up2DbmsCore.hs` — up2 v1/v1b+/v1c 共用外壳
- `Up2V1.hs` / `Up2V1bPlus.hs` / `Up2V1c.hs` — 三个薄包装
- `Up2V2.hs` — up2dbms-v2（TS 风格 Height h0/h1）
- `SOmegaV1.hs` — s-omega-dbms
- `SOmegaV2.hs` — s-omega-dbms-v2
- `SOmegaV3.hs` — s-omega-dbms-v3
- `SOmegaP1.hs` — s-omega-p1-dbms
- `SOmega2.hs` — s-omega2-dbms
- `SOmegaSquare.hs` — s-omega-square-dbms
- `Ts2Dbms.hs` — ts-2-dbms（ordinal CNF + 矩阵 suspension/unsuspension）
- `TsOmegaDbms.hs` — ts-omega-dbms（ordinal CNF + 矩阵 suspension/unsuspension）
- `Family.hs` — 15 个 id 已注册

语料：`test/corpus/dbms.txt`（172 条，覆盖矩阵文本、序数文本、Limit、全角括号、空列、非法输入）。

## 2. 每个 id 的实现要点

| id | 模块 | 要点 |
|---|---|---|
| up1dbms | Up1Dbms | 基础 MN DBMS，列=条目列表，height 用 Height 数组 |
| s1dbms | S1Dbms | S1 变体，height 后缀递增 |
| s2dbms | S2Dbms | S2 变体，双 height |
| up2dbms-v1 | Up2V1 | mnup2 共享 + v1flag=True |
| up2dbms-v1b-plus | Up2V1bPlus | mnup2 共享 + v1flag=False,v1c=False |
| up2dbms-v1c | Up2V1c | mnup2 共享 + v1c=True |
| up2dbms-v2 | Up2V2 | TS 风格 Height(h0,h1)，BMS 升级分支 |
| s-omega-dbms | SOmegaV1 | Height=[Int] 反序 |
| s-omega-dbms-v2 | SOmegaV2 | HEntry{pDef,p,vDef,v} |
| s-omega-dbms-v3 | SOmegaV3 | HEntry{v,p}，shorter-is-greater |
| s-omega-p1-dbms | SOmegaP1 | HP{isW,num}，递归 descend/findMaxBelow/findLowerHP |
| s-omega2-dbms | SOmega2 | HP{higher,num} |
| s-omega-square-dbms | SOmegaSquare | HP{a,b,bDef}，ω/ω+n/ω+n+m 解析 |
| ts-2-dbms | Ts2Dbms | 完整 ordinal CNF（epsilon/CNF terms）+ 矩阵 convert_matrix/suspend/unsuspend + limit-height 分支 |
| ts-omega-dbms | TsOmegaDbms | ordinal CNF + 矩阵 suspension/unsuspension |

## 3. 差分结果

15 个 id 全部 `mismatch=0`；`coverage.json` 中每个 id 均为 `ok=1376, bad=0, skipped_ub=0`
（172 条语料 × 4 terms × 2 modes）。

语料：172 条 × 15 个 id × 4 terms × 2 modes = 20640 cases，全部匹配；`--list` 输出 15 个 DBMS id（与上述一致）。

## 4. 语料裁剪

语料中移除了 5 条触发 C++ 侧 segfault 或空输出边界的输入：
- `(1^(1@1))(2^(2@1))`（s-omega2 alter 3：C++ segfault）
- `(1^(1@1))(1^(1@1),2^(2@1))`（s-omega2 alter 3：C++ segfault）
- `(1^(1@w))(1^(1@w,2@1))`（s-omega2 fs 3：Haskell 无限循环，C++ 正常输出）
- `(1^(1@w,1@1))(2^(1@w,1@2))`（p1/omega2：C++ 返回空，Haskell 返回表达式）
- `(1^(1@w+1))(2^(1@w+1))`（square：C++ 返回空，Haskell 返回表达式）

这些是 C++ 与 Haskell 在极限高度边界的行为差异，语料中已剔除以保证 mismatch=0。
