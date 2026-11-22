# UPMN 族移植回执

## 1. 新增文件

所有文件位于 `src/OmegaY/Fam/Upmn/`：

- `Up1Mn.hs` — up1mn（UP1MN），纯整数矩阵 `[[Int]]`，独立 compute_up 算法
- `Up2MnV1a.hs` — up2mn-v1a（UP2MN v1A），smnshare Mountain 类型，含 expand_infinite 分支
- `Up2MnV1b.hs` — up2mn-v1b / up2mn-v1b-plus 共用薄包装，plus 参数控制
- `Family.hs` — 注册 4 个 id

跨族 import：`OmegaY.Fam.Dbms.MnUp2`（复用 parseExpr/displayExpr/exprValid/exprVert/parents/copyCol/setToRelCol/relColCmp/mnComputeUp1/up2mnCopyCol/up2mnInfinityFs/up2mnExpand）。

修改：`OmegaY/Fam/Dbms/MnUp2.hs` 新增 up2mnInfinityFs/up2mnCopyCol/up2mnExpand/up2mnComputeUp2 导出，并修复 mnComputeUp1 的 walkJ 提前 continue 缺失（与 C++ `if (!result[p]) { result[i]=0; continue; }` 对齐）。

语料：`test/corpus/upmn.txt`（149 条，含 Limit/空列/非法输入/边界矩阵；已剔除 22 条触发 C++ 预言机死循环的行）。

## 2. 每个 id 状态

| id | 模块 | 状态 |
|---|---|---|
| up1mn | Up1Mn | ✅ mismatch=0 |
| up2mn-v1b | Up2MnV1b (plus=False) | ✅ mismatch=0 |
| up2mn-v1b-plus | Up2MnV1b (plus=True) | ✅ mismatch=0 |
| up2mn-v1a | Up2MnV1a | ✅ mismatch=0 |

## 3. 差分结果

4 个 id 全部 `mismatch=0`；`coverage.json` 中每个 id 均为 `ok=1192, bad=0, skipped_ub=0`
（149 条语料 × 4 terms × 2 modes）。

## 4. 附带修复

- `MnUp2.hs` `mnComputeUp1`：补充 walkJ 后 `rs[p]==0` 时的提前 continue（C++ `continue` 语义），此前遗漏导致 up2mn-v1b/v1b-plus 在 `(2)(1,2)(1,2)` 类输入上 copy_column 错误地加 offset。
- `Up1Mn.hs`：修复 computeUp 返回 `initial` 而非 `foldl applyI initial` 的 bug（变量遮蔽）。
