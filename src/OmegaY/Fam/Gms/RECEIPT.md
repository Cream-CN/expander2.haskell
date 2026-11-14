# GMS 族移植回执

## 1. 新增/修改文件（绝对路径）

- `/home/user/Doubao/chats/38453110731617282/expander2-hs/src/OmegaY/Fam/Gms/Common.hs` — FMat 列向量工具、错误串 kParseError、parseBmExpr。
- `/home/user/Doubao/chats/38453110731617282/expander2-hs/src/OmegaY/Fam/Gms/GmsCore.hs` — GMS 二维参数化内核（System × Proj）。
- `/home/user/Doubao/chats/38453110731617282/expander2-hs/src/OmegaY/Fam/Gms/LpmsCore.hs` — LPMS/LPTSS 提升引擎。
- `/home/user/Doubao/chats/38453110731617282/expander2-hs/src/OmegaY/Fam/Gms/Family.hs` — 注册全部 17 个 id。
- `/home/user/Doubao/chats/38453110731617282/expander2-hs/test/corpus/gms.txt` — 67 行语料。

未改动共享模块、app/Probe、test/ 脚本与其它族目录。

## 2. 每个 id 的模块与实现要点（1 行）

架构：底层记号（GBMS/UPMS/LPMS2）× 展开投影（ω-P/pQSS/QSS/Full/WeirdlyFull）二维参数化，非 15 份拷贝。

- `gms-gbms-omega-p` / `gms-gbms-pqss` / `gms-gbms-qss` / `gms-gbms-full` / `gms-gbms-weird` — GmsCore，System=GBMS，分别用 5 种 limitBuilder + fs/alter。
- `gms-upms-omega-p` … `gms-upms-weird` — GmsCore，System=UPMS，同 5 投影。
- `gms-lpms2-omega-p` … `gms-lpms2-weird` — GmsCore，System=LPMS2，同 5 投影。
- `lpms` — LpmsCore.runLpms False，lpms_single 提升状态机 + bms_fs 递归。
- `lptss` — LpmsCore.runLpms True，同一引擎，Limit 取 `[[i,i,i]]`。

## 3. 语料与差分

- 语料路径：`test/corpus/gms.txt`，67 行（含正常/非标准/解析失败/退化），已用 `timeout 3` 预检 oracle 无崩溃/死循环。
- 用例数：67 行 × 17 id × 4 term(0–3) × 2 模式(fs,alter) = 9112 cases；每 id 536 cases（≥40 行/id、≥300 总量满足）。
- diff.py 最后一行（原文）：

```
cases=9112 ok=9112 mismatch=0
```

## 4. 已知差异/未覆盖分支

无。17 个注册 id 全部 mismatch=0。
