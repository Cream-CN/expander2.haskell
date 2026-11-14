# expander2 计算层 → Haskell 移植

把 [cream-cn/expander2](https://github.com/cream-cn/expander2) 的**计算层**
（`Header/common`、`Header/core`、`Header/notation`，约 90 个头文件 / 34k 行 C++）
忠实移植为 Haskell。MFC 图形界面（`Expander.cpp`、`Expander.rc`、`.vcxproj`）不在移植范围。

唯一验收标准是**与 C++ 原实现逐字节一致**：输出文本、FS 截断、后缀、错误提示、
以及原实现自身的 quirk（含几处越界/未定义行为）全部照搬，不做"改进"。

## 覆盖情况

- 记号总数：**114**（`registry.hpp::kMetaArray`）
- 已注册到 Haskell 注册表：**114**
- 独立差分**逐字节归零**：**114**
- `--missing` 为空；下表 114 行全部为 ✅

## 本轮更新（MN 草稿 + X-Y V3）

本轮补齐最后 10 个记号，注册表从 104 增至 114：

- MN 族 9 个：`omega-mn`、`1-mn`、`2-mn`、`3-mn`、`pcf-omega-mn`、
  `t-omega-mn`、`s-omega2-mn`、`sa-omega2-mn`、`s-omega-pow-omega-mn`
- `xy-v3`（`notation/xy-v3.hpp`，745 行指针图算法）

关键修复（都是"草稿语义偏差"，不是数值误差）：

- `SmnShare.fillGhost`：父位置等于父列长度才进入幽灵行比较（条件原先写反）；
  逐列推进时 verticals 必须串联传递（`goI` 用上一步的 `v'`），否则每列都会重置。
- `OmegaMn`：`expandLoop` 目标列应为 `x + width*n`（草稿误写 `x`）；
  `getReferences` 必须前向累积；`adjustRightmost` 父列切片要读已改的末列。
- `TOmegaMn`：`addJ` 的 `otherwise` 分支漏了递归到 `j+1`，导致少追加一项。
- `SOmega2Mn`：`magmaIndices` 的 `row` 误把占位空行当列传，应为 `m !! i`。
- `XyV3`：整套图算法用 ST + IntMap arena 复刻 C++ 的 deque 裸指针语义
  （`parent/head/foot/ref` 互指、就地改 value/cloumn/idx/no/id/offset），
  行运算、`getBootIndex` 三分支、`copyElement`/`copyCloumn`、
  `expandDimensionSequnece` 与 `expand_impl` 逐行对照移植。

本轮差分（terms 0–3 × fs/alter）：

| 记号 | 语料 | 结果 |
|---|---|---|
| `omega-mn` / `1/2/3-mn` | `test/corpus/mn.txt` | 2712 / 456 UB |
| `pcf-omega-mn` | `test/corpus/pcf.txt` | 152 / 88 UB |
| `t-omega-mn` | `test/corpus/tmn2.txt` + `tmn.txt` | 376 / 80 UB |
| `s-omega2-mn` | `test/corpus/som2.txt` | 192 / 152 UB |
| `sa-omega2-mn` | `test/corpus/saom2.txt` + `saom2b.txt` | 316 / 228 UB |
| `s-omega-pow-omega-mn` | `test/corpus/sopom.txt` | 228 / 76 UB |
| `xy-v3` | `test/corpus/xy3.txt` + `base_seq.txt` | 9192 / 1392 UB |

上表均为 0 差异；"UB" 指 C++ 原实现断言崩溃/不终止、两侧不可比的输入。
另有一组 `pcf_rand.txt` 随机语料（80 例 0 差异、240 例 UB）。

## 目录结构

```
expander2-hs/
├── app/
│   ├── Main.hs              # 统一 CLI（114 个 id 的唯一入口）
│   └── Probe*.hs            # 单族差分探针（只装载一个族，便于隔离调试）
├── src/OmegaY/
│   ├── Matrix.hs            # 行优先二维 Int 矩阵（common/Matrix.hpp）
│   ├── Parse.hs             # parseSequence / parseMatrix / seqToString / matrixToString
│   ├── MVec.hs              # ST 可变 Int 向量（对应 C++ std::vector 的原地操作）
│   ├── Mrss.hs              # MrSS 表达式树（common/mrssshare.hpp）
│   ├── EntryGraph.hs        # Entry 山脉图（core/entry.hpp + Omega-YMagma.hpp）
│   ├── Notation/
│   │   ├── Types.hs         # Notation 记录 + expandNotation（INotation::expand 的分派语义）
│   │   ├── SeqBasic.hs      # empty / prss / PPS 系列
│   │   └── YMagma.hs        # 早期 ω-Y 引擎（已被 Fam/Mrss 取代，保留作对照）
│   ├── Fam/<Group>/         # 按记号族划分的实现（每族一个 Family.hs 汇总）
│   └── Registry.hs          # 唯一注册表：按 kMetaArray 顺序 + 显示名首码位稳定排序
└── test/
    ├── oracle/ref(.cpp)     # C++ 差分预言机（由原仓库源码编译）
    ├── build_oracle.sh      # 重建预言机（约 35s）
    ├── diff.py              # 通用逐字节差分（支持按 id / 模式 / term / param 组合）
    ├── run_all.sh           # 全部族一键复跑
    ├── smoke.py             # 逐 id 独立复核（生成 coverage.json）
    └── corpus/*.txt         # 各族语料（每行一条表达式）
```

## 构建与运行

只依赖 GHC 自带的包（base / containers / text / array / process），无需 cabal 联网。

```sh
cd expander2-hs
ghc -O1 -isrc -iapp -outputdir .build -o .build/expander2-hs app/Main.hs

./.build/expander2-hs --list                      # 已注册记号（按显示名排序）
printf '1,2,3' | ./.build/expander2-hs pps fs 3   # <id> <fs|alter> <term> [param]
```

表达式从 stdin 读入；错误以 `ERR:<错误文本>` 输出，与 C++ 侧逐字一致。

## 差分验证（可复现）

1. 重建 C++ 预言机（需要原仓库 `../expander2` 存在）：

```sh
./test/build_oracle.sh          # 生成 test/oracle/ref
```

2. 单族差分：

```sh
./test/build_group.sh Bmlike
python3 test/diff.py --bin .build-bmlike/probe \
    --ids=bsm,bhm,1-bm-bhm,2-bm-bhm,3-bm-bhm,cms,dsm,blm,wmm \
    --corpus test/corpus/bmlike.txt --terms 0,1,2,3 --modes fs,alter
```

3. 全部族一键复跑：`./test/run_all.sh`

4. 逐 id 独立复核（交付口径以此为准）：

```sh
python3 test/smoke.py --bin .build/expander2-hs --per-id 12 --json coverage.json
```

## 覆盖表

| id | 显示名 | Haskell 模块 | 验证用例数 | 状态 |
|---|---|---|---|---|
| `-1y-(n+1)ss` | (-1)Y-(n+1)SS 负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `1-bm-bhm` | 1BM-BHM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `1-mn` | 1MN | `OmegaY.Fam.Mn.Family` | 680 | ✅ 已验证 |
| `2-pps4` | 2-pps4 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `2-bm-bhm` | 2BM-BHM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `2-mn` | 2MN | `OmegaY.Fam.Mn.Family` | 680 | ✅ 已验证 |
| `3-bm-bhm` | 3BM-BHM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `3-mn` | 3MN | `OmegaY.Fam.Mn.Family` | 680 | ✅ 已验证 |
| `abs-bm1ynss` | AbsB(n+1)SS 绝对分支负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `a-omega2-mn-2` | Aω2MN2 | `OmegaY.Fam.Aw2mn.Family` | 872 | ✅ 已验证 |
| `a-omega2-mn-3` | Aω2MN3 | `OmegaY.Fam.Aw2mn.Family` | 872 | ✅ 已验证 |
| `bms-v1` | BMS v1.0 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v2` | BMS v2.0 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v21` | BMS v2.1 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v22` | BMS v2.2 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v23` | BMS v2.3 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v3` | BMS v3.0 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v31` | BMS v3.1 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v32` | BMS v3.2 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v33` | BMS v3.3 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bms-v4` | BMS v4.0 | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `bsm` | BSM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `bhm` | BHM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `blm` | BLM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `btbm` | BTBM Bubby3 超限巴斯丘矩阵 | `OmegaY.Fam.Tree.Family` | 1648 | ✅ 已验证 |
| `btbm-weak` | BTBM-weak 弱版 Bubby3 超限巴斯丘矩阵 | `OmegaY.Fam.Tree.Family` | 1648 | ✅ 已验证 |
| `bbm` | BBM 分支巴斯丘矩阵 | `OmegaY.Fam.Tree.Family` | 1648 | ✅ 已验证 |
| `bt1ynss` | BT*(n+1)SS v1 超限星负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `btlynss` | BTL(n+1)SS 带标记超限星负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `cms` | CMS | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `dsm` | DSM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `den1` | DEN1 | `OmegaY.Fam.Den.Family` | 960 | ✅ 已验证 |
| `ewpps4` | Ex. Weak PPS4 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `gms-gbms-omega-p` | GMS GBMS ω-P | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-gbms-pqss` | GMS GBMS pQSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-gbms-qss` | GMS GBMS QSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-gbms-full` | GMS GBMS Full | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-gbms-weird` | GMS GBMS Weirdly Full | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-upms-omega-p` | GMS UPMS ω-P | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-upms-pqss` | GMS UPMS pQSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-upms-qss` | GMS UPMS QSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-upms-full` | GMS UPMS Full | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-upms-weird` | GMS UPMS Weirdly Full | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-lpms2-omega-p` | GMS LPMS2 ω-P | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-lpms2-pqss` | GMS LPMS2 pQSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-lpms2-qss` | GMS LPMS2 QSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-lpms2-full` | GMS LPMS2 Full | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `gms-lpms2-weird` | GMS LPMS2 Weirdly Full | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `g-y` | G-Y | `OmegaY.Fam.Mrss.Family` | 424 | ✅ 已验证 |
| `lpms` | LPMS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `lptss` | LPTSS | `OmegaY.Fam.Gms.Family` | 536 | ✅ 已验证 |
| `mrss121` | MrSS1.2.1 | `OmegaY.Fam.Mrss.Family` | 696 | ✅ 已验证 |
| `prss` | PrSS | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `pps` | PPS | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `pps4` | PPS4 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `pcf-omega-mn` | PCF's ωMN | `OmegaY.Fam.Mn.Family` | 152 | ✅ 已验证 |
| `rel-bm1ynss` | RelB(n+1)SS 相对分支负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `spps4` | Second PPS4 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `s-omega2-mn` | Sω2MN | `OmegaY.Fam.Mn.Family` | 192 | ✅ 已验证 |
| `sa-omega2-mn` | SAω2MN | `OmegaY.Fam.Mn.Family` | 316 | ✅ 已验证 |
| `s-omega-pow-omega-mn` | Sω^ωMN | `OmegaY.Fam.Mn.Family` | 228 | ✅ 已验证 |
| `s1dbms` | S1DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s2dbms` | S2DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s-omega-dbms` | SωDBMS v1 | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s-omega-dbms-v2` | SωDBMS v2 | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s-omega-dbms-v3` | SωDBMS v3 | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s-omega-p1-dbms` | Sω+1DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s-omega2-dbms` | Sω2DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `s-omega-square-dbms` | Sω^2DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `tpps4` | Third PPS4 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `tbm` | TBM 超限巴斯丘矩阵 | `OmegaY.Fam.Tree.Family` | 1632 | ✅ 已验证 |
| `tupms` | TUPMS 超限 UPMS | `OmegaY.Fam.Tree.Family` | 1632 | ✅ 已验证 |
| `t--1y-(n+1)ss` | T(n+1)SS 超限负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `ton-m` | TON | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-i` | TON_I | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-dr` | TON_DoR | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-drc` | TON_DRC | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-drp` | TON_DRP | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-drpc` | TON_DRPC | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-ibp` | TON_IBP | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-mc` | TON_MC | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `ton-mpc` | TON_MPC | `OmegaY.Fam.Ton.Family` | 168 | ✅ 已验证 |
| `t-omega-mn` | TωMN | `OmegaY.Fam.Mn.Family` | 376 | ✅ 已验证 |
| `ts-2-dbms` | TS2DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `ts-omega-dbms` | TSωDBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `upms` | UPMS | `OmegaY.Fam.Bms.Family` | 2368 | ✅ 已验证 |
| `up1mn` | UP1MN | `OmegaY.Fam.Upmn.Family` | 1192 | ✅ 已验证 |
| `up2mn-v1a` | UP2MN v1A | `OmegaY.Fam.Upmn.Family` | 1192 | ✅ 已验证 |
| `up2mn-v1b` | UP2MN v1B | `OmegaY.Fam.Upmn.Family` | 1192 | ✅ 已验证 |
| `up2mn-v1b-plus` | UP2MN v1B+ | `OmegaY.Fam.Upmn.Family` | 1192 | ✅ 已验证 |
| `up1dbms` | UP1DBMS | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `up2dbms-v1` | UP2DBMS v1 | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `up2dbms-v1b-plus` | UP2DBMS v1B+ | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `up2dbms-v1c` | UP2DBMS v1C | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `up2dbms-v2` | UP2DBMS v2 | `OmegaY.Fam.Dbms.Family` | 1376 | ✅ 已验证 |
| `wpps4` | Weak PPS4 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |
| `xy-v1` | X-Y V1 | `OmegaY.Fam.Mrss.Family` | 450 | ✅ 已验证 |
| `xy-v2` | X-Y V2 | `OmegaY.Fam.Mrss.Family` | 1524 | ✅ 已验证 |
| `xy-v3` | X-Y V3 | `OmegaY.Fam.Mrss.Family` | 9192 | ✅ 已验证 |
| `blp` | blp | `OmegaY.Fam.Den.Family` | 960 | ✅ 已验证 |
| `e0mn` | e0MN | `OmegaY.Fam.Mn.Family` | 792 | ✅ 已验证 |
| `iblp` | iblp | `OmegaY.Fam.Den.Family` | 960 | ✅ 已验证 |
| `bt2ynss` | strong BT*(n+1)SS v2 超限星负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1480 | ✅ 已验证 |
| `strong-e0mn` | strong e0MN | `OmegaY.Fam.Mn.Family` | 792 | ✅ 已验证 |
| `wmm` | wMM | `OmegaY.Fam.Bmlike.Family` | 320 | ✅ 已验证 |
| `bt3ynss` | weak BT*(n+1)SS v3 超限星负一 Y 序列系统 | `OmegaY.Fam.M1ynss.Family` | 1408 | ✅ 已验证 |
| `weak-a-omega2-mn-2` | wAω2MN2 | `OmegaY.Fam.Aw2mn.Family` | 872 | ✅ 已验证 |
| `weak-a-omega2-mn-3` | wAω2MN3 | `OmegaY.Fam.Aw2mn.Family` | 872 | ✅ 已验证 |
| `epsilon-y` | ε-Y | `OmegaY.Fam.Mrss.Family` | 450 | ✅ 已验证 |
| `omega-y-medium` | ω-Y (medium) | `OmegaY.Fam.Mrss.Family` | 696 | ✅ 已验证 |
| `omega-y-strong` | ω-Y (strong) | `OmegaY.Fam.Mrss.Family` | 696 | ✅ 已验证 |
| `omega-y-sequence` | ω-Y sequence | `OmegaY.Fam.Mrss.Family` | 696 | ✅ 已验证 |
| `omega-mn` | ωMN | `OmegaY.Fam.Mn.Family` | 672 | ✅ 已验证 |
| `empty` | 空记号 | `OmegaY.Notation.SeqBasic` | 3716 | ✅ 已验证 |

## 忠实度与已知 quirk

- **错误文本照抄**：例如 `解析失败，请确认你没有使用全角括号，逗号`、`输入的矩阵为非标准表达式`、
  `参数必须是 0 到 25 之间的整数`、`项数必须为正整数`、`序列不能为空`。
- **FS 分派**：文本型记号 fs 且无独立 alter 时，先展开再做 MrSS 末元素截断；序列型 fs 结果长度 > 1 时 `init`；
  参数化记号先校验 param 范围。
- **原实现的未定义行为**：C++ 在若干退化输入上会死循环（`omega-y-sequence` 的 `2,3`、
  `wMM` 部分退化输入）或段错误（`omega-y-medium` 的 `0`/`2`、`bt3ynss` 的 `(1)` 等）。
  这类输入两侧**不可比**，已从语料中剔除并在各族回执中记录；Haskell 侧不做"修复式"改写。
- **UI 不移植**：MFC 界面、资源、工程文件保持原样不动。

## 完成状态

`kMetaArray` 的 114 个记号已全部接入并逐字节归零，`--missing` 为空。
后续如需扩展，只需在对应 `Fam/<Group>/` 下新增模块并在 `Family.hs` 注册。
