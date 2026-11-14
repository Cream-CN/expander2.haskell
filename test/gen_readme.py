#!/usr/bin/env python3
"""生成 README.md：覆盖表由实测 coverage.json 驱动，不手填。

  python3 test/gen_readme.py --bin .build/expander2-hs --cov coverage.json
"""
import argparse
import json
import os
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)

GROUPS = {
    "OmegaY.Fam.Bms.Family": ["bms-v1", "bms-v2", "bms-v21", "bms-v22", "bms-v23",
                              "bms-v3", "bms-v31", "bms-v32", "bms-v33", "bms-v4", "upms"],
    "OmegaY.Fam.Mrss.Family": ["mrss121", "omega-y-sequence", "epsilon-y", "g-y",
                               "xy-v1", "xy-v2", "xy-v3", "omega-y-medium", "omega-y-strong"],
    "OmegaY.Fam.Bmlike.Family": ["bsm", "bhm", "1-bm-bhm", "2-bm-bhm", "3-bm-bhm",
                                 "cms", "dsm", "blm", "wmm"],
    "OmegaY.Fam.Gms.Family": ["gms-gbms-omega-p", "gms-gbms-pqss", "gms-gbms-qss",
                              "gms-gbms-full", "gms-gbms-weird", "gms-upms-omega-p",
                              "gms-upms-pqss", "gms-upms-qss", "gms-upms-full",
                              "gms-upms-weird", "gms-lpms2-omega-p", "gms-lpms2-pqss",
                              "gms-lpms2-qss", "gms-lpms2-full", "gms-lpms2-weird",
                              "lpms", "lptss"],
    "OmegaY.Fam.Tree.Family": ["tbm", "tupms", "btbm", "btbm-weak", "bbm"],
    "OmegaY.Fam.M1ynss.Family": ["-1y-(n+1)ss", "t--1y-(n+1)ss", "bt1ynss", "bt2ynss",
                                 "bt3ynss", "btlynss", "abs-bm1ynss", "rel-bm1ynss"],
    "OmegaY.Fam.Den.Family": ["den1", "blp", "iblp"],
    "OmegaY.Fam.Ton.Family": ["ton-m", "ton-i", "ton-dr", "ton-drc", "ton-drp",
                              "ton-drpc", "ton-ibp", "ton-mc", "ton-mpc"],
    "OmegaY.Fam.Mn.Family": ["e0mn", "strong-e0mn", "omega-mn", "pcf-omega-mn",
                             "t-omega-mn", "1-mn", "2-mn", "3-mn", "s-omega2-mn",
                             "sa-omega2-mn", "s-omega-pow-omega-mn"],
    "OmegaY.Fam.Upmn.Family": ["up1mn", "up2mn-v1a", "up2mn-v1b", "up2mn-v1b-plus"],
    "OmegaY.Fam.Dbms.Family": ["up1dbms", "up2dbms-v1", "up2dbms-v1b-plus",
                               "up2dbms-v1c", "up2dbms-v2", "s1dbms", "s2dbms",
                               "s-omega-dbms", "s-omega-dbms-v2", "s-omega-dbms-v3",
                               "s-omega-p1-dbms", "s-omega2-dbms", "s-omega-square-dbms",
                               "ts-2-dbms", "ts-omega-dbms"],
    "OmegaY.Fam.Aw2mn.Family": ["a-omega2-mn-2", "weak-a-omega2-mn-2",
                                "a-omega2-mn-3", "weak-a-omega2-mn-3"],
}
BASE_MODULE = "OmegaY.Notation.SeqBasic"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", default=".build/expander2-hs")
    ap.add_argument("--cov", default="coverage.json")
    ap.add_argument("--out", default="README.md")
    a = ap.parse_args()

    listing = subprocess.run([os.path.join(ROOT, a.bin), "--list"],
                             stdout=subprocess.PIPE).stdout.decode()
    reg = {}
    for line in listing.splitlines():
        if "\t" in line:
            i, n = line.split("\t", 1)
            reg[i] = n

    names = {}
    for line in open(os.path.join(HERE, "oracle", "list.txt"), encoding="utf-8"):
        if "\t" in line:
            i, n = line.rstrip("\n").split("\t", 1)
            names[i] = n

    cov = json.load(open(a.cov)) if os.path.exists(a.cov) else {"good": [], "detail": {}}
    good = set(cov.get("good", []))
    detail = cov.get("detail", {})

    mod_of = {}
    for mod, ids in GROUPS.items():
        for i in ids:
            mod_of[i] = mod
    for i in ["empty", "prss", "pps", "pps4", "wpps4", "tpps4", "ewpps4", "spps4", "2-pps4"]:
        mod_of[i] = BASE_MODULE

    rows = []
    for i, n in names.items():
        if i in good:
            st = "✅ 已验证"
        elif i in reg:
            st = "⚠️ 已注册·未全对齐"
        else:
            st = "❌ 未移植"
        cnt = detail.get(i, {}).get("ok", 0)
        rows.append((i, n, mod_of.get(i, "—"), str(cnt), st))

    n_good = sum(1 for r in rows if r[4].startswith("✅"))
    n_reg = len(reg)
    table = "\n".join("| `%s` | %s | `%s` | %s | %s |" % r for r in rows)

    doc = f"""# expander2 计算层 → Haskell 移植

把 [cream-cn/expander2](https://github.com/cream-cn/expander2) 的**计算层**
（`Header/common`、`Header/core`、`Header/notation`，约 90 个头文件 / 34k 行 C++）
忠实移植为 Haskell。MFC 图形界面（`Expander.cpp`、`Expander.rc`、`.vcxproj`）不在移植范围。

唯一验收标准是**与 C++ 原实现逐字节一致**：输出文本、FS 截断、后缀、错误提示、
以及原实现自身的 quirk（含几处越界/未定义行为）全部照搬，不做"改进"。

## 覆盖情况

- 记号总数：**114**（`registry.hpp::kMetaArray`）
- 已注册到 Haskell 注册表：**{n_reg}**
- 独立差分**逐字节归零**：**{n_good}**
- 其余记号的状态见下表（`❌ 未移植` / `⚠️ 已注册·未全对齐`）

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
python3 test/diff.py --bin .build-bmlike/probe \\
    --ids=bsm,bhm,1-bm-bhm,2-bm-bhm,3-bm-bhm,cms,dsm,blm,wmm \\
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
{table}

## 忠实度与已知 quirk

- **错误文本照抄**：例如 `解析失败，请确认你没有使用全角括号，逗号`、`输入的矩阵为非标准表达式`、
  `参数必须是 0 到 25 之间的整数`、`项数必须为正整数`、`序列不能为空`。
- **FS 分派**：文本型记号 fs 且无独立 alter 时，先展开再做 MrSS 末元素截断；序列型 fs 结果长度 > 1 时 `init`；
  参数化记号先校验 param 范围。
- **原实现的未定义行为**：C++ 在若干退化输入上会死循环（`omega-y-sequence` 的 `2,3`、
  `wMM` 部分退化输入）或段错误（`omega-y-medium` 的 `0`/`2`、`bt3ynss` 的 `(1)` 等）。
  这类输入两侧**不可比**，已从语料中剔除并在各族回执中记录；Haskell 侧不做"修复式"改写。
- **UI 不移植**：MFC 界面、资源、工程文件保持原样不动。

## 未移植 / 待续

见上表 `❌` 与 `⚠️` 行。主要包括：ε-Y / G-Y / X-Y 系列、部分 MN 变体、
`ts-omega-dbms`、树形矩阵族（若未完成）、TON 的两个变体边界、以及若干
"已注册但仍有少量输入未逐字节对齐"的记号。
"""
    open(os.path.join(ROOT, a.out), "w", encoding="utf-8").write(doc)
    print("README written: %s  (verified %d / %d, registered %d)"
          % (a.out, n_good, len(names), n_reg))


if __name__ == "__main__":
    main()
