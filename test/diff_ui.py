#!/usr/bin/env python3
"""UI 级校验差分：refui（复刻 Expander.cpp 前置校验） vs expander2-hs --ui。

用法：
  python3 test/diff_ui.py --bin .build/expander2-hs --samples ... --terms ... --params ...

比较两者 stdout 是否逐字节一致；C++ 侧非 0 退出码记为 skipped_ub（原实现 UB）。
"""
import argparse
import json
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

DEFAULT_SAMPLES = [
    "",           # 空
    " ",          # 只有空白
    "abc",        # 完全不可解析
    "1,2,",       # 尾逗号
    "1,2,,3",     # 连续逗号
    "1，2",       # 全角逗号
    "（1,2）",     # 全角括号
    "(1,2",       # 缺右括号
    "(1,2))",     # 多右括号
    "()",         # 空列
    "0,0,1",      # 典型矩阵文本
    "-1,2",       # 负数
    "Limit",      # TON 系写法
    "1;2",        # 分号
]


def run(cmd, text, timeout):
    try:
        p = subprocess.run(cmd, input=text, capture_output=True, text=True,
                           timeout=timeout)
        return p.returncode, p.stdout
    except subprocess.TimeoutExpired:
        return "timeout", ""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", required=True, help="Haskell CLI")
    ap.add_argument("--refui", default="test/oracle/refui")
    ap.add_argument("--ids", default="", help="逗号分隔；空=全部")
    ap.add_argument("--samples", default="", help="用 | 分隔的样例清单")
    ap.add_argument("--terms", default="0,-1,abc,1")
    ap.add_argument("--params", default="0,26")
    ap.add_argument("--modes", default="fs")
    ap.add_argument("--timeout", type=float, default=8.0)
    ap.add_argument("--json", default="")
    ap.add_argument("--maxshow", type=int, default=5)
    ap.add_argument("--jobs", type=int, default=8)
    args = ap.parse_args()

    samples = args.samples.split("|") if args.samples else DEFAULT_SAMPLES
    terms = args.terms.split(",")
    params = args.params.split(",")
    modes = args.modes.split(",")

    listed = subprocess.run([args.refui, "--list"], capture_output=True,
                            text=True, check=True).stdout
    meta = {}
    for line in listed.strip().splitlines():
        parts = line.split("\t")
        if len(parts) >= 3:
            meta[parts[0]] = parts[2]
    if args.ids:
        ids = [i for i in args.ids.split(",") if i]
    else:
        ids = list(meta)
    ids = [i for i in ids if i in meta]

    cases = []
    for i in ids:
        for mode in modes:
            for term in terms:
                ps = params if meta[i] == "text_n" else ["0"]
                for param in ps:
                    for s in samples:
                        cases.append((i, mode, term, param, s))

    def one(c):
        i, mode, term, param, s = c
        cpp = run([args.refui, i, mode, term, param], s, args.timeout)
        hs = run([args.bin, "--ui", i, mode, term, param], s, args.timeout)
        if cpp[0] == "timeout":
            return ("cpp_timeout", c, cpp, hs)
        if cpp[0] != 0:
            return ("skipped_ub", c, cpp, hs)
        if hs[0] == "timeout":
            return ("hs_timeout", c, cpp, hs)
        if cpp[1] != hs[1]:
            return ("mismatch", c, cpp, hs)
        return ("ok", c, cpp, hs)

    stats = {"ok": 0, "mismatch": 0, "skipped_ub": 0, "hs_timeout": 0, "cpp_timeout": 0}
    shown = 0
    bad = []
    with ThreadPoolExecutor(max_workers=args.jobs) as ex:
        for res in ex.map(one, cases):
            stats[res[0]] += 1
            if res[0] in ("mismatch", "hs_timeout", "cpp_timeout"):
                bad.append(res)
                if shown < args.maxshow:
                    shown += 1
                    i, mode, term, param, s = res[1]
                    print(f"--- {res[0]} {i} {mode} term={term!r} param={param!r} text={s!r}")
                    print(f"    cpp(rc={res[2][0]}): {res[2][1].rstrip()[:200]}")
                    print(f"    hs (rc={res[3][0]}): {res[3][1].rstrip()[:200]}")

    print(f"cases={len(cases)} " + " ".join(f"{k}={v}" for k, v in stats.items()))
    if args.json:
        payload = {"stats": stats, "cases": len(cases),
                   "bad": [{"kind": b[0], "id": b[1][0], "mode": b[1][1],
                            "term": b[1][2], "param": b[1][3], "text": b[1][4],
                            "cpp": b[2][1], "hs": b[3][1]} for b in bad]}
        with open(args.json, "w") as f:
            json.dump(payload, f, ensure_ascii=False, indent=1)
    return 0 if stats["mismatch"] == 0 and stats["hs_timeout"] == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
