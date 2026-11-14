#!/usr/bin/env python3
"""逐个 id 冒烟校验：对注册表里每个记号，用小语料与预言机逐字节比对。

用途：集成时判断"哪些 id 真的可用"，不采信各族的自报。输出 coverage.json 与
一份人可读报告。

  python3 test/smoke.py --bin .build/expander2-hs --per-id 12 --json coverage.json
"""
import argparse
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REF = os.path.join(HERE, "oracle", "ref")
CORP = os.path.join(HERE, "corpus")

# 各族的语料文件；缺失时回退到通用池
FAMILY_CORPUS = {
    "base": "base_seq.txt",
    "mrss": "mrss.txt",
    "bmlike": "bmlike.txt",
    "gms": "gms.txt",
    "dbms": "dbms.txt",
    "mn": "mn.txt",
    "m1ynss": "m1ynss.txt",
}

GENERIC = ["1,2", "1,2,3", "1,1,2", "2,3", "(1)(2)", "[[0,1]]", "0(1,2)", "1"]


def run(binary, ident, mode, term, text, timeout=5):
    """返回 (stdout, returncode)。负 rc = 被信号杀死（原实现 UB）。"""
    try:
        p = subprocess.run([binary, ident, mode, str(term)],
                           input=text.encode(), stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE, timeout=timeout)
        return p.stdout.decode("utf-8", "replace"), p.returncode
    except subprocess.TimeoutExpired:
        return "<TIMEOUT>", 124


def load_pool(per_id):
    pool = []
    for fn in sorted(FAMILY_CORPUS.values()):
        p = os.path.join(CORP, fn)
        if not os.path.exists(p):
            continue
        lines = [l.rstrip("\n") for l in open(p, encoding="utf-8")]
        lines = [l for l in lines if l.strip()]
        pool.extend(lines[: per_id * 2])
    if not pool:
        pool = GENERIC[:per_id]
    return pool[: max(per_id * 2, 20)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", required=True)
    ap.add_argument("--per-id", type=int, default=6)
    ap.add_argument("--json", default="coverage.json")
    ap.add_argument("--only", default=None, help="逗号分隔的 id 子集")
    a = ap.parse_args()

    listing = subprocess.run([a.bin, "--list"], stdout=subprocess.PIPE).stdout.decode()
    ids = [l.split("\t")[0] for l in listing.splitlines() if l.strip()]
    if a.only:
        want = set(a.only.split(","))
        ids = [i for i in ids if i in want]

    pool = load_pool(a.per_id)
    result = {}
    for ident in ids:
        ok = bad = skipped = 0
        first = None
        for text in pool[: a.per_id]:
            for mode in ("fs", "alter"):
                for term in (1, 2):
                    r, rc = run(REF, ident, mode, term, text)
                    if rc != 0:
                        # 预言机自身崩溃/不终止 → 原实现 UB，不可比，跳过
                        skipped += 1
                        continue
                    h, _ = run(a.bin, ident, mode, term, text)
                    if r == h:
                        ok += 1
                    else:
                        bad += 1
                        if first is None:
                            first = (mode, term, text, r, h)
        result[ident] = {"ok": ok, "bad": bad, "skipped_ub": skipped}
        if bad:
            print("BAD %-28s ok=%d bad=%d  first=%r" % (ident, ok, bad, first))

    good = [i for i in ids if result[i]["bad"] == 0 and result[i]["ok"] > 0]
    tot_ub = sum(v["skipped_ub"] for v in result.values())
    print("VERIFIED_IDS=%d / TOTAL=%d  (skipped_ub=%d)" % (len(good), len(ids), tot_ub))
    json.dump({"good": good, "detail": result}, open(a.json, "w"),
              ensure_ascii=False, indent=1)
    return 0


if __name__ == "__main__":
    sys.exit(main())
