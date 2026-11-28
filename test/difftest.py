#!/usr/bin/env python3
"""差分测试：同一输入分别交给 C++ 参考实现与 Haskell 移植，逐字节比较输出。

用法：
  python3 test/difftest.py [--ids id1,id2] [--family seq|all] [--cases N]

语料按记号族生成；只有两侧都能处理的输入才会比对（C++ 返回解析失败时，
Haskell 必须给出同一条错误文本才算通过）。
"""
import argparse
import itertools
import random
import subprocess
import sys
import os

HERE = os.path.dirname(os.path.abspath(__file__))
HS = os.path.join(HERE, "..", ".build", "expander2-hs")
REF = os.environ.get("REF", "/tmp/oracle/ref")


def run(binary, ident, term, param, text):
    args = [binary, ident, str(term)]
    if param is not None:
        args.append(str(param))
    p = subprocess.run(args, input=text.encode("utf-8"), stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE)
    return p.stdout.decode("utf-8", "replace").rstrip("\n")


def run_hs(binary, ident, term, param, text):
    args = [binary, "--id", ident, "--term", str(term)]
    if param is not None:
        args += ["--param", str(param)]
    p = subprocess.run(args, input=text.encode("utf-8"), stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE)
    return p.stdout.decode("utf-8", "replace").rstrip("\n")


def seq_pool(limit=400):
    """序列型记号的输入池：短整数序列。"""
    base = []
    alph = [0, 1, 2, 3]
    for n in range(1, 7):
        for tup in itertools.product(alph, repeat=n):
            base.append(",".join(str(x) for x in tup))
            if len(base) > limit * 3:
                break
        if len(base) > limit * 3:
            break
    rnd = random.Random(7)
    extra = []
    for _ in range(limit):
        n = rnd.randint(1, 8)
        extra.append(",".join(str(rnd.randint(0, 5)) for _ in range(n)))
    return base[:limit] + extra


def compare(cases, verbose=False):
    ok = bad = skipped = 0
    fails = []
    for ident, term, param, text in cases:
        ref = run(REF, ident, term, param, text)
        hs = run_hs(HS, ident, term, param, text)
        if ref == hs:
            ok += 1
        else:
            bad += 1
            fails.append((ident, term, param, text, ref, hs))
    for f in fails[:20]:
        print("MISMATCH", f[0], "term=%s param=%s" % (f[1], f[2]),
              "input=%r\n  cpp=%r\n  hs =%r" % (f[3], f[4], f[5]))
    print("cases=%d ok=%d mismatch=%d" % (ok + bad, ok, bad))
    return bad == 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ids", default=None)
    ap.add_argument("--cases", type=int, default=60)
    ap.add_argument("--family", default="seq")
    args = ap.parse_args()

    ids = args.ids.split(",") if args.ids else None
    if args.family == "seq":
        pool = seq_pool(limit=args.cases)
        targets = ids or ["empty", "prss", "pps", "pps4", "wpps4", "tpps4",
                          "ewpps4", "spps4", "2-pps4"]
        cases = [(i, t, None, x)
                 for i in targets
                 for t in (1, 2, 3, 4)
                 for x in pool]
    else:
        print("unknown family", file=sys.stderr)
        return 1
    return 0 if compare(cases) else 1


if __name__ == "__main__":
    sys.exit(main())
