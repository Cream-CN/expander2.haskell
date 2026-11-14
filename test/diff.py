#!/usr/bin/env python3
"""逐字节差分：同一输入分别交给 C++ 预言机与本族探针，比较完整 stdout。

用法：
  python3 test/diff.py --bin .build-mrss/probe --ids mrss121,g-y \
      --corpus test/corpus/mrss.txt --terms 1,2,3 --modes fs,alter --params 0,1

约定：
  * 语料文件每行一条表达式（原样作为 stdin；去掉行尾换行）。
  * 两侧 stdout 都以单个换行结尾时按字节比较；不一致即 MISMATCH。
  * 退出码 0 表示该族全部一致。
"""
import argparse
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REF = os.path.join(HERE, "oracle", "ref")


def run(binary, ident, mode, term, param, text):
    args = [binary, ident, mode, str(term)]
    if param is not None:
        args.append(str(param))
    p = subprocess.run(args, input=text.encode("utf-8"),
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       timeout=5)
    return p.stdout.decode("utf-8", "replace"), p.returncode


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", required=True)
    ap.add_argument("--ids", required=True)
    ap.add_argument("--corpus", required=True)
    ap.add_argument("--terms", default="1,2,3")
    ap.add_argument("--modes", default="fs,alter")
    ap.add_argument("--params", default="")
    ap.add_argument("--maxshow", type=int, default=15)
    ap.add_argument("--json", default=None)
    a = ap.parse_args()

    ids = [x.strip() for x in a.ids.split(",") if x.strip()]
    terms = [int(x) for x in a.terms.split(",") if x.strip()]
    modes = [x.strip() for x in a.modes.split(",") if x.strip()]
    params = [int(x) for x in a.params.split(",")] if a.params else [None]

    lines = [ln.rstrip("\n") for ln in open(a.corpus, encoding="utf-8")]
    lines = [ln for ln in lines if ln.strip() != ""]

    ok = bad = 0
    per = {}
    fails = []
    for ident in ids:
        for mode in modes:
            for term in terms:
                for param in params:
                    for text in lines:
                        try:
                            ref, rrc = run(REF, ident, mode, term, param, text)
                        except subprocess.TimeoutExpired:
                            rrc = 124
                            ref = ""
                        if rrc != 0:
                            # 原实现 UB（崩溃/不终止）→ 不可比，跳过
                            per.setdefault(ident, [0, 0, 0])[2] += 1
                            continue
                        try:
                            hs, _ = run(a.bin, ident, mode, term, param, text)
                        except subprocess.TimeoutExpired:
                            hs = "<TIMEOUT>"
                        st = per.setdefault(ident, [0, 0, 0])
                        if ref == hs:
                            ok += 1
                            st[0] += 1
                        else:
                            bad += 1
                            st[1] += 1
                            if len(fails) < a.maxshow:
                                fails.append((ident, mode, term, param, text, ref, hs))
    for f in fails:
        print("MISMATCH id=%s mode=%s term=%s param=%s\n  input=%r\n  cpp=%r\n  hs =%r" % f)
    if a.json:
        import json
        json.dump({k: {"ok": v[0], "bad": v[1], "skipped_ub": v[2]}
                   for k, v in sorted(per.items())},
                  open(a.json, "w"), ensure_ascii=False, indent=1)
    ub = sum(v[2] for v in per.values())
    print("cases=%d ok=%d mismatch=%d skipped_ub=%d" % (ok + bad, ok, bad, ub))
    return 0 if bad == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
