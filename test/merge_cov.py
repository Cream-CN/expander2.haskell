#!/usr/bin/env python3
"""合并逐族 coverage json，输出统一 coverage.json（good = 该 id 无任何 mismatch）。"""
import json, sys, glob
good, detail = [], {}
for f in sys.argv[1:-1]:
    try: d = json.load(open(f))
    except Exception: continue
    for i, v in d.items():
        cur = detail.setdefault(i, {"ok": 0, "bad": 0, "skipped_ub": 0})
        for k in ("ok", "bad", "skipped_ub"):
            cur[k] += v.get(k, 0)
for i, v in detail.items():
    if v["bad"] == 0 and v["ok"] > 0:
        good.append(i)
out = sys.argv[-1]
json.dump({"good": sorted(good), "detail": detail}, open(out, "w"), ensure_ascii=False, indent=1)
print("COVERAGE: verified=%d / registered=%d" % (len(good), len(detail)))
bad = [i for i, v in detail.items() if v["bad"] > 0]
if bad: print("NOT_ZERO:", ", ".join(bad))
