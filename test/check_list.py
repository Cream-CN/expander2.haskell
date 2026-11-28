#!/usr/bin/env python3
"""比对 --list 输出：Haskell 注册表必须与 C++ 预言机 119 行逐行一致（含排序）。

只比较文本行，不比较行尾（Windows 上 GHC 输出 CRLF、fixture 可能为 LF）。
"""
import subprocess, sys, os
HERE = os.path.dirname(os.path.abspath(__file__))
ref = open(os.path.join(HERE, "oracle", "list.txt"), encoding="utf-8").read().splitlines()
hs = subprocess.run([sys.argv[1], "--list"], stdout=subprocess.PIPE).stdout.decode("utf-8").splitlines()
if ref == hs:
    print("LIST_MATCH lines=%d" % len(ref)); sys.exit(0)
import difflib
d = list(difflib.unified_diff(ref, hs, "cpp", "hs", lineterm=""))
print("\n".join(d[:40])); print("LIST_MISMATCH ref=%d hs=%d" % (len(ref), len(hs))); sys.exit(1)
