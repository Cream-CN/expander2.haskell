#!/usr/bin/env python3
"""比对 --list 输出：Haskell 注册表必须与 C++ 预言机 114 行逐行一致（含排序）。"""
import subprocess, sys, os
HERE=os.path.dirname(os.path.abspath(__file__))
ref=open(os.path.join(HERE,"oracle","list.txt"),encoding="utf-8").read()
hs=subprocess.run([sys.argv[1],"--list"],stdout=subprocess.PIPE).stdout.decode("utf-8")
if ref==hs:
    print("LIST_MATCH lines=%d"%len(ref.strip().split("\n"))); sys.exit(0)
import difflib
d=list(difflib.unified_diff(ref.split("\n"),hs.split("\n"),"cpp","hs",lineterm=""))
print("\n".join(d[:40])); print("LIST_MISMATCH ref=%d hs=%d"%(len(ref.strip().split("\n")),len(hs.strip().split("\n")))); sys.exit(1)
