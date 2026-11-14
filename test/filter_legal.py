#!/usr/bin/env python3
"""按原 UI 的合法性规则筛掉语料里的不合法输入。

每种记号的合法表达式规则不同，本脚本不自己判，而是调用已经照抄了
`Expander.cpp` 校验逻辑的两个实现：

* `--check ui`（默认）：`expander2-hs --ui <id> fs <term> <param>`，输出以
  `ERR:` 开头即不合法（项数/参数/空输入/记号内部文法）。
* `--check oracle`：再跑一遍 C++ 预言机 `test/oracle/refui`，退出码非 0 或
  超时（原实现的未定义行为）也筛掉——这类输入没有参考输出，留着只会让差分卡死。
* `--check both`：两者都过才保留。

用法：
  python3 test/filter_legal.py --bin .build/expander2-hs \
      --ids g-y,epsilon-y --corpus test/corpus/base_seq.txt \
      --out test/corpus/legal/base_seq.txt

产物：
  <out>                 保留的合法输入（保持原顺序）
  <out>.dropped.txt     被筛掉的输入，逐行 `行号<TAB>记号<TAB>原因<TAB>原文`
  stdout 打印汇总
"""
import argparse
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor


def run(cmd, text, timeout):
    try:
        p = subprocess.run(cmd, input=text, capture_output=True, text=True,
                           timeout=timeout)
        return p.returncode, p.stdout
    except subprocess.TimeoutExpired:
        return "timeout", ""


def load_params(bin_path):
    """id -> (kind, default_param)"""
    out = subprocess.run([bin_path, "--params"], capture_output=True,
                         text=True, check=True).stdout
    table = {}
    for line in out.strip().splitlines():
        parts = line.split("\t")
        if len(parts) < 3:
            continue
        ident, desc, kind = parts[0], parts[1], parts[2]
        default = 0
        if desc != "-" and "default=" in desc:
            default = int(desc.split("default=")[1])
        table[ident] = (kind, default)
    return table


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", required=True, help="Haskell CLI 路径")
    ap.add_argument("--refui", default="test/oracle/refui")
    ap.add_argument("--ids", required=True, help="逗号分隔的记号 id")
    ap.add_argument("--corpus", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--check", choices=["ui", "oracle", "both"], default="both")
    ap.add_argument("--terms", default="1,2", help="校验时使用的项数（默认 1,2）")
    ap.add_argument("--timeout", type=float, default=5.0)
    ap.add_argument("--jobs", type=int, default=6)
    ap.add_argument("--drop-slow", action="store_true",
                    help="把 Haskell 侧展开超时的输入也筛掉（默认只记录到 .slow.txt，仍保留）")
    args = ap.parse_args()

    params = load_params(args.bin)
    ids = [i for i in args.ids.split(",") if i]
    unknown = [i for i in ids if i not in params]
    if unknown:
        print("未知记号 id：" + ",".join(unknown), file=sys.stderr)
        return 2
    terms = args.terms.split(",")

    with open(args.corpus, encoding="utf-8") as f:
        lines = [ln.rstrip("\n") for ln in f]

    dropped = []
    slow = []
    kept = []

    def check_one(idx_line):
        idx, text = idx_line
        if not text.strip():
            return idx, text, None  # 空行原样保留
        slow_marks = []
        for ident in ids:
            param = params[ident][1]
            for term in terms:
                if args.check in ("ui", "both"):
                    rc, out = run([args.bin, "--ui", ident, "fs", term, str(param)],
                                  text, args.timeout)
                    if rc == "timeout":
                        # 慢 ≠ 不合法：默认保留，仅登记；--drop-slow 时才筛掉
                        slow_marks.append((ident, f"HS 展开超时（>{args.timeout}s）"))
                        if args.drop_slow:
                            return idx, text, ("DROP", ident, f"HS 展开超时（>{args.timeout}s）")
                        continue
                    if out.startswith("ERR:"):
                        return idx, text, ("DROP", ident, out.strip()[4:])
                if args.check in ("oracle", "both"):
                    rc, _ = run([args.refui, ident, "fs", term, str(param)],
                                text, args.timeout)
                    if rc == "timeout":
                        return idx, text, ("DROP", ident, f"C++ 预言机超时（>{args.timeout}s，原实现未定义行为）")
                    if rc != 0:
                        return idx, text, ("DROP", ident, f"C++ 预言机退出码 {rc}（原实现未定义行为）")
        if slow_marks:
            return idx, text, ("SLOW", slow_marks[0][0], slow_marks[0][1])
        return idx, text, None

    with ThreadPoolExecutor(max_workers=args.jobs) as ex:
        for idx, text, reason in ex.map(check_one, enumerate(lines, start=1)):
            if reason is None:
                kept.append(text)
            elif reason[0] == "SLOW":
                kept.append(text)
                slow.append((idx, reason[1], reason[2], text))
            else:
                dropped.append((idx, reason[1], reason[2], text))

    os.makedirs(os.path.dirname(os.path.abspath(args.out)), exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as f:
        for text in kept:
            f.write(text + "\n")
    with open(args.out + ".dropped.txt", "w", encoding="utf-8") as f:
        for idx, ident, reason, text in dropped:
            f.write(f"{idx}\t{ident}\t{reason}\t{text}\n")

    print(f"corpus={args.corpus} ids={args.ids} check={args.check}")
    with open(args.out + ".slow.txt", "w", encoding="utf-8") as f:
        for idx, ident, reason, text in slow:
            f.write(f"{idx}\t{ident}\t{reason}\t{text}\n")

    print(f"  保留 {len(kept)} 行，筛掉 {len(dropped)} 行 -> {args.out}")
    if slow:
        print(f"  另有 {len(slow)} 行在 Haskell 侧展开超时（已保留，见 {args.out}.slow.txt）")
    if dropped:
        print(f"  明细：{args.out}.dropped.txt")
    return 0


if __name__ == "__main__":
    sys.exit(main())
