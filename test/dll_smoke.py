#!/usr/bin/env python3
"""DLL 冒烟对拍：同一输入分别交给 expander2.dll（ctypes 直调）与 CLI（expander2-hs.exe），
逐字节比较结果 / 错误文本。

  python3 test/dll_smoke.py --dll .build-dll/expander2.dll \\
      --bin .build/expander2-hs.exe --per-id 6 --expect-backend llvm

做法：
  * 每个 id 单独起一个子进程跑（`--only <id>` 是内部模式），这样原实现
    的退化输入即使把 DLL 拖死/拖崩，也只损失那一个 id，不会毁掉整轮测试。
  * 调用契约全按头文件走：hs_init -> 记号入口 / EXPANDER2_EXPAND -> EXPANDER2_FREE。
  * 返回值与 CLI 的 stdout 比较时，只去掉 CLI 末尾那一个换行（Windows 上是 CRLF）。
"""
import argparse
import ctypes
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LEGAL = os.path.join(HERE, "corpus", "legal")

CCHAR_P = ctypes.c_char_p
CINT = ctypes.c_int
CVOID_P = ctypes.c_void_p


class Dll:
    def __init__(self, path):
        self.path = os.path.abspath(path)
        self.lib = ctypes.CDLL(self.path)
        l = self.lib
        l.hs_init.argtypes = [ctypes.POINTER(CINT), ctypes.POINTER(ctypes.POINTER(CCHAR_P))]
        l.hs_init.restype = None
        l.hs_exit.argtypes = []
        l.hs_exit.restype = None
        l.EXPANDER2_EXPAND.argtypes = [CCHAR_P, CINT, CINT, CINT, CCHAR_P]
        l.EXPANDER2_EXPAND.restype = CVOID_P
        l.EXPANDER2_FREE.argtypes = [CVOID_P]
        l.EXPANDER2_FREE.restype = None
        l.EXPANDER2_LAST_ERROR.argtypes = []
        l.EXPANDER2_LAST_ERROR.restype = CVOID_P
        l.EXPANDER2_BUILD.argtypes = []
        l.EXPANDER2_BUILD.restype = CVOID_P
        l.EXPANDER2_COUNT.argtypes = []
        l.EXPANDER2_COUNT.restype = CINT
        for nm in ("EXPANDER2_ID", "EXPANDER2_NAME", "EXPANDER2_SYMBOL"):
            f = getattr(l, nm)
            f.argtypes = [CINT]
            f.restype = CVOID_P
        for nm in ("EXPANDER2_KIND", "EXPANDER2_PARAM_MIN", "EXPANDER2_PARAM_MAX",
                   "EXPANDER2_PARAM_DEFAULT"):
            f = getattr(l, nm)
            f.argtypes = [CINT]
            f.restype = CINT

    # -- 契约 helper --------------------------------------------------------
    def init(self):
        argc = CINT(1)
        argv = (CCHAR_P * 2)(b"dll-smoke", None)
        p = ctypes.cast(argv, ctypes.POINTER(CCHAR_P))
        self.lib.hs_init(ctypes.byref(argc), ctypes.byref(p))

    def shutdown(self):
        self.lib.hs_exit()

    def take(self, ptr):
        if not ptr:
            return None
        s = ctypes.string_at(ptr).decode("utf-8", "replace")
        self.lib.EXPANDER2_FREE(ptr)
        return s

    def build_info(self):
        return self.take(self.lib.EXPANDER2_BUILD())

    def count(self):
        return int(self.lib.EXPANDER2_COUNT())

    def meta(self, i):
        return {
            "id": self.take(self.lib.EXPANDER2_ID(CINT(i))),
            "name": self.take(self.lib.EXPANDER2_NAME(CINT(i))),
            "symbol": self.take(self.lib.EXPANDER2_SYMBOL(CINT(i))),
            "kind": int(self.lib.EXPANDER2_KIND(CINT(i))),
            "pmin": int(self.lib.EXPANDER2_PARAM_MIN(CINT(i))),
            "pmax": int(self.lib.EXPANDER2_PARAM_MAX(CINT(i))),
            "pdef": int(self.lib.EXPANDER2_PARAM_DEFAULT(CINT(i))),
        }

    def call_symbol(self, sym, payload, term, param, kind):
        f = getattr(self.lib, sym)  # 缺失符号 -> AttributeError
        data = payload.encode("utf-8")
        if kind == 2:
            f.argtypes = [CCHAR_P, CINT, CINT]
            f.restype = CVOID_P
            ptr = f(data, CINT(term), CINT(param))
        else:
            f.argtypes = [CCHAR_P, CINT]
            f.restype = CVOID_P
            ptr = f(data, CINT(term))
        if ptr:
            out = ctypes.string_at(ptr).decode("utf-8", "replace")
            self.lib.EXPANDER2_FREE(ptr)
            return out, None
        return None, self.take(self.lib.EXPANDER2_LAST_ERROR()) or ""

    def call_generic(self, ident, mode, payload, term, param):
        ptr = self.lib.EXPANDER2_EXPAND(CCHAR_P(ident.encode()), CINT(mode), CINT(term),
                                        CINT(param), CCHAR_P(payload.encode()))
        if ptr:
            out = ctypes.string_at(ptr).decode("utf-8", "replace")
            self.lib.EXPANDER2_FREE(ptr)
            return out, None
        return None, self.take(self.lib.EXPANDER2_LAST_ERROR()) or ""


def decode_cli(raw):
    """CLI 的 stdout 编码取决于它启动时拿到的代码页：有控制台时是 UTF-8，
    无控制台（例如被 Start-Process -WindowStyle Hidden 拉起）时可能退到
    ANSI（cp936）。两种都试，取第一个能完整解码的。"""
    for enc in ("utf-8", "cp936"):
        try:
            return raw.decode(enc)
        except UnicodeDecodeError:
            pass
    return raw.decode("utf-8", "replace")


def cli_run(binpath, ident, mode, term, param, payload, timeout=20):
    """返回 (out, err)；out 为 None 表示 CLI 报错路径。CLI 崩溃 -> (None, None) 跳过。"""
    try:
        p = subprocess.run([binpath, ident, mode, str(term), str(param)],
                           input=payload.encode("utf-8"),
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
    except subprocess.TimeoutExpired:
        return None, None
    if p.returncode != 0:
        return None, None
    out = decode_cli(p.stdout)
    if out.endswith("\r\n"):
        out = out[:-2]
    elif out.endswith("\n"):
        out = out[:-1]
    if out.startswith("ERR:"):
        return None, out[4:]
    return out, None


def load_pool(per_id, corpus_dir):
    pool = []
    if os.path.isdir(corpus_dir):
        for fn in sorted(os.listdir(corpus_dir)):
            if not fn.endswith(".txt"):
                continue
            if fn.endswith(".dropped.txt") or fn.endswith(".slow.txt"):
                continue
            path = os.path.join(corpus_dir, fn)
            lines = [l.rstrip("\n") for l in open(path, encoding="utf-8") if l.strip()]
            pool.extend(lines[: max(3, per_id)])
    if not pool:
        pool = ["1,2", "1,2,3", "1,1,2", "(0)(1)", "(0,0)(1,1)", "[[0,1]]"]
    seen, uniq = set(), []
    for x in pool:
        if x not in seen:
            seen.add(x)
            uniq.append(x)
    return uniq[: max(per_id * 3, per_id)]


def params_for(m):
    if m["kind"] != 2:
        return [0]
    vals = [m["pdef"], m["pmin"]]
    vals += [m["pmax"]]
    out = []
    for v in vals:
        if v not in out:
            out.append(v)
    return out


def run_one(dll, binpath, ident, per_id, corpus_dir, terms, modes):
    d = Dll(dll)
    d.init()
    idx = {m["id"]: (i, m) for i in range(d.count()) for m in [d.meta(i)]}
    if ident not in idx:
        d.shutdown()
        return {"id": ident, "ok": 0, "bad": 0, "skip": 0,
                "error": "id 不在 DLL 注册表里"}
    i, m = idx[ident]
    pool = load_pool(per_id, corpus_dir)
    # 每个 id 取固定几个输入，保证可复现
    payloads = pool[:per_id]
    stats = {"id": ident, "ok": 0, "bad": 0, "skip": 0, "first": None}

    for mode_name, mode in (("fs", 0), ("alter", 1)):
        if mode_name not in modes:
            continue
        for term in terms:
            for param in params_for(m):
                for payload in payloads:
                    c_out, c_err = cli_run(binpath, ident, mode_name, term, param, payload)
                    if c_out is None and c_err is None:
                        stats["skip"] += 1
                        continue
                    try:
                        sym = m["symbol"] + ("_ALTER" if mode_name == "alter" else "")
                        d_out, d_err = d.call_symbol(sym, payload, term, param, m["kind"])
                    except AttributeError:
                        stats["bad"] += 1
                        if stats["first"] is None:
                            stats["first"] = {"why": "符号缺失", "symbol":
                                              m["symbol"] + ("_ALTER" if mode_name == "alter" else "")}
                        continue
                    if (d_out, d_err) == (c_out, c_err):
                        stats["ok"] += 1
                    else:
                        stats["bad"] += 1
                        if stats["first"] is None:
                            stats["first"] = {
                                "symbol": sym, "mode": mode_name,
                                "term": term, "param": param, "input": payload,
                                "cli": [c_out, c_err], "dll": [d_out, d_err],
                            }
    d.shutdown()
    return stats


def parent(args):
    d_probe = Dll(args.dll)
    d_probe.init()
    build = d_probe.build_info() or ""
    n = d_probe.count()
    ids = [d_probe.meta(i)["id"] for i in range(n)]
    d_probe.shutdown()
    print("dll: %s" % args.dll)
    print("cli: %s" % args.bin)
    print("build: %s" % build)
    if args.expect_backend and ("backend=" + args.expect_backend) not in build:
        print("FAIL: 期望 backend=%s，实际 build=%r" % (args.expect_backend, build))
        return 1

    if args.only:
        ids = [x for x in ids if x in set(args.only.split(","))]

    terms = [int(x) for x in args.terms.split(",") if x.strip()]
    modes = [x.strip() for x in args.modes.split(",") if x.strip()]

    results, failures = [], 0
    for ident in ids:
        # 用 --one=<id> 的形式传：有的 id 以 '-' 开头（如 -1y-(n+1)ss），
        # 空格分隔时 argparse 会把它当成选项。
        cmd = [sys.executable, os.path.abspath(__file__),
               "--dll=" + os.path.abspath(args.dll), "--bin=" + os.path.abspath(args.bin),
               "--one=" + ident,
               "--per-id", str(args.per_id), "--terms", args.terms,
               "--modes", args.modes, "--corpus", args.corpus]
        try:
            env = dict(os.environ)
            env["PYTHONIOENCODING"] = "utf-8"  # 子进程的 JSON 用 UTF-8 传回来
            p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               env=env, timeout=args.timeout)
        except subprocess.TimeoutExpired:
            results.append({"id": ident, "ok": 0, "bad": 1, "skip": 0,
                            "error": "子进程超时（疑似原实现 UB 死循环）"})
            failures += 1
            print("TIMEOUT %-30s" % ident)
            continue
        lines = [l for l in p.stdout.decode("utf-8", "replace").splitlines() if l.strip()]
        obj = None
        for l in lines:
            if l.startswith("{"):
                obj = json.loads(l)
        if obj is None:
            results.append({"id": ident, "ok": 0, "bad": 1, "skip": 0,
                            "error": "子进程没有输出结果：%s" % p.stderr.decode("utf-8", "replace")[-400:]})
            failures += 1
            print("CRASH  %-30s" % ident)
            continue
        results.append(obj)
        if obj.get("bad"):
            failures += 1
            print("BAD    %-30s ok=%d bad=%d  %r" % (ident, obj["ok"], obj["bad"], obj.get("first")))
        elif obj.get("error"):
            failures += 1
            print("ERROR  %-30s %s" % (ident, obj["error"]))
        else:
            print("ok     %-30s ok=%d skip=%d" % (ident, obj["ok"], obj["skip"]))

    ok = sum(r.get("ok", 0) for r in results)
    bad = sum(r.get("bad", 0) for r in results)
    skip = sum(r.get("skip", 0) for r in results)
    broken = [r["id"] for r in results if r.get("bad") or r.get("error")]
    print()
    print("cases=%d ok=%d mismatch=%d skipped=%d  ids=%d broken_ids=%d"
          % (ok + bad, ok, bad, skip, len(results), len(broken)))
    if args.json:
        json.dump({"build": build, "detail": results},
                  open(args.json, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    return 0 if bad == 0 and not broken else 1


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dll", required=True)
    ap.add_argument("--bin", required=True, help="CLI：expander2-hs.exe")
    ap.add_argument("--per-id", type=int, default=6)
    ap.add_argument("--terms", default="1,2")
    ap.add_argument("--modes", default="fs,alter")
    ap.add_argument("--corpus", default=LEGAL)
    ap.add_argument("--only", default=None, help="只跑这些 id（逗号分隔）")
    ap.add_argument("--one", default=None, help="内部用：只跑这一个 id，打印一行 JSON")
    ap.add_argument("--expect-backend", default=None, help="llvm | ncg")
    ap.add_argument("--timeout", type=int, default=90, help="每个 id 子进程的超时秒数")
    ap.add_argument("--json", default=None)
    args = ap.parse_args()

    if not os.path.exists(args.dll):
        print("找不到 DLL：%s" % args.dll)
        return 2
    if not os.path.exists(args.bin):
        print("找不到 CLI：%s" % args.bin)
        return 2

    if args.one:
        stats = run_one(args.dll, args.bin, args.one, args.per_id,
                        args.corpus, [int(x) for x in args.terms.split(",") if x.strip()],
                        [x.strip() for x in args.modes.split(",") if x.strip()])
        print(json.dumps(stats, ensure_ascii=False))
        return 0

    return parent(args)


if __name__ == "__main__":
    sys.exit(main())
