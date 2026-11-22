#!/usr/bin/env python3
"""DLL 异常边界回归：记号内部抛出的 Haskell 异常必须被收成 NULL + LAST_ERROR，
不能让 uncaught exception 穿过 foreign export 把宿主进程 abort 掉。

  python3 test/dll_robustness.py --dll .build-dll/expander2.dll

背景：`OmegaY.Dll.expandWith` 原先只 `evaluate` 到 WHNF，`Right` 的字段仍是惰性
thunk，展开里的 `error` / 负索引要等 `newUtf8` 消费字符串时才爆，此时已出了 `try`，
于是变成 RTS uncaught exception，宿主进程直接终止（实测 `1-mn` + `(0,0)(1,1)`）。
2026-11-22 改为深求值（`length . forceStr`）后，这类输入返回 NULL。

判定：
  * 抛出型输入：返回 NULL，且 LAST_ERROR 非空；
  * 正常输入仍照常返回（深求值不该改变既有结果）；
  * 脚本能跑到最后 = 进程没被 abort（这是本测试最关键的一条）。
"""
import argparse
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from dll_smoke import Dll  # noqa: E402  （复用同一套调用契约 helper）

# (id, mode, term, param, input)：修复前会在宿主进程里 abort 的输入
THROWING = [
    ("1-mn", 0, 1, 0, "(0,0)(1,1)"),
    ("1-mn", 0, 1, 0, "(0,0)(1,1)(2,2)"),
]

# (id, mode, term, param, input, expected)：正常路径，确认深求值没有改变结果
NORMAL = [
    ("pps", 0, 3, 0, "1,2,3", "1,2"),
    ("pps", 1, 3, 0, "1,2,3", "1,2,3"),
    ("bms-v4", 0, 2, 0, "(0,0)(1,1)(2,2)(3,3)", "(0,0)(1,1)(2,2)(3,2)"),
]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dll", required=True)
    ap.add_argument("--expect-backend", default=None, help="llvm | ncg")
    a = ap.parse_args()

    d = Dll(a.dll)
    d.init()
    fails = []

    build = d.build_info() or ""
    print("build:", build)

    for ident, mode, term, param, payload in THROWING:
        out, err = d.call_generic(ident, mode, payload, term, param)
        if out is not None:
            fails.append(f"{ident} {payload!r}: 期望异常被拦截，却返回了 {out!r}")
        elif not err:
            fails.append(f"{ident} {payload!r}: 返回 NULL 但错误槽为空")
        else:
            print(f"ok   {ident} {payload!r} -> NULL / {err}")

    for ident, mode, term, param, payload, want in NORMAL:
        out, err = d.call_generic(ident, mode, payload, term, param)
        if out != want:
            fails.append(f"{ident} {payload!r}: {out!r} != {want!r}（err={err!r}）")
        else:
            print(f"ok   {ident} {payload!r} -> {out!r}")

    if a.expect_backend:
        got = "llvm" if "backend=llvm" in build else "ncg"
        if got != a.expect_backend:
            fails.append(f"后端不符：期望 {a.expect_backend}，实际 {got}")

    d.shutdown()

    # 能执行到这里，说明上面每个"抛异常"的调用都没有把进程带走。
    print(f"\ncases={len(THROWING) + len(NORMAL)} fails={len(fails)}（进程存活）")
    for f in fails:
        print("FAIL", f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
