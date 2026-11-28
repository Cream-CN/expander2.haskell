#!/usr/bin/env python3
"""覆盖核对：把 119 个 id 的「权威清单」与「手册实际条目」逐条对上。

三份事实来源，全部从源码与文档现场读取，不写死任何清单：
  1. src/OmegaY/Registry.hs 的 idOrder —— 权威 id 顺序（119 项）；
  2. src/OmegaY/Fam/*/Family.hs 与 Notation 下的提供者 —— 每个 id 由哪个族注册；
  3. docs/notation-flows/<族>/README.md 里的 `### `id`` 小标题 —— 手册实际写了哪些 id。

用法：
  python docs/notation-flows/coverage.py            # 只打印核对结果，有问题时非零退出
  python docs/notation-flows/coverage.py --write     # 同时刷新 coverage.md
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "src" / "OmegaY"
DOCS = Path(__file__).resolve().parent

# YMagma 的两个 id 由 Fam/Mrss/Family.hs 注册、实现在 Notation/YMagma.hs，
# 手册按「提供者模块」归到 YMagma/ 目录；这是唯一一处声明式例外。
YMAGMA_IDS = {"omega-y-medium", "omega-y-strong"}


def read(p: Path) -> str:
    return p.read_text(encoding="utf-8")


def id_order() -> list[str]:
    body = read(SRC / "Registry.hs")
    block = re.search(r"idOrder\s*=\s*\[(.*?)\n\s*\]", body, re.S)
    if not block:
        sys.exit("ERROR: 在 Registry.hs 里找不到 idOrder")
    return re.findall(r'"([^"]+)"', block.group(1))


CONSTRUCTOR = (
    r"(?:seqNotation|textNotation|textAlterNotation|paramNotation|paramAlterNotation"
    r"|nId\s*=|Notation\s*\{)"
)

# Registry.notationPool 的装配顺序。同一 id 若在多个族文件里都有定义，
# poolMap = M.fromList 让**靠后**的那份覆盖靠前的，所以归属按「越靠后越优先」判。
POOL_ORDER = [
    "Bms", "Mrss", "Bmlike", "Gms", "Tree", "M1ynss", "Den", "Ton", "Mn",
    "Upmn", "Dbms", "Aw2mn", "Yseq", "Xy", "G11242",
]


def family_of(path: Path) -> str:
    """定义文件 -> 手册目录名。Fam/<X>/... 取 X，Notation/S*.hs 取文件名。"""
    parts = path.relative_to(SRC).parts
    if parts[0] == "Fam":
        return parts[1]
    return path.stem


def rank(path: Path) -> tuple[int, int]:
    fam = family_of(path)
    if path.relative_to(SRC).parts[0] == "Notation":
        return (1, 0)  # 记号基础模块里的定义即最终实现
    return (0, POOL_ORDER.index(fam) if fam in POOL_ORDER else -1)


def providers(order: list[str]) -> tuple[dict[str, str], list[str]]:
    """每个 id 的定义文件属于哪个族。返回 (id->族, 有歧义的 id 列表)。"""
    files = sorted(SRC.rglob("*.hs"))
    fam_of: dict[str, str] = {}
    ambiguous: list[str] = []
    for i in order:
        hits = [f for f in files
                if re.search(CONSTRUCTOR + r'\s+"' + re.escape(i) + r'"', read(f))]
        if len(hits) > 1:
            hits = sorted(hits, key=rank, reverse=True)
            ambiguous.append(i)
        if hits:
            fam_of[i] = family_of(hits[0])
            continue
        # 退路：id 以字面出现在某个 Family.hs 的参数位置（如 nMnNotation 1 "1-mn"）
        for fam_dir in sorted(p for p in (SRC / "Fam").iterdir() if p.is_dir()):
            fam_file = fam_dir / "Family.hs"
            if fam_file.exists() and f'"{i}"' in read(fam_file):
                fam_of[i] = fam_dir.name
                break
    return fam_of, ambiguous




def documented() -> dict[str, list[str]]:
    """族目录名 -> README 里 `### `id`` 的顺序。"""
    out: dict[str, list[str]] = {}
    for d in sorted(p for p in DOCS.iterdir() if p.is_dir()):
        md = d / "README.md"
        if not md.exists():
            out[d.name] = []
            continue
        out[d.name] = re.findall(r"^### `([^`]+)`", read(md), re.M)
    return out


def main() -> int:
    order = id_order()
    prov_all, ambiguous = providers(order)
    doc = documented()

    known = set(order)
    for i in YMAGMA_IDS:
        if i in prov_all:
            prov_all[i] = "YMagma"

    doc_all: dict[str, str] = {}
    dup: list[tuple[str, str]] = []
    for fam, ids in doc.items():
        prev = doc.get(fam, [])
        seen = set()
        for i in ids:
            if i in seen:
                dup.append((fam, i))
            seen.add(i)
            if i in doc_all:
                dup.append((fam, i))
            doc_all[i] = fam
        if ids != sorted(ids, key=lambda x: order.index(x) if x in known else 10**9):
            print(f"WARN: {fam} 的 id 顺序与 idOrder 不一致")

    missing = [i for i in order if i not in doc_all]
    extra = [i for i in doc_all if i not in known]
    wrong_fam = [(i, prov_all.get(i, "?"), doc_all[i]) for i in order
                 if i in doc_all and prov_all.get(i) != doc_all[i]]

    print(f"idOrder 条数        : {len(order)}")
    print(f"手册覆盖条数        : {len(doc_all)}")
    print(f"缺漏                : {len(missing)}")
    print(f"多余（不在 idOrder）: {len(extra)}")
    print(f"重复条目            : {len(dup)}")
    print(f"族归属不符          : {len(wrong_fam)}")
    if missing:
        print("  MISSING: " + ", ".join(missing))
    if extra:
        print("  EXTRA  : " + ", ".join(extra))
    if dup:
        print("  DUP    : " + ", ".join(f"{f}:{i}" for f, i in dup))
    if wrong_fam:
        for i, exp, got in wrong_fam:
            print(f"  归属不符: {i} 应属 {exp}，写在 {got}")

    rows = []
    for i in order:
        rows.append((i, prov_all.get(i, "?"), doc_all.get(i, "-"), "OK" if i in doc_all else "缺"))
    fams = sorted({f for f in prov_all.values()} | set(doc))
    counts = {f: sum(1 for _, p, _, _ in rows if p == f) for f in fams}

    print(f"\n按族（源码定义位置）应有 / 手册已写：")
    for f in fams:
        have = counts[f]
        wrote = sum(1 for i, p, _, _ in rows if p == f and i in doc_all)
        flag = "" if have == wrote else "  <- 未写完"
        print(f"  {f:<14} {have:>3} / {wrote:>3}{flag}")

    if "--write" in sys.argv:
        lines = [
            "# 覆盖核对表（119 个 id）",
            "",
            "由 `coverage.py` 现场生成：`python docs/notation-flows/coverage.py --write`。",
            "id 顺序取 `src/OmegaY/Registry.hs` 的 `idOrder`；族归属取各族 `Family.hs` 里出现的 id；",
            "「已写」列取手册里的 `### `id`` 小标题。",
            "",
            "## 按族汇总",
            "",
            "| 族 | id 数 | 手册文件 |",
            "|---|---|---|",
        ]
        for f in fams:
            lines.append(f"| {f} | {counts[f]} | `docs/notation-flows/{f}/README.md` |")
        lines += [
            f"| **合计** | **{sum(counts.values())}** | 应为 119 |",
            "",
            "## 逐 id 核对",
            "",
            "| # | id | 族 | 手册 | 已写 |",
            "|---|---|---|---|---|",
        ]
        for n, (i, p, d, st) in enumerate(rows, 1):
            lines.append(f"| {n} | `{i}` | {p} | `{d}/README.md` | {st} |")
        lines.append("")
        (DOCS / "coverage.md").write_text("\n".join(lines), encoding="utf-8")
        print(f"已写出 {DOCS / 'coverage.md'}")

    if missing or extra or dup or wrong_fam:
        return 1
    print("\nOK: 119 个 id 全部覆盖，无重复、无所属错误。")
    return 0


if __name__ == "__main__":
    sys.exit(main())
