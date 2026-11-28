#!/usr/bin/env python3
"""图产物质检：对所有逐记号流程图的 CSV 与生成物做一遍硬规则检查。

检查项（全部来自 visio-csv-diagram 的硬性规则 + 本任务额外约束）：
  1. nodes.csv / edges.csv 存在，表头正确，edge 端点都在节点表里；
  2. 节点数 ≤ 40；
  3. 恰好 1 个 start，至少 1 个 end；
  4. 每个 decision 的出边都必须带 label，且出边 ≤ 3 条；
  5. label ≤ 20 字，不含英文逗号 / 双引号 / `<` / `&`；
  6. diagram.drawio 与 diagram.svg 存在且非空；drawio 里 mxCell 数 = 节点 + 连线 + 1（根层）。

用法：python docs/notation-flows/qc_diagrams.py [目录名 ...]
     不给参数则检查 docs/notation-flows/*/ 下所有含 nodes.csv 的目录。
"""
import csv
import re
import sys
from pathlib import Path

DOCS = Path(__file__).resolve().parent
MAX_NODES = 40
LABEL_MAX = 20
FORBIDDEN = set(',"<&')


def check(d: Path) -> list[str]:
    errs: list[str] = []
    nodes_csv, edges_csv = d / "nodes.csv", d / "edges.csv"
    for f in (nodes_csv, edges_csv):
        if not f.exists():
            return [f"缺文件 {f.name}"]

    with nodes_csv.open(encoding="utf-8-sig") as fh:
        nodes = list(csv.DictReader(fh))
    with edges_csv.open(encoding="utf-8-sig") as fh:
        edges = list(csv.DictReader(fh))

    if not nodes:
        return ["nodes.csv 为空"]
    if len(nodes) > MAX_NODES:
        errs.append(f"节点 {len(nodes)} > {MAX_NODES}")

    ids: dict[str, dict] = {}
    for n in nodes:
        nid = (n.get("id") or "").strip()
        if not nid:
            errs.append("有节点缺 id")
        if nid in ids:
            errs.append(f"节点 id 重复：{nid}")
        ids[nid] = n
        lab = (n.get("label") or "").strip()
        if not lab:
            errs.append(f"{nid} 缺 label")
        if len(lab) > LABEL_MAX:
            errs.append(f"{nid} label {len(lab)} 字 > {LABEL_MAX}：{lab}")
        bad = FORBIDDEN & set(lab)
        if bad:
            errs.append(f"{nid} label 含禁用字符 {''.join(sorted(bad))}：{lab}")

    out_count: dict[str, int] = {}
    for e in edges:
        s = (e.get("source") or "").strip()
        t = (e.get("target") or "").strip()
        for x in (s, t):
            if x not in ids:
                errs.append(f"边端点不在节点表：{x}")
        out_count[s] = out_count.get(s, 0) + 1
        lab = (e.get("label") or "").strip()
        if len(lab) > LABEL_MAX:
            errs.append(f"边 {s}->{t} 标签 {len(lab)} 字 > {LABEL_MAX}")
        if FORBIDDEN & set(lab):
            errs.append(f"边 {s}->{t} 标签含禁用字符：{lab}")

    types = [(n.get("type") or "process").strip() for n in nodes]
    starts = sum(1 for t in types if t in ("start", "开始"))
    ends = sum(1 for t in types if t in ("end", "结束"))
    if starts != 1:
        errs.append(f"start 节点数 = {starts}（应为 1）")
    if ends < 1:
        errs.append("没有 end 节点")

    for n in nodes:
        t = (n.get("type") or "process").strip()
        if t not in ("decision", "判断", "条件", "分支"):
            continue
        nid = (n.get("id") or "").strip()
        outs = [e for e in edges if (e.get("source") or "").strip() == nid]
        if len(outs) > 3:
            errs.append(f"判断 {nid} 出边 {len(outs)} 条 > 3")
        for e in outs:
            if not (e.get("label") or "").strip():
                errs.append(f"判断 {nid} 的出边 {e.get('target')} 缺 label")

    for f in ("diagram.drawio", "diagram.svg"):
        p = d / f
        if not p.exists() or p.stat().st_size == 0:
            errs.append(f"{f} 缺失或为空")
    if (d / "diagram.drawio").exists():
        body = (d / "diagram.drawio").read_text(encoding="utf-8")
        cells = len(re.findall(r'<mxCell[^>]*(?:vertex|edge)="1"', body))
        want = len(nodes) + len(edges)
        if cells != want:
            errs.append(f"drawio 里 vertex+edge 单元 {cells} != 节点+连线 {want}")
    if list(d.glob("*.vsdx")):
        errs.append("目录里出现了 .vsdx（本流程不产出 .vsdx）")

    return errs, len(nodes), len(edges)


def main() -> int:
    targets = sys.argv[1:]
    dirs = ([DOCS / t for t in targets] if targets
            else [p for p in sorted(DOCS.iterdir()) if p.is_dir() and (p / "nodes.csv").exists()])
    if not dirs:
        print("没有找到任何图目录")
        return 1
    bad = 0
    for d in dirs:
        res = check(d)
        if isinstance(res, tuple):
            errs, nn, ne = res
        else:
            errs, nn, ne = res, 0, 0
        if errs:
            bad += 1
            print(f"FAIL {d.name}  ({nn} 节点 / {ne} 边)")
            for e in errs:
                print(f"   - {e}")
        else:
            print(f"OK   {d.name}  ({nn} 节点 / {ne} 边)")
    print(f"\n{len(dirs) - bad}/{len(dirs)} 张图通过")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
