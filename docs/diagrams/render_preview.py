#!/usr/bin/env python3
"""把 nodes.csv / edges.csv 渲染成带正交布线（channel routing）的 SVG 预览。

分层布局与 build_visio_source.py 一致（最长路径分层，左→右），
但连线走列间通道：水平进通道 → 通道内竖直 → 水平进目标左边缘，
因此不会压到任何节点框。只用于预览；Visio 主件是 skill 生成的 diagram.drawio。

用法: python render_preview.py <diagram_dir>
"""
import csv
import html
import sys
from pathlib import Path

MARGIN = 40
COL_W = 340
ROW_H = 150
HALF_W = 100
HEIGHT = {"process": 76, "start": 56, "end": 56, "decision": 92, "data": 76, "sub": 76, "note": 62}
ROW_OVERRIDE = {"01-module-calls": 108}
FILL = {
    "process": ("#DAE8FC", "#6C8EBF"),
    "start": ("#D5E8D4", "#82B366"),
    "end": ("#F8CECC", "#B85450"),
    "decision": ("#FFF2CC", "#D6B656"),
    "data": ("#E1D5E7", "#9673A6"),
    "sub": ("#FFFFFF", "#666666"),
    "note": ("#FFF9C4", "#C0C0C0"),
}
TYPE_ALIAS = {
    "process": "process", "步骤": "process", "流程": "process", "处理": "process", "task": "process",
    "start": "start", "开始": "start", "起止": "start",
    "end": "end", "结束": "end", "终止": "end",
    "decision": "decision", "判断": "decision", "条件": "decision", "分支": "decision",
    "data": "data", "数据": "data", "输入输出": "data", "io": "data", "文档": "data",
    "sub": "sub", "子流程": "sub", "预定义": "sub", "subprocess": "sub",
    "note": "note", "注释": "note", "备注": "note",
}


def read_csv(p):
    with open(p, newline="", encoding="utf-8-sig") as f:
        rows = [{k.strip(): (v or "").strip() for k, v in r.items() if k} for r in csv.DictReader(f)]
    return [r for r in rows if any(r.values())]


def ntype(t):
    return TYPE_ALIAS.get((t or "").strip().lower(), "process")


def char_w(ch):
    return 1.0 if ord(ch) > 127 else 0.56


def text_w(s):
    return sum(char_w(c) for c in s)


def wrap(label, maxw, fs=12):
    """按像素宽度折行；优先在空格/斜杠处断，实在不行才硬断。"""
    limit = maxw / fs
    lines, cur = [], ""
    for token in _tokens(label):
        if text_w(cur + token) <= limit:
            cur += token
            continue
        if cur:
            lines.append(cur.rstrip())
            cur = ""
        tok = token.lstrip()
        while text_w(tok) > limit:
            acc = ""
            for c in tok:
                if text_w(acc + c) > limit:
                    break
                acc += c
            lines.append(acc)
            tok = tok[len(acc):]
        cur = tok
    if cur.strip():
        lines.append(cur.rstrip())
    return lines or [""]


def _tokens(label):
    """切分为不可再分的片段：ASCII 单词、中文单字、分隔符跟随前段。"""
    out, buf = [], ""
    for ch in label:
        if ord(ch) > 127:
            if buf:
                out.append(buf)
                buf = ""
            out.append(ch)
        elif ch in " ":
            out.append(buf + ch)
            buf = ""
        else:
            buf += ch
    if buf:
        out.append(buf)
    return out


def layout(nodes, edges):
    ids = [n["id"] for n in nodes]
    rank = {i: 0 for i in ids}
    for _ in range(len(ids) + 1):
        changed = False
        for e in edges:
            s, t = e.get("source"), e.get("target")
            if s in rank and t in rank and rank[t] < rank[s] + 1:
                rank[t] = rank[s] + 1
                changed = True
        if not changed:
            break
    slots = {}
    for n in nodes:
        slots.setdefault(rank[n["id"]], []).append(n["id"])
    centers = {}
    for r, group in slots.items():
        for k, nid in enumerate(group):
            centers[nid] = (MARGIN + HALF_W + r * COL_W, MARGIN + 70 + k * ROW_H)
    return centers, rank, slots


def build(dirpath):
    global ROW_H, COL_W
    if dirpath.name in ROW_OVERRIDE:
        ROW_H = ROW_OVERRIDE[dirpath.name]
    nodes = read_csv(dirpath / "nodes.csv")
    edges = read_csv(dirpath / "edges.csv")
    centers, rank, slots = layout(nodes, edges)
    byid = {n["id"]: n for n in nodes}

    # 通道内每条边分配一个竖直位置：按 (源 y, 目标 y) 排序，减少交叉
    groups = {}
    for e in edges:
        s, t = e.get("source"), e.get("target")
        if s not in centers or t not in centers or rank[s] >= rank[t]:
            continue
        groups.setdefault(rank[s], []).append(e)
    lane = {}
    for r, es in groups.items():
        es.sort(key=lambda e: (centers[e["source"]][1] + centers[e["target"]][1]))
        gl = MARGIN + HALF_W + r * COL_W + HALF_W + 8
        gr = MARGIN + HALF_W + (r + 1) * COL_W - HALF_W - 8
        n = len(es)
        for i, e in enumerate(es):
            lane[(e["source"], e["target"])] = gl + (gr - gl) * (i + 0.5) / n

    width = MARGIN + 2 * HALF_W + max(rank.values()) * COL_W + 60
    height = MARGIN + 70 + max(len(g) for g in slots.values()) * ROW_H
    out = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{int(width)}" height="{int(height)}" '
        f'viewBox="0 0 {int(width)} {int(height)}" '
        f'font-family="Microsoft YaHei, PingFang SC, Segoe UI, sans-serif">',
        '<defs><marker id="ar" markerWidth="9" markerHeight="9" refX="7.5" refY="3" orient="auto">'
        '<path d="M0,0 L7.5,3 L0,6 z" fill="#333"/></marker></defs>',
        f'<rect width="100%" height="100%" fill="#ffffff"/>',
    ]
    boxes = {}
    for n in nodes:
        nid, cx, cy = n["id"], *centers[n["id"]]
        t = ntype(n.get("type"))
        h = HEIGHT.get(t, 76)
        box = (cx - HALF_W, cy - h / 2, HALF_W * 2, h)
        boxes[nid] = box
        fill, stroke = FILL.get(t, FILL["process"])
        x, y, w, hh = box
        if t in ("start", "end"):
            shape = (f'<ellipse cx="{cx}" cy="{cy}" rx="{HALF_W}" ry="{h / 2}" '
                     f'fill="{fill}" stroke="{stroke}" stroke-width="1.4"/>')
        elif t == "decision":
            shape = (f'<polygon points="{cx},{y} {x + w},{cy} {cx},{y + hh} {x},{cy}" '
                     f'fill="{fill}" stroke="{stroke}" stroke-width="1.4"/>')
        elif t == "data":
            sk = hh * 0.22
            shape = (f'<polygon points="{x + sk},{y} {x + w},{y} {x + w - sk},{y + hh} {x},{y + hh}" '
                     f'fill="{fill}" stroke="{stroke}" stroke-width="1.4"/>')
        else:
            dash = ' stroke-dasharray="5 4"' if t == "sub" else ""
            shape = (f'<rect x="{x}" y="{y}" width="{w}" height="{hh}" rx="6" fill="{fill}" '
                     f'stroke="{stroke}" stroke-width="1.4"{dash}/>')
        out.append(shape)
        maxw = (w - 26) if t != "decision" else 118
        lines = wrap(n.get("label", ""), maxw)
        step = 15
        top = cy - (len(lines) - 1) * step / 2 + 4
        for i, ln in enumerate(lines):
            out.append(f'<text x="{cx}" y="{top + i * step:.1f}" font-size="12" fill="#1a1a1a" '
                       f'text-anchor="middle">{html.escape(ln)}</text>')

    for e in edges:
        s, t = e.get("source"), e.get("target")
        if s not in boxes or t not in boxes or rank[s] >= rank[t]:
            continue
        sb, tb = boxes[s], boxes[t]
        ys, yt = centers[s][1], centers[t][1]
        cxr = lane[(s, t)]
        x1, x2 = sb[0] + sb[2], tb[0]
        pts = [(x1, ys), (cxr, ys), (cxr, yt), (x2, yt)]
        out.append('<polyline points="{}" fill="none" stroke="#333" stroke-width="1.2" '
                   'marker-end="url(#ar)"/>'.format(" ".join(f"{px:.1f},{py:.1f}" for px, py in pts)))
        if e.get("label"):
            out.append(f'<text x="{x2 - 6:.1f}" y="{yt - 7:.1f}" font-size="10.5" fill="#555" '
                       f'text-anchor="end">{html.escape(e["label"])}</text>')
    out.append("</svg>")
    (dirpath / "diagram.svg").write_text("\n".join(out), encoding="utf-8")
    print(f"OK {dirpath.name}: nodes={len(nodes)} edges={len(edges)} size={int(width)}x{int(height)}")


if __name__ == "__main__":
    for d in sys.argv[1:]:
        build(Path(d))
