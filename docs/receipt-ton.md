# TON 族移植回执

## 文件清单
- `src/OmegaY/Fam/Ton/TonCore.hs` — 核心模块（TonNode ADT、flatten、compareRaise、raise、rConfig、parseImpl、displayNoraise/displayMain、regressRepeated、markMain/markMc/markFs、索引机器 collectSubtermIndex/collectSmallindex/collectSmallpart/getA2/getN、stdMemoized、FS 生成器 tonStep/tonGenNext/expandBeta/cacheFs、makeLimit、tonExpandText 入口）
- `src/OmegaY/Fam/Ton/Variants.hs` — 9 个变体的 standard_q/builtq 实现（TonConfig 记录驱动）
- `src/OmegaY/Fam/Ton/Family.hs` — 注册 9 个 id 的配置表
- `test/corpus/ton.txt` — 27 条语料（已剔除 "0" term≥1 的 oracle UB segfault 输入）

## 注册 id
ton-m, ton-i, ton-dr, ton-drc, ton-drp, ton-drpc, ton-ibp, ton-mc, ton-mpc

## 差分结果

9 个 id（`ton-m`、`ton-i`、`ton-dr`、`ton-drc`、`ton-drp`、`ton-drpc`、`ton-ibp`、`ton-mc`、`ton-mpc`）
全部 `bad=0`，逐 id 数据见 `coverage.json`。

## 语料口径

- `test/corpus/ton.txt` 已剔除会让 C++ 预言机 segfault 的输入（`"0"` 裸 leaf、term≥1）。
