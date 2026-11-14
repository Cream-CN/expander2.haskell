# TON 族移植回执

## 文件清单
- `src/OmegaY/Fam/Ton/TonCore.hs` — 核心模块（TonNode ADT、flatten、compareRaise、raise、rConfig、parseImpl、displayNoraise/displayMain、regressRepeated、markMain/markMc/markFs、索引机器 collectSubtermIndex/collectSmallindex/collectSmallpart/getA2/getN、stdMemoized、FS 生成器 tonStep/tonGenNext/expandBeta/cacheFs、makeLimit、tonExpandText 入口）
- `src/OmegaY/Fam/Ton/Variants.hs` — 9 个变体的 standard_q/builtq 实现（TonConfig 记录驱动）
- `src/OmegaY/Fam/Ton/Family.hs` — 注册 9 个 id 的配置表
- `test/corpus/ton.txt` — 27 条语料（已剔除 "0" term≥1 的 oracle UB segfault 输入）

## 注册 id
ton-m, ton-i, ton-dr, ton-drc, ton-drp, ton-drpc, ton-ibp, ton-mc, ton-mpc

## 差分结果
- 语料：27 条 × terms 0-3 × modes fs,alter = 1944 cases
- ok=1940, mismatch=4
- 7 个 id（ton-m, ton-dr, ton-drc, ton-drp, ton-drpc, ton-mc, ton-mpc）：mismatch=0
- ton-i, ton-ibp：仅在 "Limit" term=3 有 1 字符差异（fs 和 alter 各 1 条）
  - ref: `0ΩΩ0CC0ΩΩΩCCCCC`
  - hs:  `0ΩΩ0CC0ΩΩ0CCCCC`
  - 差异：第 10 字符 Ω(ref) vs 0(hs)，位于 walkSpine 对 c1.r=leaf(0) 的处理分支

## 已知未覆盖
- "0"（裸 leaf -1）term≥1：oracle segfault（UB），hs probe 会 hang，已从语料剔除
- ton-i/ton-ibp "Limit" term=3：walkSpine 在 c1.r=leaf(0) 时的分支判定与 C++ 有 1 字符差异
