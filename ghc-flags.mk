# =============================================================================
# expander2.haskell / GHC 优化参数 —— 唯一真源（single source of truth）
#
#   * Makefile        : include 本文件，直接使用下面的变量
#   * build.ps1       : 用正则解析 DEFAULT_PROFILE / GHC_HOME / PROFILE_*
#   * CMakeLists.txt  : 用正则解析 DEFAULT_PROFILE / GHC_HOME / PROFILE_*
#
# 改这里 -> 三套构建入口（PowerShell / make / cmake）同时生效。
# 不要在任何入口脚本里另抄一份参数，否则三方迟早对不上。
#
# 约定（GHC 9.14.1 上已逐条实测可用）：
#   * GHC 没有 -O3，最高标准优化等级是 -O2（-O 等价 -O1，-O0 关优化）；
#   * -fsimplifier-phases=4 / -fmax-simplifier-iterations 必须写在 -O2 之后，
#     否则会被 -O2 压回默认值；
#   * 按项目要求禁用 -march=native / -mcpu=native，任何 profile 里都不许出现
#     （build.ps1 会主动拦截这两个 flag）；
#   * LLVM 后端（-fllvm -optlo/-optlc）不写在这里，由各入口的 LLVM 开关追加。
# =============================================================================

DEFAULT_PROFILE := extreme

# GHC 安装位置（三个入口都读这一行；不存在时各脚本自动退回 PATH 里的 ghc）
GHC_HOME := D:/sdk/ghc/ghcup/bin

# 快速档：只开 GHC 标准优化，适合日常增量编译 / 改一行就看结果
PROFILE_FAST    := -O2

# 兼容档：只用 GHC 自己的标准优化（任何 GHC 版本都认），且强制走内置 NCG 后端，
# 不追加任何激进 -f 开关、不启用 LLVM —— 换机器 / 换 GHC 版本 / 排错时用这一档。
PROFILE_COMPAT  := -O2

# 强档：标准优化 + 4 轮化简 + 静态参数变换 + 严格字段拆箱
PROFILE_STRONG  := -O2 -fsimplifier-phases=4 -fstatic-argument-transformation -funbox-strict-fields

# 极档（默认）：强档 + 更多化简迭代 + 跨模块激进特化 + 末轮严格性分析
#   -fexpose-all-unfoldings  导出所有 unfolding，跨模块内联/特化才吃得饱（编译变慢、内存变高）
#   -fspecialise-aggressively 不限特化规模；-flate-specialise 特化后再跑一遍
#   -flate-dmd-anal          末轮严格性分析，把进阶优化后的惰性再收紧一次
#   -fspec-constr-count=10   构造器特化配额放大
#   -fworker-wrapper-cbv     worker/wrapper 传值拆箱，减少装箱分配
PROFILE_EXTREME := -O2 -fsimplifier-phases=4 -fmax-simplifier-iterations=20 -fstatic-argument-transformation -funbox-strict-fields -fspecialise-aggressively -flate-specialise -fexpose-all-unfoldings -flate-dmd-anal -fspec-constr-count=10 -fworker-wrapper-cbv
