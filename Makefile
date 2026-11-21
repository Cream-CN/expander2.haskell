# =============================================================================
# expander2.haskell - GNU make 入口（POSIX sh：WSL / Git Bash / MSYS2）
#
#   make                      -> .build/expander2-hs            (profile extreme，默认最强劲)
#   make shell                -> .build-shell/expander2-shell
#   make PROFILE=strong       -> .build-strong/expander2-hs     (日常增量编译更快)
#   make PROFILE=fast|strong|extreme   便捷目标，等价于 PROFILE=<x> all
#   make JOBS=6               -> ghc -j6；JOBS=0 表示不传 -j
#   make list | test | info | clean | clean-cmake | help
#
# 优化参数的真源是 ghc-flags.mk（build.ps1 / CMakeLists.txt 读同一份），
# GHC 路径也来自那里的 GHC_HOME（D:/sdk/ghc/ghcup/bin），找不到才退回 PATH。
# 原生 Windows 的 cmd/PowerShell 请直接用 .\build.ps1，不要再套 make。
# 按项目要求：任何 profile 都不含 -march=native / -mcpu=native。
#
# 说明：本 Makefile 不枚举 .hs 依赖，每次交给 ghc --make 自己判断增量
#       （GHC 的重编译检查基于 .hi 时间戳，比 make 猜依赖更准）。
# =============================================================================

include ghc-flags.mk

# GHC：优先用 GHC_HOME 下的可执行文件，不存在就退回 PATH 里的 ghc
GHC ?= $(shell if [ -x "$(GHC_HOME)/ghc.exe" ]; then echo "$(GHC_HOME)/ghc.exe"; \
              elif [ -x "$(GHC_HOME)/ghc" ]; then echo "$(GHC_HOME)/ghc"; \
              else command -v ghc; fi)

PROFILE ?= $(DEFAULT_PROFILE)
JOBS    ?= 4
RTSOPTS ?= -A64m
LLVM    ?= auto

ifeq ($(OS),Windows_NT)
EXE := .exe
else
EXE :=
endif

ifeq ($(filter $(PROFILE),fast strong extreme compat),)
$(error PROFILE must be fast/strong/extreme/compat, got '$(PROFILE)')
endif

# 默认档沿用 .build / .build-shell（对拍脚本认这个路径），其它档各自独立目录，
# 这样换档不会把不同参数编出来的 .hi/.o 混在一起。
SUFFIX   := $(if $(filter $(DEFAULT_PROFILE),$(PROFILE)),,-$(PROFILE))
OUTDIR   := .build$(SUFFIX)
SHELLDIR := .build-shell$(SUFFIX)
BIN      := $(OUTDIR)/expander2-hs$(EXE)
SHELLBIN := $(SHELLDIR)/expander2-shell$(EXE)

OPTFLAGS := $(PROFILE_$(shell echo $(PROFILE) | tr a-z A-Z))

# 注意：LLVM 后端的 flag 是 -fllvm；写成 -llvm 会被原样交给链接器当 -lvm 用。
ifeq ($(LLVM),auto)
LLVMFLAGS := $(if $(and $(shell command -v llc 2>/dev/null),$(shell command -v opt 2>/dev/null)),-fllvm -optlo=-O3 -optlc=-O3,)
else ifeq ($(LLVM),on)
LLVMFLAGS := -fllvm -optlo=-O3 -optlc=-O3
else ifeq ($(LLVM),off)
LLVMFLAGS :=
else
$(error LLVM must be auto/on/off, got '$(LLVM)')
endif

# 兼容档固定使用 GHC 内置 NCG 后端
ifeq ($(PROFILE),compat)
LLVMFLAGS :=
endif

BASE := -isrc -iapp -rtsopts
ifneq ($(RTSOPTS),)
BASE += -with-rtsopts=$(RTSOPTS)
endif
ifneq ($(JOBS),0)
BASE += -j$(JOBS)
endif

ALLFLAGS := $(OPTFLAGS) $(LLVMFLAGS) $(BASE) --make

# 参数戳：只记会影响 .hi/.o 与链接结果的部分（-j 并行度不算）
STAMP := $(OPTFLAGS) $(LLVMFLAGS) $(BASE)

.PHONY: all main shell fast strong extreme list test info clean clean-cmake help \
        $(OUTDIR)/.ghcflags $(SHELLDIR)/.ghcflags

all: main

main: $(OUTDIR)/.ghcflags
	$(GHC) $(ALLFLAGS) -outputdir $(OUTDIR) -o $(BIN) app/Main.hs

shell: $(SHELLDIR)/.ghcflags
	$(GHC) $(ALLFLAGS) -outputdir $(SHELLDIR) -o $(SHELLBIN) app/Shell.hs

# flags 变了（或目录里没有戳）就清目录重编，避免不同优化级别的 .hi/.o 混用
$(OUTDIR)/.ghcflags: ghc-flags.mk
	@mkdir -p $(OUTDIR)
	@t=$$(mktemp) && printf '%s\n' '$(STAMP)' > $$t && \
	  if [ -f $@ ] && cmp -s $$t $@; then \
	    rm -f $$t; \
	  else \
	    echo "[clean] flags changed -> wiping $(OUTDIR)"; \
	    rm -rf $(OUTDIR); mkdir -p $(OUTDIR); mv $$t $@; \
	  fi

$(SHELLDIR)/.ghcflags: ghc-flags.mk
	@mkdir -p $(SHELLDIR)
	@t=$$(mktemp) && printf '%s\n' '$(STAMP)' > $$t && \
	  if [ -f $@ ] && cmp -s $$t $@; then \
	    rm -f $$t; \
	  else \
	    echo "[clean] flags changed -> wiping $(SHELLDIR)"; \
	    rm -rf $(SHELLDIR); mkdir -p $(SHELLDIR); mv $$t $@; \
	  fi

fast strong extreme:
	@$(MAKE) --no-print-directory PROFILE=$@ all

list: main
	./$(BIN) --list

test: main
	./test/run_all.sh

info:
	@echo "ghc     : $(GHC)"
	@echo "profile : $(PROFILE)"
	@echo "flags   : $(ALLFLAGS)"
	@echo "outdir  : $(OUTDIR)$(if $(filter shell,$(MAKECMDGOALS)), $(SHELLDIR),)"
	@echo "llvm    : $(LLVM)$(if $(LLVMFLAGS), -> on, -> off)"
	@echo "rtsopts : $(RTSOPTS)"
	@$(GHC) --version

clean:
	rm -rf .build .build-*

clean-cmake:
	rm -rf build-cmake

help:
	@echo "make                     extreme -> .build/expander2-hs   (最强劲，默认)"
	@echo "make PROFILE=strong      strong  -> .build-strong/expander2-hs"
	@echo "make PROFILE=fast        fast    -> .build-fast/expander2-hs"
	@echo "make PROFILE=compat      compat  -> .build-compat/expander2-hs (仅 -O2 + GHC NCG 后端)"
	@echo "make shell               shell   -> .build-shell/expander2-shell"
	@echo "make JOBS=6              parallel compile (-j6; JOBS=0 disables)"
	@echo "make LLVM=off|on|auto    LLVM backend switch"
	@echo "make list|test|info      run/debug helpers"
	@echo "make clean|clean-cmake   drop build dirs"
