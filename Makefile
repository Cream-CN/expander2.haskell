# =============================================================================
# expander2.haskell - GNU make 入口（POSIX sh：WSL / Git Bash / MSYS2）
#
#   make                      -> .build/expander2-hs          （只建这一个 CLI）
#   make PROFILE=fast         -> 换优化档（fast / strong / extreme / compat）
#   make JOBS=6               -> ghc -j6；JOBS=0 表示不传 -j
#   make list | info | clean | help
#
# 优化参数的真源是 ghc-flags.mk（build.ps1 读同一份），GHC 路径也来自那里的
# GHC_HOME，找不到才退回 PATH。原生 Windows 的 cmd/PowerShell 直接用 .\build.ps1。
# 按项目要求：任何 profile 都不含 -march=native / -mcpu=native。
# =============================================================================

include ghc-flags.mk

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

OUTDIR := .build
BIN    := $(OUTDIR)/expander2-hs$(EXE)
OPTFLAGS := $(PROFILE_$(shell echo $(PROFILE) | tr a-z A-Z))

ifeq ($(LLVM),auto)
LLVMFLAGS := $(if $(and $(shell command -v llc 2>/dev/null),$(shell command -v opt 2>/dev/null)),-fllvm -optlo=-O3 -optlc=-O3,)
else ifeq ($(LLVM),on)
LLVMFLAGS := -fllvm -optlo=-O3 -optlc=-O3
else ifeq ($(LLVM),off)
LLVMFLAGS :=
else
$(error LLVM must be auto/on/off, got '$(LLVM)')
endif

ifeq ($(PROFILE),compat)
LLVMFLAGS :=
endif

ALLFLAGS = $(OPTFLAGS) $(LLVMFLAGS) $(BASE) --make

BASE := -isrc -iapp -rtsopts
ifneq ($(RTSOPTS),)
BASE += -with-rtsopts=$(RTSOPTS)
endif
ifneq ($(JOBS),0)
BASE += -j$(JOBS)
endif

.PHONY: all fast strong extreme list info clean help

all:
	@mkdir -p $(OUTDIR)
	$(GHC) $(ALLFLAGS) -outputdir $(OUTDIR) -o $(BIN) app/Main.hs

fast strong extreme:
	@$(MAKE) --no-print-directory PROFILE=$@ all

list: all
	./$(BIN) --list

info:
	@echo "ghc     : $(GHC)"
	@echo "profile : $(PROFILE)"
	@echo "flags   : $(ALLFLAGS)"
	@echo "outdir  : $(OUTDIR)"
	@echo "llvm    : $(LLVM)$(if $(LLVMFLAGS), -> on, -> off)"
	@$(GHC) --version

clean:
	rm -rf .build

help:
	@echo "make                     extreme -> .build/expander2-hs   (最强劲，默认)"
	@echo "make PROFILE=strong      strong  -> .build/expander2-hs"
	@echo "make PROFILE=fast        fast    -> .build/expander2-hs"
	@echo "make PROFILE=compat      compat  -> .build/expander2-hs (仅 -O2 + GHC NCG 后端)"
	@echo "make JOBS=6              parallel compile (-j6; JOBS=0 disables)"
	@echo "make LLVM=off|on|auto    LLVM backend switch"
	@echo "make list|info           run/debug helpers"
	@echo "make clean               drop .build"
