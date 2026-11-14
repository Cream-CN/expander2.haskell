# 构建去掉 UI 的计算层 CLI（只依赖 GHC 自带包）
GHC      ?= ghc
GHCFLAGS ?= -O1 -isrc -iapp
OUTDIR   ?= .build
BIN      := $(OUTDIR)/expander2-hs

.PHONY: all clean test list

all: $(BIN)

$(BIN): app/Main.hs $(shell find src -name '*.hs')
	$(GHC) $(GHCFLAGS) -outputdir $(OUTDIR) -o $(BIN) app/Main.hs

list: $(BIN)
	./$(BIN) --list

test: $(BIN)
	./test/run_all.sh

clean:
	rm -rf .build .build-*
