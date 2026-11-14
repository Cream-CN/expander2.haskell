#!/bin/sh
# 最终验收：逐族编译 + 用各族完整语料逐 id 差分，合并成 coverage.json。
set -u
cd "$(dirname "$0")/.." || exit 1
rm -f /tmp/cov_*.json
GROUPS="Bms Mrss Bmlike Gms Tree M1ynss Den Ton Mn Upmn Dbms Aw2mn"
for G in $GROUPS; do
  LOW=$(echo "$G" | tr 'A-Z' 'a-z')
  ./test/build_group.sh "$G" >/dev/null 2>&1
  BIN=".build-$LOW/probe"
  if [ ! -x "$BIN" ]; then echo "[$G] BUILD_FAIL"; continue; fi
  IDS=$("$BIN" --list | cut -f1 | paste -sd, -)
  if [ -z "$IDS" ]; then echo "[$G] NO_NOTATIONS"; continue; fi
  if [ "$G" = "Mrss" ]; then
    CORP="test/corpus/mrss_seq.txt"
  else
    CORP="test/corpus/$LOW.txt"
  fi
  if [ ! -f "$CORP" ]; then echo "[$G] NO_CORPUS $CORP"; continue; fi
  case $G in
    M1ynss) P="--params 0,1,2,25,26" ;;
    *) P="" ;;
  esac
  OUT=$(python3 test/diff.py --bin "$BIN" --ids="$IDS" --corpus "$CORP" \
        --terms 0,1,2,3 --modes fs,alter $P --json "/tmp/cov_$LOW.json" 2>&1 | tail -1)
  echo "[$G] $OUT"
done
python3 test/merge_cov.py /tmp/cov_*.json coverage.json
