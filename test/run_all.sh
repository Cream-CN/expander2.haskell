#!/bin/sh
# 集成复跑：逐族编译探针并与 C++ 预言机逐字节差分。
# 用法： ./test/run_all.sh            全部族
#         ./test/run_all.sh Mrss Gms   指定族
set -u
cd "$(dirname "$0")/.." || exit 1
GROUPS=${*:-"Bms Mrss Bmlike Gms Tree M1ynss Den Ton Mn Upmn Dbms Aw2mn"}
fail=0
for G in $GROUPS; do
  LOW=$(echo "$G" | tr 'A-Z' 'a-z')
  CORP="test/corpus/$LOW.txt"
  ./test/build_group.sh "$G" >/dev/null 2>&1
  BIN=".build-$LOW/probe"
  if [ ! -x "$BIN" ]; then echo "[$G] BUILD_FAIL"; fail=1; continue; fi
  IDS=$("$BIN" --list | cut -f1 | paste -sd, -)
  if [ -z "$IDS" ]; then echo "[$G] NO_NOTATIONS"; fail=1; continue; fi
  if [ ! -f "$CORP" ]; then echo "[$G] NO_CORPUS $CORP"; fail=1; continue; fi
  case $G in
    M1ynss) PARAMS="--params 0,1,2,25,26" ;;
    *) PARAMS="" ;;
  esac
  OUT=$(python3 test/diff.py --bin "$BIN" --ids="$IDS" --corpus "$CORP" \
        --terms 0,1,2,3 --modes fs,alter $PARAMS 2>&1 | tail -1)
  echo "[$G] $OUT"
  case "$OUT" in *"mismatch=0"*) ;; *) fail=1 ;; esac
done
if [ $fail -eq 0 ]; then echo "ALL_GROUPS_ZERO_DIFF"; else echo "HAS_MISMATCH"; fi
exit $fail
