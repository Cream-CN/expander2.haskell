#!/bin/sh
# 按 UI 合法性规则过滤全部族的语料，产物在 test/corpus/legal/。
# 用法： ./test/filter_all.sh            全部族
#        ./test/filter_all.sh Ton Mrss  指定族
set -u
cd "$(dirname "$0")/.." || exit 1
BIN=${BIN:-.build/expander2-hs}
GROUPS=${*:-"Bms Mrss Bmlike Gms Tree M1ynss Den Ton Mn Upmn Dbms Aw2mn"}
for G in $GROUPS; do
  LOW=$(echo "$G" | tr 'A-Z' 'a-z')
  CORP="test/corpus/$LOW.txt"
  if [ ! -f "$CORP" ]; then echo "[$G] NO_CORPUS"; continue; fi
  PROBE=".build-$LOW/probe"
  if [ -x "$PROBE" ]; then
    IDS=$("$PROBE" --list | cut -f1 | paste -sd, -)
  else
    IDS=$(./.build/expander2-hs --list | cut -f1 | paste -sd, -)
  fi
  echo "[$G] ids=$IDS"
  python3 test/filter_legal.py --bin "$BIN" --ids="$IDS" --corpus "$CORP" \
      --out "test/corpus/legal/$LOW.txt" --terms "${TERMS:-1}" --timeout "${TIMEOUT:-4}" \
      ${EXTRA:-} 2>&1 | tail -4
done
