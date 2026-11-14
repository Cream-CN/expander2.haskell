#!/bin/sh
cd "$(dirname "$0")/.." || exit 1
HS=./.build/expander2-hs
REF=/home/user/Doubao/chats/38453110731617282/.tools/oracle/ref
: > /tmp/ydiff.log
cases=0
for t in "1,2" "1,2,3" "1,2,4" "1,2,3,4" "2,3" "1,1,2" "1,2,2,3" "1,3,4" "2,2,3" "1,1,1,2"; do
  for m in fs alter; do
    for k in 1 2 3; do
      for id in omega-y-medium omega-y-strong; do
        a=$(printf '%s' "$t" | "$REF" "$id" "$m" "$k" 2>&1)
        b=$(printf '%s' "$t" | "$HS" "$id" "$m" "$k" 2>&1)
        cases=$((cases+1))
        [ "$a" != "$b" ] && printf 'DIFF %s [%s] %s %s\n  cpp=%s\n  hs =%s\n' "$id" "$t" "$m" "$k" "$a" "$b" >> /tmp/ydiff.log
      done
    done
  done
done
echo "cases=$cases"
cat /tmp/ydiff.log
