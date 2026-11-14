#!/bin/sh
# 编译单族探针：./test/build_group.sh Mrss  ->  .build-mrss/probe
set -e
G=$(echo "$1" | tr 'A-Z' 'a-z')
cd "$(dirname "$0")/.."
mkdir -p ".build-$G"
ghc -O1 -isrc -iapp -outputdir ".build-$G" -o ".build-$G/probe" "app/Probe$1.hs" 2>&1 | grep -v '^\s*$' || true
test -x ".build-$G/probe" && echo "built: .build-$G/probe"
