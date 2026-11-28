#!/bin/sh
# 重建 C++ 差分预言机（源码取自 $EXPANDER_SRC，默认 ../Expander-Dev，只读）。
# 约 35s。产物：test/oracle/ref
set -e
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
SRC=${EXPANDER_SRC:-$ROOT/Expander-Dev}
if [ ! -d "$SRC/Header" ] && [ -d "$ROOT/expander2/Header" ]; then
  SRC="$ROOT/expander2"
fi
WORK=${TMPDIR:-/tmp}/oracle
rm -rf "$WORK"; mkdir -p "$WORK"
cp -r "$SRC/Header" "$SRC/Resource.h" "$WORK/"
# patch: std::views::split 在 g++11 不可用，改为手工切分（语义等价）
python3 - "$WORK/Header/common/sequence.hpp" <<'PY'
import sys
p=sys.argv[1]; s=open(p).read()
old="""    std::vector<int> seq;
    for (const auto& part : str | std::views::split(',')) {
        std::string token{ part.begin(), part.end() };
        token.erase(std::remove_if(token.begin(), token.end(),
            [](unsigned char c) { return std::isspace(c); }), token.end());
        if (token.empty()) continue;
        try { seq.push_back(std::stoi(token)); } catch (...) {}
    }
    return seq;"""
new="""    std::vector<int> seq;
    std::size_t start__ = 0;
    for (;;) {
        std::size_t pos__ = str.find(',', start__);
        std::string token{ str.substr(start__,
            (pos__ == std::string_view::npos ? str.size() : pos__) - start__) };
        token.erase(std::remove_if(token.begin(), token.end(),
            [](unsigned char c) { return std::isspace(c); }), token.end());
        if (!token.empty()) { try { seq.push_back(std::stoi(token)); } catch (...) {} }
        if (pos__ == std::string_view::npos) break;
        start__ = pos__ + 1;
    }
    return seq;"""
assert old in s
s=s.replace(old,new)
# 语义等价：views::split 保留尾随空段（空 token 被跳过），此处逐段 substr
open(p,'w').write(s)
PY
ln -sf MrSS121.hpp "$WORK/Header/notation/mrss121.hpp"
cp "$(dirname "$0")/oracle/ref.cpp" "$WORK/ref.cpp"
g++ -std=c++20 -O2 -w -DNDEBUG -I"$WORK/Header" -I"$WORK" "$WORK/ref.cpp" -o "$(dirname "$0")/oracle/ref"
# UI 级预言机（含 Expander.cpp 的前置校验分支）
cp "$(dirname "$0")/oracle/refui.cpp" "$WORK/refui.cpp"
g++ -std=c++20 -O2 -w -DNDEBUG -I"$WORK/Header" -I"$WORK" "$WORK/refui.cpp" -o "$(dirname "$0")/oracle/refui"
echo "oracle built: $WORK -> $(dirname "$0")/oracle/ref"
