// UI 级预言机：复刻 Expander.cpp 中 IDM_FS / IDM_FSALTER 三个分支的
// 输入校验（文本型 / 参数化 / 序列型），再做展开。
//
// 与 ref.cpp 的差别：term / param 以**原始字符串**传入，用来复刻
// std::stoi 失败时的「项数必须是整数」提示；校验顺序与 MessageBox 分支
// 完全一致。错误文本里的换行统一转义成字面 "\n"，便于单行对拍。
#include <iostream>
#include <iterator>
#include <string>
#include <string_view>
#include "notation/registry.hpp"

namespace {

std::string esc(const std::string& s) {
    std::string out;
    out.reserve(s.size());
    for (char c : s) {
        if (c == '\n') out += "\\n";
        else if (c == '\\') out += "\\\\";
        else out += c;
    }
    return out;
}

int finish(const omegay::notation::ExpandResult& r) {
    if (!r.ok) std::cout << "ERR:" << esc(r.err) << "\n";
    else std::cout << r.text << "\n";
    return 0;
}

}  // namespace

int main(int argc, char** argv) {
    using namespace omegay::notation;

    if (argc >= 2 && std::string(argv[1]) == "--list") {
        for (auto& n : registered_notations())
            std::cout << n->id() << "\t" << n->meta().display_name
                      << "\t" << (n->has_text() ? "text" : (n->has_text_n() ? "text_n" : "seq"))
                      << "\n";
        return 0;
    }
    if (argc < 4) {
        std::cerr << "usage: refui <id> <fs|alter> <termRaw> [paramRaw]\n";
        return 2;
    }

    const std::string ident = argv[1];
    const std::string modeStr = argv[2];
    const std::string termRaw = argv[3];
    const std::string paramRaw = (argc > 4) ? argv[4] : "0";
    const std::string text{std::istreambuf_iterator<char>(std::cin),
                           std::istreambuf_iterator<char>()};

    const ExpandMode mode =
        (modeStr == "alter") ? ExpandMode::FsAlter : ExpandMode::Fs;

    for (auto& n : registered_notations()) {
        if (std::string(n->id()) != ident) continue;

        // ---- 分支一：文本型记号 ----
        if (n->has_text()) {
            int term = 0;
            try {
                term = std::stoi(termRaw);
            } catch (...) {
                std::cout << "ERR:" << esc("项数必须是整数，\n所以说你为什么要这样输入") << "\n";
                return 0;
            }
            if (term < 1) {
                std::cout << "ERR:"
                          << esc("项数必须为正整数，\n你难道要尝试向内存反方向写入序列吗") << "\n";
                return 0;
            }
            if (text.empty()) {
                std::cout << "ERR:" << esc("序列不能为空") << "\n";
                return 0;
            }
            return finish(n->expand(mode, text, term, 0));
        }

        // ---- 分支二：参数化记号 ----
        if (n->has_text_n()) {
            int term = 0, param = 0;
            try {
                term = std::stoi(termRaw);
                param = std::stoi(paramRaw);
            } catch (...) {
                std::cout << "ERR:" << esc("项数与参数都必须是整数，\n所以说你为什么要这样输入") << "\n";
                return 0;
            }
            if (term < 1) {
                std::cout << "ERR:"
                          << esc("项数必须为正整数，\n你难道要尝试向内存反方向写入序列吗") << "\n";
                return 0;
            }
            const auto& meta = n->meta();
            if (param < meta.param_min || param > meta.param_max) {
                std::cout << "ERR:" << esc("参数必须是 " + std::to_string(meta.param_min) +
                                           " 到 " + std::to_string(meta.param_max) +
                                           " 之间的整数")
                          << "\n";
                return 0;
            }
            if (text.empty()) {
                std::cout << "ERR:" << esc("序列不能为空") << "\n";
                return 0;
            }
            return finish(n->expand(mode, text, term, param));
        }

        // ---- 分支三：序列型记号（readSeqAndTerm）----
        int term = 0;
        try {
            term = std::stoi(termRaw);
        } catch (...) {
            std::cout << "ERR:" << esc("项数必须是整数") << "\n";
            return 0;
        }
        if (term < 1) {
            std::cout << "ERR:" << esc("项数必须为正整数") << "\n";
            return 0;
        }
        if (omegay::common::parse_sequence(text).empty()) {
            std::cout << "ERR:" << esc("序列不能为空") << "\n";
            return 0;
        }
        return finish(n->expand(mode, text, term, 0));
    }

    std::cout << "ERR_NO_NOTATION\n";
    return 1;
}
