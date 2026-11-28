#include <iostream>
#include <iterator>
#include <string>
#include <string_view>
#include "notation/registry.hpp"
int main(int argc, char** argv) {
    using namespace omegay::notation;
    if (argc >= 2 && std::string(argv[1]) == "--list") {
        for (auto& n : registered_notations())
            std::cout << n->id() << "\t" << n->meta().display_name << "\n";
        return 0;
    }
    if (argc < 3) { std::cerr << "usage: ref <id> <term> [param]\n"; return 2; }
    std::string ident = argv[1];
    int term = std::stoi(argv[2]);
    int param = (argc > 3) ? std::stoi(argv[3]) : 0;
    std::string text{std::istreambuf_iterator<char>(std::cin), std::istreambuf_iterator<char>()};
    for (auto& n : registered_notations()) {
        if (std::string(n->id()) == ident) {
            auto r = n->expand(text, term, param);
            if (!r.ok) std::cout << "ERR:" << r.err << "\n";
            else std::cout << r.text << "\n";
            return 0;
        }
    }
    std::cout << "ERR_NO_NOTATION\n";
    return 1;
}
