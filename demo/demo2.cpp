#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stack>

#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>

using namespace accat::auxilia;

int main() {
  auto dfa =
      NFA::FromRegex("b(a|b)*bab").and_then(&DFA::FromNFA).rvalue().value();

  std::cout << "\nTesting strings:\n";
  auto test_strings = {
      "abb", "aabb", "babb", "ababb", "baabb", "aaabb", "ab", "a", "b", ""};
  for (const auto &str : test_strings) {
    std::cout << std::quoted(str) << ": "
              << (dfa.test(str) ? "MATCH" : "NO MATCH") << "\n";
  }
  std::cout << dfa << "\n";
  dfa.minify();
  std::cout << "After minification:\n";
  std::cout << dfa << "\n";

  // std::ofstream("dfa.dot") << dfa.to_dot();
  // system("(dot -Tpng dfa.dot -o dfa.png) && dfa.png");
}
