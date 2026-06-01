#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stack>

#include <auxilia/auxilia.hpp>
#include <auxilia/defines.hpp>

#include "Automaton.hpp"
#include "NFA.hpp"
#include "DFA.hpp"

using namespace auxilia;
using namespace accat::cp;

int main() {
  auto dfa = NFA::FromRegex("a|ba").and_then(DFA::FromNFA).value();

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

  std::ofstream("dfa.dot") << dfa.to_dot();
  system("dot -Tpng dfa.dot -o dfa.png");
  system("dfa.png");
}
