#include <cstdlib>
#include <fstream>
#include <iostream>
#include <stack>

#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>

using namespace accat::auxilia;

int main() {
  auto nfa = *NFA::FromRegex("a(b|a)*b");
  std::cout << nfa;

  // test strings
  std::cout << "\nTesting strings:\n";
  std::vector<std::string> test_strings = {
      "abb", "aabb", "babb", "ababb", "baabb", "aaabb", "ab", "a", "b", ""};
  for (const auto &str : test_strings) {
    std::cout << "\"" << str << "\": " << (nfa.test(str) ? "MATCH" : "NO MATCH")
              << "\n";
  }
  std::ofstream("nfa.dot") << nfa.to_dot();
  system("(wsl -- dot -Tpng nfa.dot -o nfa.png) && nfa.png");
}
