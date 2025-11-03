#include <iostream>
#include <stack>

#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>

using namespace accat::auxilia;

int main() {
  auto nfa = NFA::FromRegex("b(a|b)*abb");
  std::cout << nfa;

  // test strings
  std::cout << "\nTesting strings:\n";
  std::vector<std::string> test_strings = {
      "abb", "aabb", "babb", "ababb", "baabb", "aaabb", "ab", "a", "b", ""};
  for (const auto &str : test_strings) {
    std::cout << "\"" << str << "\": " << (nfa.test(str) ? "MATCH" : "NO MATCH")
              << "\n";
  }
  nfa = NFA::FromRegex("");
  std::cout << nfa;
  nfa.test("a");
  nfa.test("");
}
