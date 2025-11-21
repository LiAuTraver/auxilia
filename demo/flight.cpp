#include <iostream>
#include <memory>
#include <ranges>
#include <unordered_map>
#include <string>
#include <optional>

#include <accat/auxilia/details/Trie.hpp>
using namespace accat::auxilia;

int main() {
  auto v = {1, 2, 3};
  for (auto &&p : std::ranges::views::cartesian_product(v, v)) {
    //(1, 1) (1, 2) (1, 3) (2, 1) (2, 2) (2, 3) (3, 1) (3, 2) (3, 3)
    Println("{}", p);
  }
  // I want: (1,1) (1,2) (1,3) (2,2) (2,3) (3,3)
}
