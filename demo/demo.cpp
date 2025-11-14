#include <algorithm>
#include <atomic>
#include <iostream>
#include <ostream>
#include <string>
#include <thread>
#include <vector>
#include <ranges>

#include "accat/auxilia/auxilia.hpp"

using namespace accat::auxilia::ranges::views;
int main() {
  std::vector<int> v{2, 3, 4};

  // Direct construction still works
  // auto r1 = surround_view(v, 1, 5);

  // Pipeline syntax now works too!
  auto r2 =
      v                                                                       //
      | std::views::transform([](auto &&i) { return std::to_string(i * 2); }) //
      | surround("1", "5")                                                    //
      | std::ranges::views::join                                              //
      ;

  for (auto &&x : r2)
    std::cout << x << " "; // prints 1 2 3 4 5
}
