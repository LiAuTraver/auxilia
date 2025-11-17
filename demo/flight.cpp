#include <iostream>
#include <ostream>
#include <vector>
#include <ranges>
#include <algorithm>

int main() {
  std::vector<int> v1 = {1, 2, 3, 4, 5, 6};
  std::vector<int> v2 = {1, 2, 4, 5, 6, 8};

  auto [it1, it2] = std::ranges::mismatch(v1, v2);

  // Extract matching prefix from v1.begin() to it1
  std::vector<int> v_matched(v1.begin(), it1);

  std::cout << "Matched elements: ";
  for (auto elem : v_matched) {
    std::cout << elem << " ";
  }
  std::cout << std::endl;
}
