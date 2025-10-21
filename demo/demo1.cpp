#include <array>
#include <bitset>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <print>
#include <string>
#include <type_traits>
#include "accat/auxilia/details/chars.hpp"
#include "accat/auxilia/defines.hpp"
// test
constexpr auto t(auto &&arr) {
  return typeid(static_cast<decltype(arr) &&>(arr)).name();
}
int main() {
  using namespace accat::auxilia;
  using namespace accat::auxilia::literals;
}
