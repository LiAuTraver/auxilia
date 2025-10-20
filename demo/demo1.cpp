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
// test
constexpr auto t(auto &&arr) {
  return typeid(static_cast<decltype(arr) &&>(arr)).name();
}
int main() {
  using namespace accat::auxilia;
  using namespace accat::auxilia::literals;

  constexpr auto t4 = "world"_c;

  auto t8 = t("hello");
  std::cout << typeid(t4).name() << "\n";
  std::cout << t8 << "\n";

  auto helloWorld = "hello, world!"_c;
  std::cout << helloWorld.data() << "\n";
  std::cout << "Size: " << helloWorld.size() << "\n";
  std::cout << "Front: " << helloWorld.front() << "\n";
  std::cout << "Back: " << helloWorld.back() << "\n";
  std::cout << "Starts with 'hello': "
            << (helloWorld.starts_with("hello") ? "true" : "false") << "\n";
  std::cout << "Starts with 'world': "
            << (helloWorld.starts_with("world") ? "true" : "false") << "\n";
  std::cout << "Contains 'o': " << (helloWorld.contains('o') ? "true" : "false")
            << "\n";
  std::cout << "Count of 'l': " << helloWorld.count('l') << "\n";
  std::cout << helloWorld.at(0) << "\n";
}
