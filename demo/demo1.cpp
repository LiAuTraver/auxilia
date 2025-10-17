#include <vcruntime_typeinfo.h>
#include <array>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <type_traits>
#include "accat/auxilia/details/bitset.hpp"

using accat::auxilia::bitset;
// using std::bitset;
int main() {
  // have some tests
  bitset<20> b1(0b101010);
  bitset<20> b2(0b1100);
  bitset<20> b3 = b1 & b2;
  bitset<20> b4 = b1 | b2;
  std::cout << "b1: " << b1.to_ullong() << "\n"; // 42
  std::cout << "b2: " << b2.to_ullong() << "\n"; // 12
  std::cout << "b3: " << b3.to_ullong() << "\n"; // 8
  std::cout << "b4: " << b4.to_ullong() << "\n"; // 46
}
