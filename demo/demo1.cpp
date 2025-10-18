#include <array>
#include <bitset>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <print>
#include <type_traits>
#include "accat/auxilia/details/bitset.hpp"
#include "accat/auxilia/details/format.hpp"

using accat::auxilia::bitset;
template <size_t N> using std_bitset = std::bitset<N>;

template <typename T> void bitset_operations_test(T value) {
  T another = value;
  (void)(value & another);
  (void)(value | another);
  (void)(value ^ another);

  std::istringstream input("0010100101");
  input >> value;

  std::ostringstream output;
  output << value;

  std::cout << "Bitset value: " << output.str() << "\n";
}

int main() {
  // have some tests
  constexpr bitset<20> b1(0b101010);
  constexpr bitset<20> b2(0b1100);
  constexpr bitset<20> b3 = b1 & b2;
  constexpr bitset<20> b4 = b1 | b2;
  constexpr bitset<0> b0;
  std::cout << "b1: " << b1.to_ullong() << "\n"; // 42
  std::cout << "b2: " << b2.to_ullong() << "\n"; // 12
  std::cout << "b3: " << b3.to_ullong() << "\n"; // 8
  std::cout << "b4: " << b4.to_ullong() << "\n"; // 46
  auto myhash = std::hash<bitset<20>>::operator()(b1);
  std::cout << "hash of b1: " << myhash << "\n";

  constexpr auto std_b1 = std_bitset<20>(0b101010);
  auto stdhash = std::hash<std_bitset<20>>::operator()(std_b1);
  std::cout << "hash of std::b1: " << stdhash << "\n";
  std::cout << b1.to_string(accat::auxilia::FormatPolicy::kDetailed)
            << "\n"; // 0b00000000000000101010
  std::println("b1: {}", b1);

  bitset<5> value{};
  bitset<5> another(std::string("0100010101"));
  bitset<5> another2("0100010101");

  bitset_operations_test(value);
  bitset_operations_test(another);
  bitset_operations_test(another2);
}
