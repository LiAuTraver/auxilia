#include <cassert>
#include <climits>
#include <cstddef>
#include <iostream>
#include <new>
#include <span>

#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>

int main() { accat::auxilia::Trie<> trie; }
// unused vvv
// using namespace accat::auxilia;
// class dynamic_bitset : details::_bitset_base<std::dynamic_extent>, Printable
// {
//   using myBase = details::_bitset_base<std::dynamic_extent>;
//   friend myBase;

//   using myBase::bit_offset;
//   using myBase::bits_per_word;
//   using myBase::word_offset;
//   using storage_type = myBase::storage_type;

//   using container_type = std::vector<storage_type>;

// public:
//   using size_type = size_t;

// protected:
//   container_type myArr;
//   size_type mySize;

// public:
//   constexpr auto size() const noexcept { return mySize; }
// };
