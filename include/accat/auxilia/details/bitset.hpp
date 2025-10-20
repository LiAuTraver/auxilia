#pragma once
#include <cstddef>
#include <ostream>
#include "./config.hpp"
#include "./format.hpp"
#include "./chars_helper.hpp"

// I'm too lazy to sustitude those `constexpr` to provide compatibility for
// older standards. So I just guard the whole file for C++23 and later.
#if __cplusplus >= 202302L
EXPORT_AUXILIA
namespace accat::auxilia {
#  ifdef AC_STD_COMPLIANT_BITSET
#    define AC_BITSET_ZERO(...) AC_PRECONDITION(__VA_ARGS__)
#  else
#    define AC_BITSET_ZERO(...) static_assert(__VA_ARGS__)
#  endif
/**
 *  A bitset is a fixed-size sequence of bits.
 *  For compatibility reasons, it the same interface as `std::bitset`
 *      although I do not agree with some design decisions of `std::bitset`.
 *  @tparam N The number of bits in the bitset.
 *  @todo string-related operations are not optimized yet. Hence you got no
 *      performance advantage over `std::bitset` when using this feature.
 */
template <size_t N> class bitset : Printable {
  friend struct ::std::hash<bitset<N>>;

public:
  constexpr bitset() noexcept {}
  constexpr bitset(const unsigned long long val) noexcept
      : myArr(static_cast<storage_type>(needs_mask ? val & mask : val)) {}
  constexpr ~bitset() noexcept {}
  constexpr bitset(const char (&arr)[N]) { construct_from_string(arr); }
  constexpr bitset(const std::string_view str,
                   const size_t pos = 0,
                   size_t len = std::string_view::npos,
                   const char zero = '0',
                   const char one = '1') {
    construct_from_string(str, pos, len, zero, one);
  }

protected:
  using storage_type = std::conditional_t<N <= sizeof(unsigned long) * CHAR_BIT,
                                          unsigned long,
                                          unsigned long long>;

  inline static constexpr auto bits_per_word = CHAR_BIT * sizeof(storage_type);
  inline static constexpr auto arr_length =
      N == 0 ? 1 : (N + bits_per_word - 1) / bits_per_word;
  inline static constexpr auto needs_mask =
      N < CHAR_BIT * sizeof(unsigned long long);
  inline static constexpr auto mask = (1ull << (needs_mask ? N : 0)) - 1ull;

  storage_type myArr[arr_length] = {};

private:
  /// @{
  /**
   * @name Internal helpers functions
   */
  constexpr auto construct_from_string(const std::string_view str,
                                       const size_t pos,
                                       size_t len,
                                       const char zero,
                                       const char one) AC_NOEXCEPT {
    if (str.size() < pos) {
      AC_THROW_OR_DIE("bitset constructor: position out of range");
      return;
    }
    len = std::min(len, str.size() - pos);

    // I know nothing of vectorization at all, so here is a naive implementation
    // for constructing from string. The speed is not optimized.

    // vvv read the string in reverse order vvv
    for (size_t i = 0; i < len; ++i) {
      const char c = str[pos + len - 1 - i];
      if (c == one)
        do_set_unchecked(i, true);
      else if (c == zero)
        do_set_unchecked(i, false);
      else
        AC_THROW_OR_DIE("bitset constructor: invalid character");
    }
  }
  constexpr auto &do_set_unchecked(const size_t pos, const bool val) noexcept {
    auto &my_word = myArr[word_offset(pos)];
    const auto my_bit = storage_type{1} << bit_offset(pos);

    if (val)
      my_word |= my_bit;
    else
      my_word &= ~my_bit;

    return *this;
  }
  AC_FLATTEN constexpr auto
  do_subscript_unchecked(const size_t pos) const noexcept {
    return !!(myArr[word_offset(pos)] & (storage_type{1} << bit_offset(pos)));
  }
  AC_FLATTEN constexpr auto &do_flip_unchecked(const size_t pos) noexcept {
    myArr[word_offset(pos)] ^= (storage_type{1} << bit_offset(pos));
    return *this;
  }
  // trim off the unused bits in the last storage word
  AC_FLATTEN constexpr auto &trim() noexcept {
    if constexpr (N > 0)
      myArr[arr_length - 1] &= (storage_type{1} << (N % bits_per_word)) - 1;
    return *this;
  }
  AC_FLATTEN constexpr auto word_offset(const size_t pos) const noexcept {
    return pos / bits_per_word;
  }
  AC_FLATTEN constexpr auto bit_offset(const size_t pos) const noexcept {
    return pos % bits_per_word;
  }
  constexpr auto string(const char zero, const char one) const {
    std::string str;
    str.resize_and_overwrite(N, [this, zero, one](char *buf, size_t len) {
      for (size_t i = 0; i < len; ++i) {
        buf[len - 1 - i] =
            do_subscript_unchecked(i) ? one : zero; // reverse order
      }
      return len;
    });
    return str;
  }
  /// @}

public:
  /// @{
  /**
   * @name Standard compliant member functions
   */
  AC_NODISCARD constexpr auto test(const size_t pos) const AC_NOEXCEPT {
    AC_PRECONDITION(N > pos, "invalid bit position; position out of range")
    return do_subscript_unchecked(pos);
  }
  AC_NODISCARD constexpr auto any() const noexcept {
    for (auto i = 0ull; i < arr_length; ++i)
      if (myArr[i] != 0)
        return true;

    return false;
  }
  AC_NODISCARD constexpr auto none() const noexcept { return !any(); }
  AC_NODISCARD constexpr auto all() const noexcept {
    if constexpr (N == 0)
      return true;
    constexpr auto no_padding = N % bits_per_word == 0;
    for (auto i = 0ull; i < arr_length + no_padding; ++i) {
      if (myArr[i] != ~static_cast<storage_type>(0))
        return false;
    }

    return no_padding || myArr[arr_length - 1] ==
                             (storage_type{1} << (N % bits_per_word)) - 1;
  }
  AC_NODISCARD constexpr auto count() const noexcept {
    if constexpr (N == 0)
      return 0;
    size_t total = 0;

    for (auto i = 0ull; i < arr_length; ++i)
      // popcount from standard library has been optimized via vectorization
      // so we just use it directly. it's better than writing our own version.
      total += std::popcount(myArr[i]);

    return total;
  }
  AC_NODISCARD consteval auto size() const noexcept { return N; }
  constexpr auto &set(const size_t pos, const bool val = true) AC_NOEXCEPT {
    AC_BITSET_ZERO(N > 0, "bitset<0> does not support set(pos, val)");
    AC_PRECONDITION(N > pos, "invalid bit position; position out of range")
    return do_set_unchecked(pos, val);
  }
  constexpr auto &set() noexcept {
    if constexpr (N > 0) {
      if consteval {
        for (auto &elem : myArr)
          elem = static_cast<storage_type>(-1);
      } else {
        // memset use `int` as the 2nd argument(set value)
        ::memset(&myArr, static_cast<int>(-1), sizeof(myArr));
      }
    }
    return trim();
  }
  constexpr auto &reset(const size_t pos) noexcept {
    AC_BITSET_ZERO(N > 0, "bitset<0> does not support reset(pos)");
    return set(pos, false);
  }
  constexpr auto &reset() noexcept {
    if constexpr (N > 0) {
      if consteval {
        for (auto &elem : myArr)
          elem = 0;
      } else {
        ::memset(&myArr, 0, sizeof(myArr));
      }
    }
    return *this;
  }
  constexpr auto &flip(const size_t pos) AC_NOEXCEPT {
    AC_PRECONDITION(pos < N, "invalid bit position; position out of range")
    return do_flip_unchecked(pos);
  }
  constexpr auto &flip(void) {
    if constexpr (N > 0)
      for (auto i = 0ull; i < arr_length; ++i)
        myArr[i] = ~myArr[i];

    return trim();
  }
  AC_NODISCARD constexpr auto to_ulong() const noexcept(N <= 32) {
    if constexpr (N == 0)
      return 0;
    if constexpr (N > 32) {
      for (size_t i = 1; i < arr_length; ++i) {
        if (myArr[i] != 0) {
          AC_THROW_OR_DIE("bitset overflow");
        }
      }
    }
    if (!std::in_range<unsigned long>(myArr[0])) {
      AC_THROW_OR_DIE("bitset overflow");
    }
    return static_cast<unsigned long>(myArr[0]);
  }
  AC_NODISCARD constexpr auto to_ullong() const noexcept(N <= 64) {
    if constexpr (N == 0)
      return 0;
    if constexpr (N > 64) {
      for (size_t i = 1; i < arr_length; ++i) {
        if (myArr[i] != 0) {
          AC_THROW_OR_DIE("bitset overflow");
        }
      }
    }
    return static_cast<unsigned long long>(myArr[0]);
  }
#  ifdef AC_STD_COMPLIANT_BITSET
  AC_NODISCARD constexpr auto to_string(const char zero = '0',
                                        const char one = '1') const {
    return string(zero, one);
  }
#  else
  AC_NODISCARD constexpr auto
  to_string(const FormatPolicy policy = FormatPolicy::kDefault,
            const char zero = '0',
            const char one = '1') const {
    auto str = string(zero, one);
    if (policy == FormatPolicy::kDefault or policy == FormatPolicy::kBrief)
      return str;

    return "0b"s.append(str);
  }
#  endif
  /// @}
public:
  /// @{
  /**
   * @name Non-standard member functions
   * functions tailored for my own usage and convenience,
   *    most of which are just syntactic sugar.
   */
  template <typename NumType = unsigned long long>
  AC_NODISCARD constexpr auto num() const noexcept((N <= 64)) {
    static_assert(std::is_integral_v<NumType>,
                  "template parameter T must be an integral type");
    if constexpr (std::is_same_v<NumType, unsigned long long>) {
      return to_ullong();
    } else if constexpr (is_any_of_v<NumType,
                                     unsigned short,
                                     unsigned int,
                                     unsigned long>) {
      return to_ulong();
    }
  }
  /// @}
public:
  /**
   * Proxy class representing a reference to a bit.
   * @note if N == 0, this class will never be instantiated.
   */
  class reference {
    friend bitset;

    using container_type = bitset<N>;

    constexpr reference(container_type &bitset, const size_t pos) noexcept
        : bitset_(&bitset), pos_(pos) {}
    container_type *bitset_;
    size_t pos_;

  public:
    constexpr reference(const reference &) = default;
    constexpr ~reference() noexcept {}

    constexpr operator bool() const noexcept {
      // no need to check whether the position is in range,
      // for the reference instance only originated from the function call.
      // it's guranteed to be in range.
      return bitset_->do_subscript_unchecked(pos_);
    }

    AC_NODISCARD constexpr bool operator~() const noexcept {
      // ditto
      return !bitset_->do_subscript_unchecked(pos_);
    }
    constexpr auto &operator=(const bool val) noexcept {
      bitset_->do_set_unchecked(pos_, val);
      return *this;
    }
    constexpr auto &operator=(const reference &bitref) noexcept {
      bitset_->do_set_unchecked(pos_, static_cast<bool>(bitref));
      return *this;
    }
    constexpr auto &flip() noexcept {
      bitset_->do_flip_unchecked(pos_);
      return *this;
    }
  };

public:
  /// @{
  /**
   * @name Standard compliant bitwise operators
   */
  constexpr auto &operator&=(const bitset<N> &other) noexcept {
    for (auto i = 0ull; i < arr_length; ++i)
      myArr[i] &= other.myArr[i];
    return *this;
  }
  constexpr auto &operator|=(const bitset<N> &other) noexcept {
    for (auto i = 0ull; i < arr_length; ++i)
      myArr[i] |= other.myArr[i];
    return *this;
  }
  constexpr auto &operator^=(const bitset<N> &other) noexcept {
    for (auto i = 0ull; i < arr_length; ++i)
      myArr[i] ^= other.myArr[i];
    return *this;
  }
  constexpr auto operator<<=(const size_t shift) noexcept {
    if (shift >= N) {
      reset();
      return *this;
    }

    const auto word_shift = word_offset(shift);
    const auto bit_shift = bit_offset(shift);

    if (bit_shift == 0) {
      for (auto i = arr_length; i-- > word_shift;)
        myArr[i] = myArr[i - word_shift];
    } else {
      for (auto i = arr_length; i-- > word_shift + 1;) {
        myArr[i] = (myArr[i - word_shift] << bit_shift) |
                   (myArr[i - word_shift - 1] >> (bits_per_word - bit_shift));
      }
      myArr[word_shift] = myArr[0] << bit_shift;
    }

    for (auto i = 0ull; i < word_shift; ++i)
      myArr[i] = 0;

    return trim();
  }
  constexpr auto operator>>=(const size_t shift) noexcept {
    if (shift >= N) {
      reset();
      return *this;
    }

    const auto word_shift = word_offset(shift);
    const auto bit_shift = bit_offset(shift);

    if (bit_shift == 0) {
      for (auto i = 0ull; i < arr_length - word_shift; ++i)
        myArr[i] = myArr[i + word_shift];
    } else {
      for (auto i = 0ull; i < arr_length - word_shift - 1; ++i) {
        myArr[i] = (myArr[i + word_shift] >> bit_shift) |
                   (myArr[i + word_shift + 1] << (bits_per_word - bit_shift));
      }
      myArr[arr_length - word_shift - 1] = myArr[arr_length - 1] >> bit_shift;
    }

    for (auto i = arr_length - word_shift; i < arr_length; ++i)
      myArr[i] = 0;

    return trim();
  }
  AC_NODISCARD constexpr auto
  operator==(const bitset<N> &other) const noexcept {
    if consteval {
      for (auto i = 0ull; i < arr_length; ++i) {
        if (myArr[i] != other.myArr[i])
          return false;
      }
      return true;
    }
    return ::memcmp(&myArr, &other.myArr, sizeof(myArr)) == 0;
  }
  AC_NODISCARD constexpr auto operator<<(const size_t shift) const noexcept {
    auto tmp = *this;
    tmp <<= shift;
    return tmp;
  }
  AC_NODISCARD constexpr auto operator>>(const size_t shift) const noexcept {
    auto tmp = *this;
    tmp >>= shift;
    return tmp;
  }
  AC_NODISCARD constexpr auto operator[](const size_t pos) const AC_NOEXCEPT {
    AC_BITSET_ZERO(N > 0, "bitset<0> does not support operator[] const");
    AC_PRECONDITION(pos < N, "subscript out of range")
    return do_subscript_unchecked(pos);
  }
  AC_NODISCARD constexpr auto operator[](const size_t pos) AC_NOEXCEPT {
    AC_BITSET_ZERO(N > 0, "bitset<0> does not support operator[]");
    AC_PRECONDITION(pos < N, "subscript out of range")
    return reference{*this, pos};
  }
  /// @}
};
template <size_t N>
AC_NODISCARD inline constexpr auto operator&(const bitset<N> &left,
                                             const bitset<N> &right) noexcept {
  auto ans = left;
  ans &= right;
  return ans;
}
template <size_t N>
AC_NODISCARD inline constexpr auto operator|(const bitset<N> &left,
                                             const bitset<N> &right) noexcept {
  auto ans = left;
  ans |= right;
  return ans;
}
template <size_t N>
AC_NODISCARD inline constexpr auto operator^(const bitset<N> &left,
                                             const bitset<N> &right) noexcept {
  auto ans = left;
  ans ^= right;
  return ans;
}
template <size_t N>
std::basic_ostream<char> &operator<<(std::basic_ostream<char> &os,
                                     const bitset<N> &bs) {
  // unoptimized
  return os << bs.to_string();
}
template <size_t N>
std::basic_istream<char> &operator>>(std::basic_istream<char> &is,
                                     bitset<N> &bs) {
  // unoptimized
  std::string str;
  is >> str;
  bs = bitset<N>(str);
  return is;
}
} // namespace accat::auxilia
namespace accat::auxilia::literals {
template <const details::basic_chars_storage MyChars>
consteval auto operator""_bs() noexcept {
  return bitset<array_size_v<MyChars.arr> - 1>(
      static_cast<decltype((MyChars.arr)) &&>(MyChars.arr));
}
} // namespace accat::auxilia::literals

// ReSharper disable once CppRedundantNamespaceDefinition
namespace std {
template <size_t N> struct hash<::accat::auxilia::bitset<N>> {
  AC_NODISCARD AC_STATIC_CALL_OPERATOR constexpr auto
  operator()(const ::accat::auxilia::bitset<N> &bs)
      AC_CONST_CALL_OPERATOR noexcept {
    size_t seed = 0;
    using storage_type = typename ::accat::auxilia::bitset<N>::storage_type;
    constexpr auto arr_len = ::accat::auxilia::bitset<N>::arr_length;
    hash<storage_type> hasher;
    constexpr auto my_magic_number =
        sizeof(storage_type) == 4 ? ::accat::auxilia::hash_magic_number_32bit
                                  : ::accat::auxilia::hash_magic_number_64bit;

    for (size_t i = 0; i < arr_len; ++i) {
      seed ^= hasher(bs.myArr[i]) + my_magic_number + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};
} // namespace std
#endif
