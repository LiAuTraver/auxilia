#pragma once
#include "./config.hpp"
#include "./format.hpp"
// I'm too lazy to sustitude those `constexpr` to provide compatibility for
// older standards. So I just guard the whole file for C++23 and later.
#if __cplusplus >= 202302L
EXPORT_AUXILIA
namespace accat::auxilia {
template <size_t N> class AC_EMPTY_BASES AC_NOVTABLE bitset : Printable {
  static_assert(
      N > 0, "Sorry, not implemented if N == 0!"); // TODO: implement bitset<0>

public:
  constexpr bitset() noexcept {}
  constexpr bitset(const unsigned long long val) noexcept
      : myArr(static_cast<underlying_type>(needs_mask ? val & mask : val)) {}
  constexpr ~bitset() noexcept {}
  // TODO: construct from string, and to string

protected:
  using underlying_type =
      std::conditional_t<N <= sizeof(unsigned long) * char_bit,
                         unsigned long,
                         unsigned long long>;

  inline static constexpr auto bits_per_word =
      char_bit * sizeof(underlying_type);
  inline static constexpr auto arr_length =
      (N + bits_per_word - 1) / bits_per_word;
  inline static constexpr auto needs_mask =
      N < char_bit * sizeof(unsigned long long);
  inline static constexpr auto mask = (1ull << (needs_mask ? N : 0)) - 1ull;

  underlying_type myArr[arr_length] = {};

private:
  constexpr auto &do_set_unchecked(const size_t pos, const bool val) noexcept {
    auto &my_word = myArr[word_offset(pos)];
    const auto my_bit = underlying_type{1} << bit_offset(pos);

    if (val)
      my_word |= my_bit;
    else
      my_word &= ~my_bit;

    return *this;
  }
  AC_FLATTEN constexpr auto
  do_subscript_unchecked(const size_t pos) const noexcept {
    return !!(myArr[word_offset(pos)] &
              (underlying_type{1} << bit_offset(pos)));
  }
  AC_FLATTEN constexpr auto &do_flip_unchecked(const size_t pos) noexcept {
    myArr[word_offset(pos)] ^= (underlying_type{1} << bit_offset(pos));
    return *this;
  }
  AC_FLATTEN constexpr auto &trim() noexcept {
    myArr[arr_length - 1] &= (underlying_type{1} << (N % bits_per_word)) - 1;
    return *this;
  }
  AC_FLATTEN constexpr auto word_offset(const size_t pos) const noexcept {
    return pos / bits_per_word;
  }
  AC_FLATTEN constexpr auto bit_offset(const size_t pos) const noexcept {
    return pos % bits_per_word;
  }

public:
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
  [[nodiscard]] constexpr auto
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
  [[nodiscard]] constexpr auto test(const size_t pos) const noexcept {
    precondition(N > pos, "invalid bit position; position out of range")
    return do_subscript_unchecked(pos);
  }
  [[nodiscard]] constexpr auto any() const noexcept {
    for (auto i = 0ull; i < arr_length; ++i)
      if (myArr[i] != 0)
        return true;

    return false;
  }
  [[nodiscard]] constexpr auto none() const noexcept { return !any(); }
  [[nodiscard]] constexpr auto all() const noexcept {
    constexpr auto no_padding = N % bits_per_word == 0;
    for (auto i = 0ull; i < arr_length + no_padding; ++i) {
      if (myArr[i] != ~static_cast<underlying_type>(0))
        return false;
    }

    return no_padding || myArr[arr_length - 1] ==
                             (underlying_type{1} << (N % bits_per_word)) - 1;
  }
  [[nodiscard]] constexpr auto operator<<(const size_t shift) const noexcept {
    auto tmp = *this;
    tmp <<= shift;
    return tmp;
  }
  [[nodiscard]] constexpr auto operator>>(const size_t shift) const noexcept {
    auto tmp = *this;
    tmp >>= shift;
    return tmp;
  }
  [[nodiscard]] constexpr auto count() const noexcept {
    size_t total = 0;
    for (auto i = 0ull; i < arr_length; ++i) {
      total += std::popcount(myArr[i]);
    }
    return total;
  }
  [[nodiscard]] consteval auto size() const noexcept { return N; }
  [[nodiscard]] constexpr auto operator[](const size_t pos) const noexcept {
    precondition(pos < N, "subscript out of range")
    return do_subscript_unchecked(pos);
  }
  [[nodiscard]] constexpr auto operator[](const size_t pos) noexcept {
    precondition(pos < N, "subscript out of range")
    return reference{*this, pos};
  }
  constexpr auto &flip(const size_t pos) noexcept {
    precondition(pos < N, "invalid bit position; position out of range")
    return do_flip_unchecked(pos);
  }
  constexpr auto &flip(void) {
    for (auto i = 0ull; i < arr_length; ++i)
      myArr[i] = ~myArr[i];

    return trim();
  }
  constexpr auto &set(const size_t pos, const bool val = true) noexcept {
    precondition(N > pos, "invalid bit position; position out of range")
    return do_set_unchecked(pos, val);
  }
  constexpr auto &set() noexcept {
    if consteval {
      for (auto &elem : myArr)
        elem = static_cast<underlying_type>(-1);
    } else {
      // memset use `int` as the 2nd argument(set value)
      ::memset(&myArr, static_cast<int>(-1), sizeof(myArr));
    }
    return *this;
  }
  constexpr auto &reset(const size_t pos) noexcept { return set(pos, false); }
  constexpr auto &reset() noexcept {
    if consteval {
      for (auto &elem : myArr)
        elem = 0;
    } else {
      ::memset(&myArr, 0, sizeof(myArr));
    }
    return *this;
  }

public:
  [[nodiscard]] constexpr auto to_ullong() const noexcept(N <= 64) {

    if constexpr (N > 64) {
      for (size_t i = 1; i < arr_length; ++i) {
        if (myArr[i] != 0) {
          AC_THROW_OR_DIE("bitset overflow");
        }
      }
    }
    return static_cast<unsigned long long>(myArr[0]);
  }

public:
  // proxy class representing a reference to a bit
  class reference {
    friend bitset;
    constexpr reference(bitset<N> &bitset, const size_t pos) noexcept
        : bitset_(&bitset), pos_(pos) {}
    bitset<N> *bitset_;
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

    [[nodiscard]] constexpr bool operator~() const noexcept {
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
};
// helper to extract bits [Beg, End)
template <size_t Beg, size_t End, typename T>
inline static constexpr T extract(const T value) noexcept {
  static_assert(Beg <= End, "Beg must be less than or equal to End");
  static_assert(End <= sizeof(T) * 8, "End must be within the bit width of T");

  constexpr T mask = (static_cast<T>(1) << (End - Beg)) - 1;
  return (value >> Beg) & mask;
}

// helper to concatenate bits from multiple [Begin, End) ranges
template <typename T, size_t Begin, size_t End, size_t... Rest>
inline static constexpr T concat(const T value) noexcept {
  static_assert(sizeof...(Rest) % 2 == 0, "Rest must be a multiple of 2");
  if constexpr (sizeof...(Rest) == 0) {
    return extract<Begin, End>(value);
  } else {
    auto chunk = extract<Begin, End, T>(value);
    constexpr auto bitCount = End - Begin;
    return chunk | (concat<T, Rest...>(value) << bitCount);
  }
}

// helper to perform sign extension
// note: `NewWidthTy` shall be the same type as pc when used.
template <size_t OrigWidth, typename OrigTy, typename NewWidthTy>

inline static constexpr auto signExtend(const OrigTy value) noexcept {
  static_assert(sizeof(OrigTy) * 8 >= OrigWidth, "OrigTy is too small");
  static_assert(sizeof(NewWidthTy) * 8 >= OrigWidth, "NewWidthTy is too small");
  static_assert(std::is_integral_v<OrigTy>, "OrigTy must be an integral type");
  static_assert(std::is_integral_v<NewWidthTy>,
                "NewWidthTy must be an integral type");

  const auto signBit = (value >> (OrigWidth - 1)) & 1;
  if (signBit) {
    // if the sign bit is set, extend the sign bit to the left
    return value | ~((static_cast<OrigTy>(1) << OrigWidth) - 1);
  } else {
    // if the sign bit is not set, just return the value
    return value;
  }
}

// helper to decode 2's complement, return original value if sign bit is 1,
// otherwise return the negated value
template <typename T>
inline static constexpr std::make_signed_t<T>
decode2sComplement(const T value) noexcept {
  static_assert(std::is_integral_v<T>, "T must be an integral type");
  const auto signBit = (value >> (sizeof(T) * 8 - 1)) & 1;
  if (signBit) {
    return value;
  } else {
    return ~value + 1;
  }
}
template <size_t N>
[[nodiscard]] inline constexpr auto operator&(const bitset<N> &left,
                                              const bitset<N> &right) noexcept {
  auto ans = left;
  ans &= right;
  return ans;
}
template <size_t N>
[[nodiscard]] inline constexpr auto operator|(const bitset<N> &left,
                                              const bitset<N> &right) noexcept {
  auto ans = left;
  ans |= right;
  return ans;
}
} // namespace accat::auxilia
#endif
