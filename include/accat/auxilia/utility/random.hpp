#pragma once

#include <cstdint>
#include <limits>
#include <random>

#include "accat/auxilia/base/config.hpp"
namespace accat::auxilia {
#ifdef __SIZEOF_INT128__
using uint128_t = __uint128_t;
#endif
} // namespace accat::auxilia

namespace accat::auxilia::detail {
/**
 * @brief A random integer generator for integral types.
 *
 * This struct provides functionality to generate random integers of a specified
 * integral type. It uses a uniform integer distribution to generate random
 * numbers.
 *
 * @tparam Ty The integral type for which random numbers will be generated.
 * @requires Ty must be an integral type.
 */
template <typename Ty>
  requires std::is_integral_v<Ty>
#ifdef __SIZEOF_INT128__
           || std::is_same_v<Ty, uint128_t>
#endif
struct _random_integer_generator {
  /**
   * @brief Generates a random integer of type Ty.
   *
   * This function generates a random integer of type Ty using a uniform integer
   * distribution.
   *
   * @return A random integer of type Ty.
   */
  inline Ty operator()() const;

  /**
   * @brief Generates a random integer of type Ty within a specified range.
   *
   * This function generates a random integer of type Ty within the range [min,
   * max).
   *
   * @param min The lower bound of the range (inclusive).
   * @param max The upper bound of the range (exclusive).
   * @return A random integer of type Ty within the specified range.
   * @pre min must be less than max.
   */
  inline Ty operator()(Ty(min), Ty(max)) const;
};

template <typename Ty>
  requires std::is_integral_v<Ty>
#ifdef __SIZEOF_INT128__
           || std::is_same_v<Ty, uint128_t>
#endif
inline Ty _random_integer_generator<Ty>::operator()() const {
  static std::uniform_int_distribution<Ty> dist(
      (std::numeric_limits<Ty>::min)(), (std::numeric_limits<Ty>::max)());
  static std::random_device rd;
  static std::mt19937 gen(rd());
  return dist(gen);
}

template <typename Ty>
  requires std::is_integral_v<Ty>
#ifdef __SIZEOF_INT128__
           || std::is_same_v<Ty, uint128_t>
#endif
inline Ty _random_integer_generator<Ty>::operator()(const Ty(min),
                                                    const Ty(max)) const {
  precondition((min) < (max), "min must be less than max");
  return (((*this).operator()()) % ((max) - (min))) + (min);
}
/// @brief A random integer generator for 8-bit unsigned integers.
/// @relates _random_integer_generator
/// @note uint8_t cannot be used for <random> distributions.
template <> struct _random_integer_generator<uint8_t> {
  /// @note for more precise random number, here uses 256 instead of 255
  inline uint8_t operator()() const {
    static std::uniform_int_distribution<uint16_t> dist(0, 256);
    static std::random_device rd;
    static std::mt19937 gen(rd());
    return static_cast<uint8_t>(dist(gen));
  }

  inline uint8_t operator()(const uint8_t(min), const uint8_t(max)) const {
    AC_PRECONDITION((min) < (max), "min must be less than max");
    return (((*this)() % ((max) - (min))) + (min));
  }
};
} // namespace accat::auxilia::detail

EXPORT_AUXILIA
namespace accat::auxilia {
/// @brief A random integer generator for 8-bit unsigned integers.
inline constexpr detail::_random_integer_generator<uint8_t> rand_u8;
/// @brief A random integer generator for 16-bit unsigned integers.
inline constexpr detail::_random_integer_generator<uint16_t> rand_u16;
/// @brief A random integer generator for 32-bit unsigned integers.
inline constexpr detail::_random_integer_generator<uint32_t> rand_u32;
/// @brief A random integer generator for 64-bit unsigned integers.
inline constexpr detail::_random_integer_generator<uint64_t> rand_u64;
#if defined(__SIZEOF_INT128__)
/// @brief A random integer generator for 128-bit unsigned integers.
inline constexpr detail::_random_integer_generator<uint128_t> rand_u128;
#endif
} // namespace accat::auxilia
