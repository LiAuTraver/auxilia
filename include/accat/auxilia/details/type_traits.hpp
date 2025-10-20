#pragma once

namespace accat::auxilia::details {
/// @brief helper aggregate struct for NTTP
template <typename CharT, size_t N> struct basic_chars_storage {
  using value_type = CharT;
  static consteval auto size() noexcept { return N; }
  value_type arr[N] = {};
  consteval basic_chars_storage(const value_type (&arr)[N]) noexcept {
    for (size_t i = 0; i < N; ++i)
      this->arr[i] = arr[i];
  }
};
// CTAD
template <typename CharT, size_t N>
basic_chars_storage(const CharT (&)[N]) -> basic_chars_storage<CharT, N>;
} // namespace accat::auxilia::details

namespace accat::auxilia {
// NTTP helper
template <const details::basic_chars_storage MyChars>
consteval auto as_basic_chars_storage() noexcept {
  return MyChars;
}

template <typename Ty, Ty Val> struct integral_constant {
  static constexpr Ty value = Val;
  using value_type = Ty;
  using type = integral_constant;
  [[nodiscard]] consteval operator value_type() const noexcept { return value; }
  [[nodiscard]] consteval value_type operator()() const noexcept {
    return value;
  }
};

template <bool Val> using bool_constant = integral_constant<bool, Val>;
using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

template <typename Ty> struct remove_const { // match non-const types
  using type = Ty;
};
template <typename Ty> struct remove_const<const Ty> { // match const types
  using type = Ty;
};
template <typename Ty> using remove_const_t = typename remove_const<Ty>::type;

template <typename Ty> struct remove_volatile { // match non-volatile types
  using type = Ty;
};
template <typename Ty>
struct remove_volatile<volatile Ty> { // match volatile types
  using type = Ty;
};
template <typename Ty>
using remove_volatile_t = typename remove_volatile<Ty>::type;

template <typename Ty> struct remove_reference { // match non-reference types
  using type = Ty;
};
template <typename Ty>
struct remove_reference<Ty &> { // match lvalue reference types
  using type = Ty;
};
template <typename Ty>
struct remove_reference<Ty &&> { // match rvalue reference types
  using type = Ty;
};
template <typename Ty>
using remove_reference_t = typename remove_reference<Ty>::type;

template <typename Ty>
using remove_cv_t =
    remove_const_t<remove_volatile_t<Ty>>; // remove both const and volatile

template <typename Ty>
using remove_cvref_t =
    remove_cv_t<remove_reference_t<Ty>>; // remove both cv and reference

template <typename Ty, unsigned int N = 0>
inline constexpr auto extent_v = 0zu; // match non-array types
template <typename Ty, size_t N>
inline constexpr auto extent_v<Ty[N], 0> = N; // match first dimension
template <typename Ty, unsigned int I, size_t N>
inline constexpr auto extent_v<Ty[N], I> =
    extent_v<Ty, I - 1>; // match higher dimensions, e.g., T[M][N][K]
template <typename Ty, unsigned int I>
inline constexpr auto extent_v<Ty[], I> =
    extent_v<Ty, I - 1>; // match higher dimensions of unknown bound arrays

template <const auto &Chars>
inline constexpr size_t array_size_v =
    extent_v<remove_reference_t<decltype(Chars)>>;
template <const auto &Chars>
struct array_size : integral_constant<size_t, array_size_v<Chars>> {};
} // namespace accat::auxilia
