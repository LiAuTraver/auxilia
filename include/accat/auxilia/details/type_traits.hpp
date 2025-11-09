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
} // namespace accat::auxilia

/// You may ask why I re-implemented some of the type traits in std.
/// Just as proof of some false claims my colleagues made like
/// "it's compiler intrinsics and cannot be implemented in pure C++".
/// Well, this proves most of them wrong, isn't it? ;)
namespace accat::auxilia {

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

template <bool Condition, typename Ty = void> struct enable_if {};
template <typename Ty> struct enable_if<true, Ty> {
  using type = Ty;
};
template <bool Condition, typename Ty = void>
using enable_if_t = typename enable_if<Condition, Ty>::type;

template <bool Condition, typename Ty1, typename Ty2> struct conditional {
  using type = Ty1;
};
template <typename Ty1, typename Ty2> struct conditional<false, Ty1, Ty2> {
  using type = Ty2;
};
template <bool Condition, typename Ty1, typename Ty2>
using conditional_t = typename conditional<Condition, Ty1, Ty2>::type;

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
template <typename Ty, unsigned int I>
struct extent : integral_constant<size_t, extent_v<Ty, I>> {};

template <const auto &Chars>
inline constexpr size_t array_size_v =
    extent_v<remove_reference_t<decltype(Chars)>>;
template <const auto &Chars>
struct array_size : integral_constant<size_t, array_size_v<Chars>> {};

#ifdef __clang__
template <class Ty, class Uy> constexpr bool is_same_v = __is_same(Ty, Uy);
template <class Ty, class Uy>
struct is_same : bool_constant<__is_same(Ty, Uy)> {};
#else
template <class, class> constexpr bool is_same_v = false;
template <class _Ty> constexpr bool is_same_v<_Ty, _Ty> = true;
template <class Ty, class Uy>
struct is_same : bool_constant<is_same_v<Ty, Uy>> {};
#endif

/// @see Microsoft STL's @link std::_Is_specialization_v @endlink.
template <class Ty, template <class...> class Template>
constexpr bool is_specialization_v = false;
template <template <class...> class Template, class... Types>
constexpr bool is_specialization_v<Template<Types...>, Template> = true;
template <class Ty, template <class...> class Template>
struct is_specialization : bool_constant<is_specialization_v<Ty, Template>> {};

/// @see Microsoft STL's @link std::_Is_any_of_v @endlink.
template <typename Ty, typename... Candidates>
inline constexpr bool is_any_of_v = (is_same_v<Ty, Candidates> || ...);
template <typename Ty, typename... Candidates>
struct is_any_of : bool_constant<is_any_of_v<Ty, Candidates...>> {};

template <typename Ty>
inline constexpr auto is_integral_v = is_any_of_v<remove_cv_t<Ty>,
                                                  bool,
                                                  char,
                                                  signed char,
                                                  unsigned char,
                                                  wchar_t,
#ifdef __cpp_char8_t
                                                  char8_t,
#endif // defined(__cpp_char8_t)
                                                  char16_t,
                                                  char32_t,
                                                  short,
                                                  unsigned short,
                                                  int,
                                                  unsigned int,
                                                  long,
                                                  unsigned long,
                                                  long long,
                                                  unsigned long long>;

template <typename Ty> struct is_integral : bool_constant<is_integral_v<Ty>> {};

template <typename Ty>
inline constexpr auto is_floating_point_v =
    is_any_of_v<remove_cv_t<Ty>, float, double, long double>;
template <typename Ty>
struct is_floating_point : bool_constant<is_floating_point_v<Ty>> {};

template <typename Ty>
inline constexpr auto is_arithmetic_v =
    is_integral_v<Ty> || is_floating_point_v<Ty>;
template <typename Ty>
struct is_arithmetic : bool_constant<is_arithmetic_v<Ty>> {};

} // namespace accat::auxilia
