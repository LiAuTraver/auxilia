#pragma once

#include "./config.hpp"
EXPORT_AUXILIA
namespace accat::auxilia {
#if !AC_USE_STD_FMT
using ::fmt::format;
using ::fmt::format_string;
using ::fmt::format_to;
using ::fmt::formattable;
using ::fmt::is_formattable;
using ::fmt::print;
#  ifdef __clang__
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wcxx-attribute-extension"
[[clang::using_if_exists]] using ::fmt::println;
#    pragma clang diagnostic pop
#  elif _WIN32
using ::fmt::println;
#  else
// some wired issue `fmt::println not found` on gcc 13
template <typename... T>
inline auto println(fmt::format_string<T...> fmt, T &&...args) {
  fmt::println(stdout, fmt, std::forward<T>(args)...);
}
template <typename... T>
inline auto println(FILE *f, fmt::format_string<T...> fmt, T &&...args) {
  fmt::print(f, "{}\n", fmt::format(fmt, std::forward<T>(args)...));
}
template <typename... T>
inline auto
println(std::ostream &os, fmt::format_string<T...> fmt, T &&...args) {
  fmt::print(os, "{}\n", fmt::format(fmt, std::forward<T>(args)...));
}
template <typename... Args>
inline auto
println(std::wostream &os,
        fmt::basic_format_string<wchar_t, fmt::type_identity_t<Args>...> fmt,
        Args &&...args) {
  fmt::print(os, L"{}\n", fmt::format(fmt, std::forward<Args>(args)...));
}
template <typename... T>
inline auto println(std::FILE *f, fmt::wformat_string<T...> fmt, T &&...args) {
  fmt::print(f, L"{}\n", fmt::format(fmt, std::forward<T>(args)...));
}

template <typename... T>
inline auto println(fmt::wformat_string<T...> fmt, T &&...args) {
  [[clang::musttail]] return fmt::print(
      L"{}\n", fmt::format(fmt, std::forward<T>(args)...));
}
#  endif
template <class... T>
inline auto
println(const fmt::text_style &ts, fmt::format_string<T...> fmt, T &&...args) {
  fmt::print(ts, fmt, std::forward<decltype(args)>(args)...);
  ::putchar('\n');
}
template <class... T>
inline auto println(FILE *f,
                    const fmt::text_style &ts,
                    fmt::format_string<T...> fmt,
                    T &&...args) {
  fmt::print(f, ts, fmt, std::forward<decltype(args)>(args)...);
  ::putchar('\n');
}
#else
using ::std::format;
using ::std::format_string;
using ::std::format_to;
using ::std::formattable;
using ::std::print;
using ::std::println;

// compatible with fmt::text_style, for std::format does not support it
template <typename E, class... T>
  requires std::is_enum_v<E>
inline auto println(const E &, std::format_string<T...> fmt, T &&...args) {
  std::print(fmt, std::forward<decltype(args)>(args)...);
  ::putchar('\n');
}
template <typename E, class... T>
  requires std::is_enum_v<E>
inline auto
println(FILE *f, const E &, std::format_string<T...> fmt, T &&...args) {
  std::print(f, fmt, std::forward<decltype(args)>(args)...);
  ::putchar('\n');
}
#endif
} // namespace accat::auxilia

namespace accat::auxilia {
template <typename... Ts> class Variant;
enum class FormatPolicy : uint8_t;
struct Printable;
struct Viewable;
template <typename Ty>
  requires std::is_arithmetic_v<std::remove_cvref_t<Ty>>
inline bool is_integer(Ty &&value) noexcept {
  return std::trunc(std::forward<Ty>(value)) == value;
}
template <typename... Ts> struct match : Ts... {
  using Ts::operator()...;
};
/// @brief deduction guide for `match`
template <typename... Ts> match(Ts...) -> match<Ts...>;
enum class FormatPolicy : uint8_t {
  kDefault = 0,
  kDetailed = 1,
  kBrief = 2,
};
/// @interface Printable
/// @brief A class that represents a printable object; can be directly
/// printed via `std::cout` or `fmt::print`.
/// @note use public inheritance to make fmt::print work.
struct AC_NOVTABLE AC_EMPTY_BASES Printable {
protected:
  using string_type = std::string;
  template <typename T>
    requires std::is_base_of_v<Printable, T>
  friend auto operator<<(std::ostream &os, const T &p) -> std::ostream & {
    return os << p.to_string(FormatPolicy::kDefault);
  }
  template <typename T>
    requires std::is_base_of_v<Printable, T>
  [[nodiscard]]
  friend auto
  format_as(const T &p,
            const FormatPolicy &format_policy = FormatPolicy::kDefault)
      -> Printable::string_type {
    return p.to_string(format_policy);
  }
};

/// @interface Viewable
struct AC_NOVTABLE AC_EMPTY_BASES Viewable {
protected:
  using string_view_type = std::string_view;
  template <typename T>
    requires std::is_base_of_v<Viewable, T>
  friend auto operator<<(std::ostream &os, const T &v) -> std::ostream & {
    return os << v.to_string_view(FormatPolicy::kDefault);
  }
  template <typename T>
    requires std::is_base_of_v<Viewable, T>
  [[nodiscard]] friend auto operator<<(std::wostream &os, const T &v)
      -> std::wostream & {
    return os << v.to_string_view(FormatPolicy::kDefault);
  }
};
} // namespace accat::auxilia

// ReSharper disable once CppRedundantNamespaceDefinition
namespace std {
template <typename Derived>
  requires is_base_of_v<::accat::auxilia::Printable,
                        Derived>
struct formatter<Derived> { // NOLINT(cert-dcl58-cpp)
  inline constexpr auto parse(format_parse_context &ctx) const noexcept {
    return ctx.begin();
  }
  template <typename FormatContext>
  inline constexpr auto format(const Derived &p, FormatContext &ctx) const {
    return format_to(
        ctx.out(), "{}", p.to_string(::accat::auxilia::FormatPolicy::kDefault));
  }
};
} // namespace std
