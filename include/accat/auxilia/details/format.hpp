#pragma once

#include <cstdint>
#include <cstdio>
#include <ostream>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

#include "macros.hpp"
#include "config.hpp"
#if !AC_USE_STD_FMT
#  include <fmt/color.h>
#  include <fmt/xchar.h>
namespace accat::auxilia {
template <typename... T>
inline auto
Format(fmt::text_style ts, fmt::format_string<T...> fmt, T &&...args) {
  return ::fmt::format(ts, fmt, std::forward<T>(args)...);
}
template <typename... T>
inline auto
Format(fmt::text_style ts, fmt::wformat_string<T...> fmt, T &&...args) {
  return ::fmt::format(ts, fmt, std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(fmt::text_style ts, fmt::format_string<T...> fmt, T &&...args) {
  return ::fmt::print(
      ::std::forward<fmt::text_style>(ts), (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Print(::std::FILE *f,
                  fmt::text_style ts,
                  fmt::format_string<T...> fmt,
                  T &&...args) {
  return ::fmt::print(f,
                      ::std::forward<fmt::text_style>(ts),
                      (fmt),
                      ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Println(fmt::text_style ts, fmt::format_string<T...> fmt, T &&...args) {
  ::fmt::print(
      ::std::forward<fmt::text_style>(ts), (fmt), ::std::forward<T>(args)...);
  ::puts("\n");
}
template <typename... T>
inline auto Println(::std::FILE *f,
                    fmt::text_style ts,
                    fmt::format_string<T...> fmt,
                    T &&...args) {
  ::fmt::print(f,
               ::std::forward<fmt::text_style>(ts),
               (fmt),
               ::std::forward<T>(args)...);
  ::puts("\n");
}
} // namespace accat::auxilia
#endif
namespace accat::auxilia {
template <typename T, typename Char = char>
concept Formattable = AC_STD_OR_FMT formattable<T, Char>;

// omit the locale overload
template <typename... T>
inline auto Format(AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT format((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
  requires(!std::is_same_v<AC_STD_OR_FMT format_string<T...>, T...>)
inline auto Format(T &&...args) {
  return AC_STD_OR_FMT format(("{}"), ::std::forward<T>(args)...);
}

template <typename... T>
inline auto
Print(::std::FILE *f, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Print(AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(::std::FILE *f, AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Print(AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(::std::ostream &os, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(os, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(::std::wostream &os, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(os, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
  requires(!std::is_same_v<AC_STD_OR_FMT format_string<T...>, T...>)
inline auto Print(T &&...args) {
  return AC_STD_OR_FMT format(("{}"), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(::std::ostream &os,
                    AC_STD_OR_FMT format_string<T...> fmt,
                    T &&...args) {
  return AC_STD_OR_FMT println(os, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(::std::wostream &os,
                    AC_STD_OR_FMT format_string<T...> fmt,
                    T &&...args) {
  return AC_STD_OR_FMT println(os, (fmt), ::std::forward<T>(args)...);
}

template <typename... T>
inline auto
Println(::std::FILE *f, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Println(::std::FILE *f, AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println((fmt), ::std::forward<T>(args)...);
}
inline auto Println(void) { return AC_STD_OR_FMT println(""); }

template <typename... T>
  requires(!std::is_same_v<AC_STD_OR_FMT format_string<T...>, T...>)
inline auto Println(T &&...args) {
  return AC_STD_OR_FMT format(("{}\n"), ::std::forward<T>(args)...);
}
} // namespace accat::auxilia

namespace accat::auxilia {
template <typename... Ts> class Variant;
enum class FormatPolicy : uint8_t;
struct Printable;
struct Viewable;
template <typename Ty>
  requires std::is_arithmetic_v<std::remove_cvref_t<Ty>>
inline bool is_integer(Ty &&value) noexcept {
  return std::trunc(::std::forward<Ty>(value)) == value;
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
    if constexpr (requires { p.to_string(FormatPolicy::kDefault); }) {
      return os << p.to_string(FormatPolicy::kDefault);
    } else {
      return os << p.to_string();
    }
  }
  template <typename T>
    requires std::is_base_of_v<Printable, T>
  [[nodiscard]]
  friend auto
  format_as(const T &p,
            const FormatPolicy format_policy = FormatPolicy::kDefault)
      -> Printable::string_type {
    if constexpr (requires { p.to_string(format_policy); }) {
      return p.to_string(format_policy);
    } else {
      return p.to_string();
    }
  }

public:
  static constexpr auto Brief = [](const auto &p)
    requires std::is_base_of_v<Printable, std::decay_t<decltype(p)>>
  { return p.to_string(FormatPolicy::kBrief); };
  static constexpr auto Detailed = [](const auto &p)
    requires std::is_base_of_v<Printable, std::decay_t<decltype(p)>>
  { return p.to_string(FormatPolicy::kDetailed); };
  static constexpr auto Default = [](const auto &p)
    requires std::is_base_of_v<Printable, std::decay_t<decltype(p)>>
  { return p.to_string(FormatPolicy::kDefault); };
};

/// @interface Viewable
struct AC_NOVTABLE AC_EMPTY_BASES Viewable {
protected:
  using string_view_type = std::string_view;
  template <typename T>
    requires std::is_base_of_v<Viewable, T>
  friend auto operator<<(std::ostream &os, const T &v) -> std::ostream & {
    if constexpr (requires { v.to_string_view(FormatPolicy::kDefault); }) {
      return os << v.to_string_view(FormatPolicy::kDefault);
    } else {
      return os << v.to_string_view();
    }
  }
  template <typename T>
    requires std::is_base_of_v<Viewable, T>
  [[nodiscard]] friend auto operator<<(std::wostream &os, const T &v)
      -> std::wostream & {
    if constexpr (requires { v.to_string_view(FormatPolicy::kDefault); }) {
      return os << v.to_string_view(FormatPolicy::kDefault);
    } else {
      return os << v.to_string_view();
    }
  }

public:
  static constexpr auto Brief = [](const auto &v)
    requires std::is_base_of_v<Viewable, std::decay_t<decltype(v)>>
  { return v.to_string_view(FormatPolicy::kBrief); };
  static constexpr auto Detailed = [](const auto &v)
    requires std::is_base_of_v<Viewable, std::decay_t<decltype(v)>>
  { return v.to_string_view(FormatPolicy::kDetailed); };
  static constexpr auto Default = [](const auto &v)
    requires std::is_base_of_v<Viewable, std::decay_t<decltype(v)>>
  { return v.to_string_view(FormatPolicy::kDefault); };
};
} // namespace accat::auxilia

namespace std {
template <typename Derived>
  requires std::is_base_of_v<::accat::auxilia::Printable, Derived>
struct formatter<Derived> {
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
