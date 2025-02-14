#pragma once

#include "./config.hpp"
EXPORT_AUXILIA
namespace accat::auxilia {
#if __has_include(<fmt/format.h>)
using ::fmt::format;
using ::fmt::format_string;
using ::fmt::format_to;
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
  [[clang::musttail]] return fmt::println(
      stdout, fmt, std::forward<T>(args)...);
}
template <typename... T>
inline void println(FILE *f, fmt::format_string<T...> fmt, T &&...args) {
  fmt::print(f, "{}\n", fmt::format(fmt, std::forward<T>(args)...));
}
template <typename... T>
inline auto
println(std::ostream &os, fmt::format_string<T...> fmt, T &&...args) {
  [[clang::musttail]] return fmt::print(
      os, "{}\n", fmt::format(fmt, std::forward<T>(args)...));
}
template <typename... Args>
inline auto
println(std::wostream &os,
        fmt::basic_format_string<wchar_t, fmt::type_identity_t<Args>...> fmt,
        Args &&...args) {
  [[clang::musttail]] return fmt::print(
      os, L"{}\n", fmt::format(fmt, std::forward<Args>(args)...));
}
template <typename... T>
inline auto println(std::FILE *f, fmt::wformat_string<T...> fmt, T &&...args) {
  [[clang::musttail]] return fmt::print(
      f, L"{}\n", fmt::format(fmt, std::forward<T>(args)...));
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
using ::std::format_to;
using ::std::print;
using ::std::println;

// compatible with fmt::text_style, for std::format does not support it
template <class... T>
inline auto println(const auto &, std::format_string<T...> fmt, T &&...args) {
  std::print(fmt, std::forward<decltype(args)>(args)...);
  putchar('\n');
}
template <class... T>
inline auto
println(FILE *f, const auto &, std::format_string<T...> fmt, T &&...args) {
  std::print(f, fmt, std::forward<decltype(args)>(args)...);
  putchar('\n');
}
#endif
} // namespace accat::auxilia

namespace accat::auxilia {
template <typename... Ts> class Variant;
enum class FormatPolicy : uint8_t;
template <typename Derived> struct Printable;
template <typename Derived> struct Viewable;
template <typename Ty>
  requires std::is_arithmetic_v<std::remove_cvref_t<Ty>>
bool is_integer(Ty &&value) noexcept {
  [[clang::musttail]] return std::trunc(std::forward<Ty>(value)) == value;
}
template <typename... Ts> struct match : Ts... {
  using Ts::operator()...;
};
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
template <typename Derived> struct AC_NOVTABLE Printable {
public:
  using string_type = string;

public:
  constexpr Printable() = default;
  constexpr Printable(const Printable &) = default;
  constexpr Printable(Printable &&) noexcept = default;
  constexpr auto operator=(const Printable &) -> Printable & = default;
  constexpr auto operator=(Printable &&) noexcept -> Printable & = default;

protected:
  constexpr ~Printable() = default;

private:
  [[nodiscard]] auto
  _to_string(const FormatPolicy &format_policy = FormatPolicy::kDefault) const
      -> string_type {
    return static_cast<const Derived *>(this)->to_string(format_policy);
  }

private:
  friend auto operator<<(std::ostream &os, const Printable &p)
      -> std::ostream & {
    return os << p._to_string();
  }
  [[nodiscard]]
  friend auto
  format_as(const Printable &p,
            const FormatPolicy &format_policy = FormatPolicy::kDefault)
      -> string_type {
    return p._to_string(format_policy);
  }
};

/// @interface Viewable
template <typename Derived> struct AC_NOVTABLE Viewable {
public:
  using string_view_type = std::string_view;

public:
  constexpr Viewable() = default;
  constexpr Viewable(const Viewable &) = default;
  constexpr Viewable(Viewable &&) noexcept = default;
  constexpr auto operator=(const Viewable &) -> Viewable & = default;
  constexpr auto operator=(Viewable &&) noexcept -> Viewable & = default;

protected:
  constexpr ~Viewable() = default;

private:
  [[nodiscard]] auto _to_string_view(
      const FormatPolicy &format_policy = FormatPolicy::kDefault) const
      -> string_view_type
    requires requires(const Derived &d) {
      {
        d.to_string_view(format_policy)
      } -> std::convertible_to<string_view_type>;
    }
  {
    [[clang::musttail]] return static_cast<const Derived *>(this)
        ->to_string_view(format_policy);
  }

private:
  friend auto operator<<(std::ostream &os, const Viewable &v)
      -> std::ostream & {
    return os << v._to_string_view();
  }
  [[nodiscard]]
  friend auto
  format_as(const Viewable &v,
            const FormatPolicy &format_policy = FormatPolicy::kDefault)
      -> string_view_type {
    [[clang::musttail]] return v._to_string_view(format_policy);
  }
};
} // namespace accat::auxilia

namespace std {
template <typename Derived>
  requires is_base_of_v<::accat::auxilia::Printable<Derived>,
                        Derived>
struct formatter<Derived> { // NOLINT(cert-dcl58-cpp)
  constexpr auto parse(format_parse_context &ctx) const noexcept {
    [[clang::musttail]] return ctx.begin();
  }
  template <typename FormatContext>
  constexpr auto format(const Derived &p, FormatContext &ctx) const {
    [[clang::musttail]] return format_to(
        ctx.out(), "{}", p.to_string(::accat::auxilia::FormatPolicy::kDefault));
  }
};
} // namespace std
