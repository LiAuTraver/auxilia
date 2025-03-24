#pragma once

#include <type_traits>
#include <utility>

#include "./config.hpp"
#include "./Monostate.hpp"
#include "./format.hpp"
EXPORT_AUXILIA
namespace accat::auxilia {
/// @brief A simple variant wrapper class around @link std::variant @endlink for
/// convenience when evaluating expressions, especially when the operation was
/// `to_string` or check the type's name when debugging.
/// @note exception-free variant wrapper
template <typename... Types> class Variant : public Printable, public Viewable {
  static_assert(Variantable<Types...>, "Types must be variantable");

  using monostate_like_type = std::tuple_element_t<0, std::tuple<Types...>>;
  using self_type = Variant<Types...>;

public:
  using variant_type = std::variant<Types...>;
  using string_type = typename Printable::string_type;
  using string_view_type = typename Viewable::string_view_type;

public:
  inline constexpr Variant() = default;

  template <typename Ty>
    requires(!std::same_as<std::decay_t<Ty>, Variant> &&
             (std::is_same_v<std::decay_t<Ty>, Types> || ...))
  Variant(Ty &&value) : my_variant(std::forward<Ty>(value)) {
    static_assert(
        (std::is_same_v<std::decay_t<Ty>, Types> || ...),
        "The type of the value must be one of the types in the variant, "
        "otherwise if the variant failed to construct, the error message will "
        "be horrible and hard to find the cause. please use explicit type "
        "constructor.");
  }

  Variant(const Variant &) = default;
  Variant(Variant &&that) noexcept : my_variant(std::move(that.my_variant)) {
    that.my_variant.template emplace<monostate_like_type>();
  }
  Variant &operator=(const Variant &) = default;
  Variant &operator=(Variant &&that) noexcept {
    my_variant = std::move(that.my_variant);
    that.my_variant.template emplace<monostate_like_type>();
    return *this;
  }
  bool operator==(const Variant &that) const noexcept {
    return my_variant == that.my_variant;
  }
  bool operator!=(const Variant &that) const noexcept {
    return my_variant != that.my_variant;
  }
  ~Variant() = default;

public:
  [[clang::reinitializes]] auto set(Variant &&that = {}) noexcept -> Variant & {
    my_variant = std::move(that.my_variant);
    that.my_variant.template emplace<monostate_like_type>();
    return *this;
  }

#if AC_HAS_EXPLICIT_THIS_PARAMETER
  template <typename Callable>
  auto visit(this auto &&self, Callable &&callable) -> decltype(auto) {
    using ReturnType = decltype(std::forward<Callable>(callable)(
        std::declval<variant_type>()));
    static_assert(std::is_default_constructible_v<ReturnType> ||
                      std::is_void_v<ReturnType>,
                  "ReturnType must be default constructible");
    if constexpr (std::is_void_v<ReturnType>) {
      return std::visit(std::forward<Callable>(callable), self.my_variant);
    } else {
      return self.is_valid()
                 ? static_cast<ReturnType>(std::visit(
                       std::forward<Callable>(callable), self.my_variant))
                 : ReturnType{};
    }
  }
#else
  template <typename Callable>
  auto visit(Callable &&callable) const -> decltype(auto) {
    using ReturnType = decltype(std::forward<Callable>(callable)(
        std::declval<variant_type>()));
    static_assert(std::is_default_constructible_v<ReturnType> ||
                      std::is_void_v<ReturnType>,
                  "ReturnType must be default constructible");
    if constexpr (std::is_void_v<ReturnType>) {
      return std::visit(std::forward<Callable>(callable), my_variant);
    } else {
      return is_valid() ? static_cast<ReturnType>(std::visit(
                              std::forward<Callable>(callable), my_variant))
                        : ReturnType{};
    }
  }
#endif

  auto type_name() const {
    return is_valid() ? this->visit([]([[maybe_unused]] const auto &value)
                                        -> string_view_type {
      // doesn't require rtti
      return typeid(value).name();
    })
                      : "invalid state"sv;
  }
  auto index() const noexcept { return my_variant.index(); }
  template <typename... Args>
    requires requires {
      std::declval<variant_type>().template emplace<Args...>(
          std::declval<Args>()...);
    }
  auto emplace(Args &&...args) -> decltype(auto) {
    return my_variant.template emplace<Args...>(std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires {
      std::declval<variant_type>().template emplace<Args...>(
          std::declval<Args>()...);
    }
  auto emplace_and_then(Args &&...args) -> decltype(auto) {
    my_variant.template emplace<Args...>(std::forward<Args>(args)...);
    return *this;
  }
  template <typename Args>
    requires requires { std::declval<variant_type>().template emplace<Args>(); }
  constexpr auto
  emplace() noexcept(noexcept(my_variant.template emplace<Args>()))
      -> decltype(auto) {
    return my_variant.template emplace<Args>();
  }
#if AC_HAS_EXPLICIT_THIS_PARAMETER
  template <typename Ty>
  inline constexpr auto get(this auto &&self) noexcept(false)
      -> decltype(auto) {
    return std::get<Ty>(self.my_variant);
  }
  template <typename Ty>
  inline constexpr auto get_if(this auto &&self) noexcept -> decltype(auto) {
    return std::get_if<Ty>(&self.my_variant);
  }
#else
  template <typename Ty> inline constexpr auto get() noexcept(false) {
    return std::get<Ty>(my_variant);
  }
  template <typename Ty> inline constexpr auto get_if() noexcept {
    return std::get_if<Ty>(&my_variant);
  }
  template <typename Ty> inline constexpr auto get_if() const noexcept {
    return std::get_if<Ty>(&my_variant);
  }
  template <typename Ty> inline constexpr auto get_if() const noexcept {
    return std::get_if<Ty>(&my_variant);
  }
#endif
  constexpr auto swap(Variant &that) noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Types...>,
                         std::is_nothrow_swappable<Types...>>) -> Variant & {
    my_variant.swap(that.my_variant);
    return *this;
  }
  [[clang::reinitializes]]
#if AC_HAS_EXPLICIT_THIS_PARAMETER
  constexpr auto clear(this auto &&self) noexcept(
      noexcept(self.my_variant.template emplace<monostate_like_type>()))
      -> decltype(auto) {
    self.my_variant.template emplace<monostate_like_type>();
    return self;
  }
#else
  constexpr auto
  clear() noexcept(noexcept(my_variant.template emplace<monostate_like_type>()))
      -> decltype(auto) {
    my_variant.template emplace<monostate_like_type>();
    return *this;
  }
#endif

  constexpr auto empty() const noexcept -> bool {
    return my_variant.index() == 0;
  }
  auto underlying_string(const FormatPolicy &format_policy =
                             FormatPolicy::kDefault) const -> string_type {
    return this->visit([&](const auto &value) -> string_type {
      return value.to_string(format_policy);
    });
  }
  template <typename Ty> inline auto is_type() const noexcept -> bool {
    return std::holds_alternative<Ty>(my_variant);
  }

private:
  variant_type my_variant{monostate_like_type{}};

private:
  inline bool is_valid() const noexcept {
    return my_variant.index() != std::variant_npos;
  }

public:
  constexpr auto to_string(const FormatPolicy &format_policy) const
      -> string_type {
    return getTypedStrImpl(format_policy);
  }
  constexpr auto to_string_view(const FormatPolicy &format_policy) const
      -> string_view_type {
    return getTypedStrImpl(format_policy);
  }

private:
  constexpr auto
  getTypedStrImpl(const FormatPolicy &format_policy) const noexcept -> const
      char * {
#ifdef __cpp_rtti
    if (format_policy == FormatPolicy::kDefault) {
      return typeid(decltype(*this)).name();
    } else if (format_policy == FormatPolicy::kDetailed) {
      return typeid(decltype(*this))
#  if _WIN32
          .raw_name();
#  else
          .name(); // g++ doesn't support raw_name()
#  endif
    }
#else
    return __PRETTY_FUNCTION__;
#endif
  }
};
} // namespace accat::auxilia
