#pragma once

/// @file StatusOr.hpp
/// @brief A class that represents a value or an error status.
/// For more information of the class, please refer to the documentation of the
/// @file Status.hpp

#ifndef AUXILIA_STATUSOR_HPP
#  define AUXILIA_STATUSOR_HPP

#  include <algorithm>
#  include <type_traits>
#  include <memory>
#  include <utility>

#  if __cpp_lib_expected >= 202211L
#    include <expected>
#  endif

#  include "auxilia/base/config.hpp"
#  include "auxilia/base/format.hpp"
#  include "auxilia/meta/Monostate.hpp"
#  include "auxilia/meta/type_traits.hpp"

#  include "Status.hpp"

EXPORT_AUXILIA
namespace auxilia {
using ::std::in_place;
using ::std::in_place_t;

/// @brief A class that represents the status of a function call,
///          or a value.
///        it's designed to be as identical as possible to the
///         `absl::StatusOr` class.
/// @tparam Ty the type of the value
/// @note `absl::StatusOr` class is very optimized, yet comes with a
/// not-really-flexible api. It's recommended to use it rather than this
/// `StatusOr` if performance matters more.
template <typename Ty>
class AC_NODISCARD_REASON("returned value with status should not be ignored")
    StatusOr : public Printable,
               public details::StatusBase<StatusOr<Ty>> {
  static_assert(Storable<Ty>,
                "StatusOr should not be used with non-storable types.");
  static_assert(!std::is_const_v<Ty> && !std::is_volatile_v<Ty>,
                "storing cv-qualified variable is ill-formed");
  static_assert(!std::is_reference_v<Ty>,
                "haven't done yet; maybe C++26's std::expected may help...");
  static_assert(!is_specialization_v<Ty, StatusOr>,
                "Please do NOT store a StatusOr in a StatusOr.");
  static_assert(!std::is_same_v<Ty, Status>,
                "Please do NOT use Status as value type in StatusOr.");
  friend struct details::StatusBase<StatusOr<Ty>>;

public:
  using Code = details::Code;
  using enum Code;
  using base_type = details::StatusBase<StatusOr<Ty>>;
  using value_type = Ty;
  using base_type::code;
  using base_type::is_active;
  using base_type::is_return;
  using base_type::log_err;
  using base_type::log_err_;
  using base_type::ok;
  using base_type::raw_code;
  using base_type::operator bool;
  using base_type::ignore;

private:
  union {
    string_type my_message;
    value_type my_value;
  };

public:
  AC_NODISCARD inline constexpr StatusOr()
    requires(std::is_default_constructible_v<value_type>)
      : base_type(kOk) {
    std::ranges::construct_at(std::addressof(my_value));
  }
  AC_NODISCARD inline StatusOr(const Status &status) noexcept
      : base_type(status.my_code) {
    if (!is_active())
      std::ranges::construct_at(std::addressof(my_message), status.my_message);
  }
  AC_NODISCARD inline StatusOr(Status &&status) : base_type(status.my_code) {
    if (!is_active()) {
      std::ranges::construct_at(std::addressof(my_message),
                                std::move(status.my_message));
      AC_DEBUG_ONLY(status.my_message = "moved from");
    } else {
      AC_RUNTIME_ASSERT(false,
                        "an OkStatus is provided, yet no value is present")
    }
  }
  AC_NODISCARD inline StatusOr(const value_type &value) : base_type(kOk) {
    std::ranges::construct_at(std::addressof(my_value), value);
  }
  AC_NODISCARD inline StatusOr(value_type &&value) : base_type(kOk) {
    std::ranges::construct_at(std::addressof(my_value), std::move(value));
  }
  AC_NODISCARD inline StatusOr(StatusOr &&that) noexcept
      : base_type(that.my_code) {
    if (that.is_active()) {
      std::ranges::construct_at(std::addressof(my_value),
                                std::move(that.my_value));
      std::ranges::destroy_at(std::addressof(that.my_value));
      std::ranges::construct_at(std::addressof(that.my_message));
      AC_DEBUG_ONLY(that.my_message = "moved from");
    } else {
      std::ranges::construct_at(std::addressof(my_message),
                                std::move(that.my_message));
      AC_DEBUG_ONLY(that.my_message = "moved from");
    }
    that.my_code = kMovedFrom;
  }
  inline StatusOr &operator=(StatusOr &&that) noexcept {
    if (this == &that)
      return *this;

    if (is_active() && that.is_active()) {
      my_value = std::move(that.my_value);
      std::ranges::destroy_at(std::addressof(that.my_value));
      std::ranges::construct_at(std::addressof(that.my_message));
    } else if (!is_active() && !that.is_active()) {
      my_message = std::move(that.my_message);
    } else if (is_active() && !that.is_active()) {
      std::ranges::destroy_at(std::addressof(my_value));
      std::ranges::construct_at(std::addressof(my_message),
                                std::move(that.my_message));
    } else {
      std::ranges::destroy_at(std::addressof(my_message));
      std::ranges::construct_at(std::addressof(my_value),
                                std::move(that.my_value));
      std::ranges::destroy_at(std::addressof(that.my_value));
      std::ranges::construct_at(std::addressof(that.my_message));
    }

    this->my_code = std::exchange(that.my_code, kMovedFrom);
    AC_DEBUG_ONLY(that.my_message = "moved from");

    return *this;
  }
  ~StatusOr() noexcept {
    if (is_active())
      std::ranges::destroy_at(std::addressof(my_value));
    else
      std::ranges::destroy_at(std::addressof(my_message));
  }

  template <typename... Args>
  explicit inline StatusOr(in_place_t, Args &&...args) noexcept(
      noexcept(std::ranges::construct_at(std::addressof(my_value),
                                         std::forward<Args>(args)...)))
      : base_type(kOk) {
    std::ranges::construct_at(std::addressof(my_value),
                              std::forward<Args>(args)...);
  }
  AC_NODISCARD std::string_view message() const [[clang::lifetimebound]] {
    if (!is_active())
      return my_message;
    else
      return "";
  }
  AC_NODISCARD auto &raw_message() const AC_NOEXCEPT {
    AC_RUNTIME_ASSERT(!is_active())
    return my_message;
  }
#  define AC_STATUSOR_DELETE                                                   \
    AC_DELETE_WITH_MESSAGE(                                                    \
        "Constructing a StatusOr<Ty> from a StatusOr<Uy> where Ty != Uy is "   \
        "not allowed. Please use explicit function `as_status()` if you "      \
        "want to return the Status, or use monadic functions like "            \
        "`and_then()` or `or_else()` to convert the StatusOr<Uy> to "          \
        "StatusOr<Ty>.")

  template <typename U> StatusOr(const StatusOr<U> &) = AC_STATUSOR_DELETE;
  template <typename U> StatusOr(StatusOr<U> &&) = AC_STATUSOR_DELETE;
  template <typename U>
  auto operator=(const StatusOr<U> &) = AC_STATUSOR_DELETE;
  template <typename U> auto operator=(StatusOr<U> &&) = AC_STATUSOR_DELETE;
#  undef AC_STATUSOR_DELETE

public:
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline decltype(auto) value(this auto &&self) {
    AC_RUNTIME_ASSERT(self.is_active(),
                      "Cannot dereference a status that is not OK.");
    return std::forward_like<decltype(self)>(self.my_value);
  }
#  else
  AC_NODISCARD
  inline value_type value() & {
    AC_RUNTIME_ASSERT(is_active(),
                      "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline const value_type value() const & {
    AC_RUNTIME_ASSERT(is_active(),
                      "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline value_type value() && {
    AC_RUNTIME_ASSERT(is_active(),
                      "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
  AC_NODISCARD
  inline const value_type &&value() const && {
    AC_RUNTIME_ASSERT(is_active(),
                      "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// this function returns a copy, which may not you want; call `std::move`
  /// first to get the rvalue reference and to avoid unnecessary copy.
  AC_NODISCARD AC_CONSTEXPR20 inline value_type
  value_or(this auto &&self, value_type &&default_value) {
    return self.is_active() ? std::forward_like<decltype(self)>(self.my_value)
                            : static_cast<value_type>(
                                  std::forward<value_type>(default_value));
  }
#  else

#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline constexpr decltype(auto) operator*(this auto &&self) AC_NOEXCEPT {
    AC_PRECONDITION(self.is_active(),
                    "Cannot dereference a status that is not OK.");
    return std::forward_like<decltype(self)>(self.my_value);
  }
#  else

#  endif

  AC_NODISCARD
  inline constexpr auto operator->() AC_NOEXCEPT->value_type * {
    AC_PRECONDITION(is_active(), "Cannot dereference a status that is not OK.");
    return std::addressof(my_value);
  }
  AC_NODISCARD
  inline constexpr auto operator->() const AC_NOEXCEPT->const value_type * {
    AC_PRECONDITION(is_active(), "Cannot dereference a status that is not OK.");
    return std::addressof(my_value);
  }

public:
  /// this is not optimized at all -- use with caution.
  constexpr inline void
  swap(StatusOr &that) noexcept(std::is_nothrow_swappable_v<value_type> &&
                                std::is_nothrow_swappable_v<base_type>) {
    if constexpr (!std::is_swappable_v<value_type>)
      always_false<value_type>("is not swappable");

    if (is_active() && that.is_active()) {
      ::std::swap(my_value, that.my_value);
    } else if (!is_active() && !that.is_active()) {
      ::std::swap(my_message, that.my_message);
    } else if (is_active() && !that.is_active()) {
      auto temp_value = std::move(my_value);
      std::ranges::destroy_at(std::addressof(my_value));
      std::ranges::construct_at(std::addressof(my_message),
                                std::move(that.my_message));

      std::ranges::destroy_at(std::addressof(that.my_message));
      std::ranges::construct_at(std::addressof(that.my_value),
                                std::move(temp_value));
    } else {
      auto temp_value = std::move(that.my_value);
      std::ranges::destroy_at(std::addressof(that.my_value));
      std::ranges::construct_at(std::addressof(that.my_message),
                                std::move(my_message));

      std::ranges::destroy_at(std::addressof(my_message));
      std::ranges::construct_at(std::addressof(my_value),
                                std::move(temp_value));
    }
    base_type::swap(that);
  }

  [[nodiscard]] auto
  to_string(FormatPolicy policy = FormatPolicy::kDefault) const -> string_type {
    if (!this->is_active()) {
      if (policy == FormatPolicy::kBrief)
        return my_message;
      else
        return Format("{}: {}", details::to_string(code()), my_message);
    }

    if constexpr (std::is_base_of_v<Printable, value_type>)
      return my_value.to_string(policy);
    else if constexpr (requires { Format("{}", my_value); })
      return Format("{}", my_value);
    else
      return Format("StatusOr<{}>: <unformattable>", typeid(value_type).name());
  }
  /// @deprecated just uses operator=(StatusOr &&that) instead.
  [[clang::reinitializes,
    deprecated(
        "just uses operator=(StatusOr &&that) instead")]] constexpr inline auto
  reset() AC_NOEXCEPT {
    return reset({});
  }
  [[clang::reinitializes]] constexpr inline auto reset(value_type &&value)
      AC_NOEXCEPT {
    if (!is_active()) {
      std::ranges::destroy_at(std::addressof(my_message));
      std::ranges::construct_at(std::addressof(my_value), std::move(value));
    } else {
      my_value = std::move(value);
    }
    this->my_code = kOk;
    return *this;
  }

public:
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD AC_FLATTEN inline constexpr auto as_status(this auto &&self)
      AC_NOEXCEPT -> Status {
    if (self.is_active())
      return {self.my_code};
    else
      return {self.my_code, std::forward_like<decltype(self)>(self.my_message)};
  }
#  else

#  endif
#  define AC_DOLL_ASSERT(_name_)                                               \
    static_assert(                                                             \
        !is_specialization_v<std::invoke_result_t<F, value_type>, StatusOr>,   \
        "Playing Russian doll with StatusOr is not allowed: `" #_name_ "()` "  \
        "called to return a StatusOr inside another StatusOr. "                \
        "Please use `and_then()` or `or_else()` instead.");
#  define AC_STATUSOR_CONSUME_METHOD(_name_)                                   \
    static_assert(                                                             \
        std::is_rvalue_reference_v<decltype(self)>,                            \
        "Method `" #_name_ "` is designed to consume the StatusOr, "           \
        "but it is called on an lvalue. Please call it on an rvalue "          \
        "or use the corresponding method that does not consume the "           \
        "StatusOr.");                                                          \
    AC_DEFER {                                                                 \
      /* we only do this for checking, usually the monadic operations just     \
       * consume itself so the leftover statusor should be an unnnamed obj, so \
       * this wont be called*/                                                 \
      AC_DEBUG_BLOCK {                                                         \
        if (self.is_active()) {                                                \
          std::ranges::destroy_at(std::addressof(self.my_value));              \
          std::ranges::construct_at(std::addressof(self.my_message),           \
                                    "status accessed after moved from.");      \
        }                                                                      \
        self.my_code = kMovedFrom;                                             \
      };                                                                       \
    };

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the value stored in the StatusOr
  /// if it is OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Ty -> StatusOr<Uy>
  /// @return a StatusOr<Uy> that is the result of the function call
  template <typename F,
            typename... Args,
            typename R = std::invoke_result_t<F, value_type, Args...>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(this auto &&self, F &&f, Args &&...args) -> std::
      conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<value_type>> {
    AC_STATUSOR_CONSUME_METHOD(and_then)
    if (!self.is_active())
      // `forward` is sufficient since `as_status` is a perfect function itself.
      return {std::forward<decltype(self)>(self).as_status()};
    else
      return std::invoke(std::forward<F>(f),
                         std::forward_like<decltype(self)>(self.my_value),
                         std::forward<Args>(args)...);
  }
  /// @brief NTTP version of `and_then` above.
  /// @tparam CPO NTTP callable (e.g., a member function pointer)
  /// @return a StatusOr<Uy> that is the result of the function call
  template <
      auto CPO,
      typename... Args,
      typename R = std::invoke_result_t<decltype(CPO), value_type, Args...>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(this auto &&self, Args &&...args) -> std::
      conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<value_type>> {
    AC_STATUSOR_CONSUME_METHOD(and_then)

    if (!self.is_active())
      return {std::forward<decltype(self)>(self).as_status()};
    else
      return std::invoke(CPO,
                         std::forward_like<decltype(self)>(self.my_value),
                         std::forward<Args>(args)...);
  }
#  else

#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the status stored in the StatusOr
  /// if it is not OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Status -> StatusOr, or simply Ty.
  /// @return a StatusOr<Ty> that is the result of/from the function call
  template <typename F, typename... Args>
    requires std::is_invocable_r_v<StatusOr<value_type>, F, Status, Args...>
  auto or_else(this auto &&self, F &&f, Args &&...args) -> StatusOr {
    AC_STATUSOR_CONSUME_METHOD(or_else)
    if (self.is_active())
      return std::forward<decltype(self)>(self);

    return std::invoke(std::forward<F>(f),
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }

  template <auto CPO, typename... Args>
    requires std::
        is_invocable_r_v<StatusOr<value_type>, decltype(CPO), Status, Args...>
      auto or_else(this auto &&self, Args &&...args) -> StatusOr {
    AC_STATUSOR_CONSUME_METHOD(or_else)
    if (self.is_active())
      return std::forward<decltype(self)>(self);

    return std::invoke(CPO,
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }

#  else

#  endif
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the value stored in the StatusOr
  /// if it is OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Ty -> Uy, where Uy cannot be a StatusOr<Ry>
  /// @return a StatusOr<Uy> that is the result of the function call
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, value_type, Args...> &&
             (!std::is_void_v<std::invoke_result_t<F, value_type, Args...>>)
  auto transform(this auto &&self, F &&f, Args &&...args)
      -> StatusOr<std::invoke_result_t<F, value_type, Args...>> {
    AC_DOLL_ASSERT(transform)
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.is_active()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    return {std::invoke(std::forward<F>(f),
                        std::forward_like<decltype(self)>(self.my_value),
                        std::forward<Args>(args)...)};
  }
  /// @copydoc transform
  /// @param f Ty -> void
  /// @return StatusOr<Monostate>
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, value_type, Args...> &&
             std::is_void_v<std::invoke_result_t<F, value_type, Args...>>
  auto transform(this auto &&self, F &&f, Args &&...args)
      -> StatusOr<Monostate> {
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.is_active()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    std::invoke(std::forward<F>(f),
                std::forward_like<decltype(self)>(self.my_value),
                std::forward<Args>(args)...);
    return {};
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), value_type, Args...> &&
             (!std::is_void_v<
                 std::invoke_result_t<decltype(CPO), value_type, Args...>>)
  auto transform(this auto &&self, Args &&...args)
      -> StatusOr<std::invoke_result_t<decltype(CPO), value_type, Args...>> {
    using F = decltype(CPO);
    AC_DOLL_ASSERT(transform)
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.is_active()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    return {std::invoke(CPO,
                        std::forward_like<decltype(self)>(self.my_value),
                        std::forward<Args>(args)...)};
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), value_type, Args...> &&
             std::is_void_v<
                 std::invoke_result_t<decltype(CPO), value_type, Args...>>
  auto transform(this auto &&self, Args &&...args) -> StatusOr<Monostate> {
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.is_active()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    std::invoke(CPO,
                std::forward_like<decltype(self)>(self.my_value),
                std::forward<Args>(args)...);
    return {};
  }

#  else
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the status stored in the StatusOr
  /// if it is not OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Status -> Status
  /// @return a StatusOr<Ty> that is the result of the function call
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, Status, Args...> &&
             (!std::is_void_v<std::invoke_result_t<F, Status, Args...>>)
  auto transform_error(this auto &&self, F &&f, Args &&...args) -> StatusOr {
    static_assert(std::is_rvalue_reference_v<decltype(self)>, "bad call.");
    if (self.is_active()) {
      return std::forward<decltype(self)>(self);
    }
    return std::invoke(std::forward<F>(f),
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }
  /// @copydoc transform_error
  /// @param f Status -> void
  /// @return StatusOr<Ty> with err unmodified
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, Status, Args...> &&
             std::is_void_v<std::invoke_result_t<F, Status, Args...>>
  auto transform_error(this auto &&self, F &&f, Args &&...args) -> StatusOr {
    AC_STATUSOR_CONSUME_METHOD(transform_error)
    if (!self.is_active()) {
      std::invoke(std::forward<F>(f),
                  std::forward<decltype(self)>(self).as_status(),
                  std::forward<Args>(args)...);
    }
    return std::forward<decltype(self)>(self);
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), Status, Args...> &&
             (!std::is_void_v<
                 std::invoke_result_t<decltype(CPO), Status, Args...>>)
  auto transform_error(this auto &&self, Args &&...args) -> StatusOr {
    static_assert(std::is_rvalue_reference_v<decltype(self)>, "bad call.");
    if (self.is_active()) {
      return std::forward<decltype(self)>(self);
    }
    return std::invoke(CPO,
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), Status, Args...> &&
             std::is_void_v<
                 std::invoke_result_t<decltype(CPO), Status, Args...>>
  auto transform_error(this auto &&self, Args &&...args) -> StatusOr {
    AC_STATUSOR_CONSUME_METHOD(transform_error)
    if (!self.is_active()) {
      std::invoke(CPO,
                  std::forward<decltype(self)>(self).as_status(),
                  std::forward<Args>(args)...);
    }
    return std::forward<decltype(self)>(self);
  }
#  else
#  endif
#  if AC_HAS_EXPLICIT_THIS_PARAMETER

  template <typename F>
  auto inspect(this auto &&self, F &&f) -> decltype(auto) {
    if (self.is_active()) {
      static_assert(
          std::is_invocable_v<F, const value_type &>,
          "Inspect function must be invocable with const value_type&");
      std::invoke(std::forward<F>(f), self.my_value);
    }
    return std::forward<decltype(self)>(self);
  }
  template <auto CPO> auto inspect(this auto &&self) -> decltype(auto) {
    using F = decltype(CPO);
    if (self.is_active()) {
      static_assert(
          std::is_invocable_v<F, const value_type &>,
          "Inspect function must be invocable with const value_type&");
      std::invoke(CPO, self.my_value);
    }
    return std::forward<decltype(self)>(self);
  }
  template <typename F>
  auto inspect_error(this auto &&self, F &&f) -> decltype(auto) {
    if (!self.is_active()) {
      static_assert(std::is_invocable_v<F, const Status &>,
                    "Inspect function must be invocable with const Status&");
      std::invoke(std::forward<F>(f), self.as_status());
    }
    return std::forward<decltype(self)>(self);
  }
  template <auto CPO> auto inspect_error(this auto &&self) -> decltype(auto) {
    using F = decltype(CPO);
    if (!self.is_active()) {
      static_assert(std::is_invocable_v<F, const Status &>,
                    "Inspect function must be invocable with const Status&");
      std::invoke(CPO, self.as_status());
    }
    return std::forward<decltype(self)>(self);
  }

#  else
#  endif

  /// @brief Converts the StatusOr to a std::optional; ownership is transferred.
  constexpr inline auto to_optional(this auto &&self) AC_NOEXCEPT
      -> std::optional<value_type> {
    AC_STATUSOR_CONSUME_METHOD(to_optional)
    if (self.is_active()) {
      return std::make_optional(
          std::forward_like<decltype(self)>(self.my_value));
    } else {
      return std::nullopt;
    }
  }
#  if __cpp_lib_expected >= 202211L
  constexpr inline auto to_expected(this auto &&self) AC_NOEXCEPT
      -> std::expected<value_type, Status> {
    AC_STATUSOR_CONSUME_METHOD(to_expected)
    if (self.is_active()) {
      return std::expected(std::forward_like<decltype(self)>(self.my_value));
    } else {
      return std::unexpected(std::forward<decltype(self)>(self).as_status());
    }
  }
#  endif

#  undef AC_STATUSOR_CONSUME_METHOD
#  undef AC_STATUSOR_DELETE
#  undef AC_DOLL_ASSERT
};

template <typename Ty> StatusOr(Ty &&value) -> StatusOr<std::decay_t<Ty>>;

/// it's a bit controversial here: the usage of `std::decay_t` may cause
/// potential copy happens(without `std::decay_t`, if the parameter is an lvalue
/// reference, `static_assert` fails), but I choose to be consistent with
/// standard library's `make_optional`.
template <typename Ty>
inline constexpr auto OkStatus(Ty &&value) noexcept(
    noexcept(StatusOr<std::decay_t<Ty>>{std::forward<Ty>(value)})) {
  return StatusOr<std::decay_t<Ty>>{std::forward<Ty>(value)};
}

/// emplace-like `OkStatus` function
template <typename Ty, typename... Args>
inline constexpr auto OkStatus(Args &&...args) noexcept(noexcept(StatusOr<Ty>{
    in_place, std::forward<Args>(args)...})) {
  return StatusOr<Ty>{in_place, std::forward<Args>(args)...};
}
} // namespace auxilia

#endif // AUXILIA_STATUSOR_HPP
