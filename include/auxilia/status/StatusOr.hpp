#pragma once

/// @file StatusOr.hpp
/// @brief A class that represents a value or an error status.
/// For more information of the class, please refer to the documentation of the
/// @file Status.hpp

#ifndef AUXILIA_STATUSOR_HPP
#  define AUXILIA_STATUSOR_HPP

#  include <type_traits>
#  include <utility>

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
template <typename Ty> class StatusOr : public Status {
  static_assert(Storable<Ty>,
                "StatusOr should not be used with non-storable types.");
  static_assert(!is_specialization_v<Ty, StatusOr>,
                "Please do NOT store a StatusOr in a StatusOr.");
  static_assert(!std::is_same_v<Ty, Status>,
                "Please do NOT use Status as value type in StatusOr.");

public:
  using base_type = Status;
  using value_type = Ty;
  using rvoff_value_t = std::remove_cv_t<Ty>;

public:
  AC_NODISCARD inline constexpr StatusOr() = default;
  AC_NODISCARD inline StatusOr(const Status &status) : base_type(status) {}
  AC_NODISCARD inline StatusOr(Status &&status)
      : base_type(std::move(status)) {}
  AC_NODISCARD inline StatusOr(const value_type &value)
      : base_type(kOk), my_value(value) {}
  AC_NODISCARD inline StatusOr(value_type &&value)
      : base_type(kOk), my_value(std::move(value)) {}
  AC_NODISCARD inline StatusOr(const Status &status, const value_type &value)
      : base_type(status), my_value(value) {}
  AC_NODISCARD inline StatusOr(Status &&status, value_type &&value)
      : base_type(std::move(status)), my_value(std::move(value)) {}
  AC_NODISCARD inline StatusOr(const StatusOr &that)
      : base_type(that), my_value(that.my_value) {}
  AC_NODISCARD inline StatusOr(StatusOr &&that) noexcept
      : base_type(std::move(that)), my_value(std::move(that.my_value)) {}
  inline StatusOr &operator=(const StatusOr &that) {
    base_type::operator=(that);
    my_value = that.my_value;
    return *this;
  }
  inline StatusOr &operator=(StatusOr &&that) noexcept {
    base_type::operator=(std::move(that));
    my_value = std::move(that.my_value);
    return *this;
  }
  ~StatusOr() noexcept = default;

  template <typename... Args>
  explicit inline StatusOr(in_place_t, Args &&...args) noexcept(
      noexcept(value_type{std::forward<Args>(args)...}))
      : base_type(kOk), my_value(std::forward<Args>(args)...) {}

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
  StatusOr &operator=(const StatusOr<U> &) = AC_STATUSOR_DELETE;
  template <typename U>
  StatusOr &operator=(StatusOr<U> &&) = AC_STATUSOR_DELETE;
#  undef AC_STATUSOR_DELETE

public:
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline decltype(auto) value(this auto &&self) {
    AC_RUNTIME_ASSERT(self.ok() or self.is_return(),
                      "Cannot dereference a status that is not OK.");
    return std::forward<decltype(self)>(self).my_value;
  }
#  else
  AC_NODISCARD
  inline value_type value() & {
    AC_RUNTIME_ASSERT(ok() or is_return(),
                      "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline const value_type value() const & {
    AC_RUNTIME_ASSERT(ok() or is_return(),
                      "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline value_type value() && {
    AC_RUNTIME_ASSERT(ok() or is_return(),
                      "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
  AC_NODISCARD
  inline const value_type &&value() const && {
    AC_RUNTIME_ASSERT(ok() or is_return(),
                      "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// this function returns a copy, which may not you want; call `std::move`
  /// first or just use `.rvalue()` to get the rvalue reference and move it
  /// out to avoid unnecessary copy.
  AC_NODISCARD AC_CONSTEXPR20 inline value_type
  value_or(this auto &&self, rvoff_value_t &&default_value) {
    return self.ok() ? std::forward<decltype(self)>(self).my_value
                     : static_cast<value_type>(
                           std::forward<rvoff_value_t>(default_value));
  }
#  else
  AC_NODISCARD AC_CONSTEXPR20 inline value_type
  value_or(rvoff_value_t &&default_value) const & {
    return ok() ? my_value
                : static_cast<value_type>(
                      std::forward<rvoff_value_t>(default_value));
  }
  AC_NODISCARD AC_CONSTEXPR20 inline value_type
  value_or(rvoff_value_t &&default_value) && {
    return ok() ? std::move(my_value)
                : static_cast<value_type>(
                      std::forward<rvoff_value_t>(default_value));
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline constexpr decltype(auto) operator*(this auto &&self) AC_NOEXCEPT {
    AC_PRECONDITION(self.ok() or self.is_return(),
                    "Cannot dereference a status that is not OK.");
    return std::forward<decltype(self)>(self).my_value;
  }
#  else
  AC_NODISCARD
  inline constexpr value_type &operator*() & AC_NOEXCEPT {
    AC_PRECONDITION(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline constexpr const value_type &operator*() const &AC_NOEXCEPT {
    AC_PRECONDITION(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return my_value;
  }
#  endif

  AC_NODISCARD
  inline constexpr auto operator->() AC_NOEXCEPT->value_type * {
    AC_PRECONDITION(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return std::addressof(my_value);
  }
  AC_NODISCARD
  inline constexpr auto operator->() const AC_NOEXCEPT->const value_type * {
    AC_PRECONDITION(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return std::addressof(my_value);
  }

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD AC_FLATTEN inline constexpr auto as_status(this auto &&self)
      AC_NOEXCEPT -> decltype(auto) {
    return (static_cast<base_type>(std::forward<decltype(self)>(self)));
  }
#  else
  AC_NODISCARD AC_FLATTEN inline constexpr auto as_status() & AC_NOEXCEPT {
    return static_cast<base_type &>(*this);
  }
  AC_NODISCARD AC_FLATTEN inline constexpr const auto
  as_status() const &AC_NOEXCEPT {
    return static_cast<const base_type &>(*this);
  }
  AC_NODISCARD AC_FLATTEN inline constexpr auto as_status() && AC_NOEXCEPT {
    return static_cast<base_type &&>(*this);
  }
  AC_NODISCARD AC_FLATTEN inline constexpr const auto
  as_status() const &&AC_NOEXCEPT {
    return static_cast<const base_type &&>(*this);
  }
#  endif
#  define AC_DOLL_ASSERT(_name_)                                               \
    static_assert(                                                             \
        !is_specialization_v<std::invoke_result_t<F, Ty>, StatusOr>,           \
        "Playing Russian doll with StatusOr is not allowed: `" #_name_ "()` "  \
        "called to return a StatusOr inside another StatusOr. "                \
        "Please use `and_then()` or `or_else()` instead.");
#  define AC_STATUSOR_CONSUME_METHOD(_name_)                                   \
    static_assert(                                                             \
        std::is_rvalue_reference_v<decltype(self)>,                            \
        "Method `" #_name_ "` is designed to consume the StatusOr, "           \
        "but it is called on an lvalue. Please call it on an rvalue "          \
        "or use the corresponding method that does not consume the "           \
        "StatusOr.");
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the value stored in the StatusOr
  /// if it is OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Ty -> StatusOr<Uy>
  /// @return a StatusOr<Uy> that is the result of the function call
  template <typename F,
            typename... Args,
            typename R = std::invoke_result_t<F, Ty, Args...>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(this auto &&self, F &&f, Args &&...args)
      -> std::conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
    AC_STATUSOR_CONSUME_METHOD(and_then)
    if (!self.ok())
      // `forward` is sufficient since `as_status` is a perfect function itself.
      return {std::forward<decltype(self)>(self).as_status()};
    else
      return std::invoke(std::forward<F>(f),
                         std::forward<decltype(self)>(self).my_value,
                         std::forward<Args>(args)...);
  }
  /// @brief NTTP version of `and_then` above.
  /// @tparam CPO NTTP callable (e.g., a member function pointer)
  /// @return a StatusOr<Uy> that is the result of the function call
  template <auto CPO,
            typename... Args,
            typename R = std::invoke_result_t<decltype(CPO), Ty, Args...>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(this auto &&self, Args &&...args)
      -> std::conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {

    AC_STATUSOR_CONSUME_METHOD(and_then)

    if (!self.ok())
      return {std::forward<decltype(self)>(self).as_status()};
    else
      return std::invoke(CPO,
                         std::forward<decltype(self)>(self).my_value,
                         std::forward<Args>(args)...);
  }
#  else
  // template <typename F, typename R = std::invoke_result_t<F, Ty>>
  //   requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  // auto and_then(F &&f) & -> std::
  //     conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
  //   if (!ok()) {
  //     return {as_status()};
  //   }
  //   return std::invoke(std::forward<F>(f), my_value);
  // }

  // template <typename F, typename R = std::invoke_result_t<F, Ty>>
  //   requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  // auto and_then(F &&f) const & -> std::
  //     conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
  //   if (!ok()) {
  //     return {as_status()};
  //   }
  //   return std::invoke(std::forward<F>(f), my_value);
  // }

  template <typename F, typename R = std::invoke_result_t<F, Ty>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(F &&f) && -> std::
      conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    return std::invoke(std::forward<F>(f), std::move(my_value));
  }

  template <typename F, typename R = std::invoke_result_t<F, Ty>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(F &&f) const && -> std::
      conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    return std::invoke(std::forward<F>(f), std::move(my_value));
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the status stored in the StatusOr
  /// if it is not OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Status -> StatusOr<Ty>, or simply Ty.
  /// @return a StatusOr<Ty> that is the result of/from the function call
  template <typename F, typename... Args>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Status, Args...>
  auto or_else(this auto &&self, F &&f, Args &&...args) -> StatusOr<Ty> {
    AC_STATUSOR_CONSUME_METHOD(or_else)
    if (self.ok()) {
      return std::forward<decltype(self)>(self);
    }
    return std::invoke(std::forward<F>(f),
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_r_v<StatusOr<Ty>, decltype(CPO), Status, Args...>
  auto or_else(this auto &&self, Args &&...args) -> StatusOr<Ty> {
    AC_STATUSOR_CONSUME_METHOD(or_else)
    if (self.ok()) {
      return std::forward<decltype(self)>(self);
    }
    return std::invoke(CPO,
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }

#  else
  // template <typename F>
  //   requires std::is_invocable_r_v<StatusOr<Ty>, F, Status>
  // auto or_else(F &&f) & -> StatusOr<Ty> {
  //   if (ok()) {
  //     return *this;
  //   }
  //   return std::invoke(std::forward<F>(f), as_status());
  // }
  // template <typename F>
  //   requires std::is_invocable_r_v<StatusOr<Ty>, F, Status>
  // auto or_else(F &&f) const & -> StatusOr<Ty> {
  //   if (ok()) {
  //     return *this;
  //   }
  //   return std::invoke(std::forward<F>(f), as_status());
  // }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Status>
  auto or_else(F &&f) && -> StatusOr<Ty> {
    if (ok()) {
      return std::move(*this);
    }
    return std::invoke(std::forward<F>(f), as_status());
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Status>
  auto or_else(F &&f) const && -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f), as_status());
  }
#  endif
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the value stored in the StatusOr
  /// if it is OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Ty -> Uy, where Uy cannot be a StatusOr<Ry>
  /// @return a StatusOr<Uy> that is the result of the function call
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, Ty, Args...> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty, Args...>>)
  auto transform(this auto &&self, F &&f, Args &&...args)
      -> StatusOr<std::invoke_result_t<F, Ty, Args...>> {
    AC_DOLL_ASSERT(transform)
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.ok()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    return {std::invoke(std::forward<F>(f),
                        std::forward<decltype(self)>(self).my_value,
                        std::forward<Args>(args)...)};
  }
  /// @copydoc transform
  /// @param f Ty -> void
  /// @return StatusOr<Monostate>
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, Ty, Args...> &&
             std::is_void_v<std::invoke_result_t<F, Ty, Args...>>
  auto transform(this auto &&self, F &&f, Args &&...args)
      -> StatusOr<Monostate> {
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.ok()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    std::invoke(std::forward<F>(f),
                std::forward<decltype(self)>(self).my_value,
                std::forward<Args>(args)...);
    return {};
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), Ty, Args...> &&
             (!std::is_void_v<std::invoke_result_t<decltype(CPO), Ty, Args...>>)
  auto transform(this auto &&self, Args &&...args)
      -> StatusOr<std::invoke_result_t<decltype(CPO), Ty, Args...>> {
    using F = decltype(CPO);
    AC_DOLL_ASSERT(transform)
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.ok()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    return {std::invoke(CPO,
                        std::forward<decltype(self)>(self).my_value,
                        std::forward<Args>(args)...)};
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), Ty, Args...> &&
             std::is_void_v<std::invoke_result_t<decltype(CPO), Ty, Args...>>
  auto transform(this auto &&self, Args &&...args) -> StatusOr<Monostate> {
    using F = decltype(CPO);
    AC_STATUSOR_CONSUME_METHOD(transform)
    if (!self.ok()) {
      return {std::forward<decltype(self)>(self).as_status()};
    }
    std::invoke(CPO,
                std::forward<decltype(self)>(self).my_value,
                std::forward<Args>(args)...);
    return {};
  }

#  else
  // template <typename F>
  //   requires std::is_invocable_v<F, Ty> &&
  //            (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  // auto transform(F &&f) & -> StatusOr<std::invoke_result_t<F, Ty>> {
  //   AC_DOLL_ASSERT(transform)
  //   if (!ok()) {
  //     return {as_status()};
  //   }
  //   return {std::invoke(std::forward<F>(f), my_value)};
  // }

  // template <typename F>
  //   requires std::is_invocable_v<F, Ty> &&
  //            std::is_void_v<std::invoke_result_t<F, Ty>>
  // auto transform(F &&f) & -> StatusOr<Monostate> {
  //   if (!ok()) {
  //     return {as_status()};
  //   }
  //   std::invoke(std::forward<F>(f), my_value);
  //   return {};
  // }

  // template <typename F>
  //   requires std::is_invocable_v<F, Ty> &&
  //            (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  // auto transform(F &&f) const & -> StatusOr<std::invoke_result_t<F, Ty>> {
  //   AC_DOLL_ASSERT(transform)
  //   if (!ok()) {
  //     return {as_status()};
  //   }
  //   return {std::invoke(std::forward<F>(f), my_value)};
  // }

  // template <typename F>
  //   requires std::is_invocable_v<F, Ty> &&
  //            std::is_void_v<std::invoke_result_t<F, Ty>>
  // auto transform(F &&f) const & -> StatusOr<Monostate> {
  //   if (!ok()) {
  //     return {as_status()};
  //   }
  //   std::invoke(std::forward<F>(f), my_value);
  //   return {};
  // }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  auto transform(F &&f) && -> StatusOr<std::invoke_result_t<F, Ty>> {
    AC_DOLL_ASSERT(transform)
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    return {std::invoke(std::forward<F>(f), std::move(my_value))};
  }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             std::is_void_v<std::invoke_result_t<F, Ty>>
  auto transform(F &&f) && -> StatusOr<Monostate> {
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    std::invoke(std::forward<F>(f), std::move(my_value));
    return {};
  }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  auto transform(F &&f) const && -> StatusOr<std::invoke_result_t<F, Ty>> {
    AC_DOLL_ASSERT(transform)
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    return {std::invoke(std::forward<F>(f), std::move(my_value))};
  }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             std::is_void_v<std::invoke_result_t<F, Ty>>
  auto transform(F &&f) const && -> StatusOr<Monostate> {
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    std::invoke(std::forward<F>(f), std::move(my_value));
    return {};
  }

#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the status stored in the StatusOr
  /// if it is not OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Status -> Status
  /// @return a StatusOr<Ty> that is the result of the function call
  template <typename F, typename... Args>
    requires std::is_invocable_v<F, base_type, Args...> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type, Args...>>)
  auto transform_error(this auto &&self, F &&f, Args &&...args)
      -> StatusOr<Ty> {
    static_assert(std::is_rvalue_reference_v<decltype(self)>, "bad call.");
    if (self.ok()) {
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
    requires std::is_invocable_v<F, base_type, Args...> &&
             std::is_void_v<std::invoke_result_t<F, base_type, Args...>>
  auto transform_error(this auto &&self, F &&f, Args &&...args)
      -> StatusOr<Ty> {
    AC_STATUSOR_CONSUME_METHOD(transform_error)
    if (!self.ok()) {
      std::invoke(std::forward<F>(f),
                  std::forward<decltype(self)>(self).as_status(),
                  std::forward<Args>(args)...);
    }
    return std::forward<decltype(self)>(self);
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), base_type, Args...> &&
             (!std::is_void_v<
                 std::invoke_result_t<decltype(CPO), base_type, Args...>>)
  auto transform_error(this auto &&self, Args &&...args) -> StatusOr<Ty> {
    using F = decltype(CPO);
    static_assert(std::is_rvalue_reference_v<decltype(self)>, "bad call.");
    if (self.ok()) {
      return std::forward<decltype(self)>(self);
    }
    return std::invoke(CPO,
                       std::forward<decltype(self)>(self).as_status(),
                       std::forward<Args>(args)...);
  }

  template <auto CPO, typename... Args>
    requires std::is_invocable_v<decltype(CPO), base_type, Args...> &&
             std::is_void_v<
                 std::invoke_result_t<decltype(CPO), base_type, Args...>>
  auto transform_error(this auto &&self, Args &&...args) -> StatusOr<Ty> {
    using F = decltype(CPO);
    AC_STATUSOR_CONSUME_METHOD(transform_error)
    if (!self.ok()) {
      std::invoke(CPO,
                  std::forward<decltype(self)>(self).as_status(),
                  std::forward<Args>(args)...);
    }
    return std::forward<decltype(self)>(self);
  }
#  else
  // template <typename F>
  //   requires std::is_invocable_v<F, base_type> &&
  //            (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  // auto transform_error(F &&f) & -> StatusOr<Ty> {
  //   if (ok()) {
  //     return *this;
  //   }
  //   return std::invoke(std::forward<F>(f), as_status());
  // }

  // template <typename F>
  //   requires std::is_invocable_v<F, base_type> &&
  //            std::is_void_v<std::invoke_result_t<F, base_type>>
  // auto transform_error(F &&f) & -> StatusOr<Ty> {
  //   if (!ok()) {
  //     std::invoke(std::forward<F>(f), as_status());
  //   }
  //   return *this;
  // }

  // template <typename F>
  //   requires std::is_invocable_v<F, base_type> &&
  //            (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  // auto transform_error(F &&f) const & -> StatusOr<Ty> {
  //   if (ok()) {
  //     return *this;
  //   }
  //   return std::invoke(std::forward<F>(f), as_status());
  // }

  // template <typename F>
  //   requires std::is_invocable_v<F, base_type> &&
  //            std::is_void_v<std::invoke_result_t<F, base_type>>
  // auto transform_error(F &&f) const & -> StatusOr<Ty> {
  //   if (!ok()) {
  //     std::invoke(std::forward<F>(f), as_status());
  //   }
  //   return *this;
  // }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(F &&f) && -> StatusOr<Ty> {
    if (ok()) {
      return std::move(*this);
    }
    return std::invoke(std::forward<F>(f), as_status());
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(F &&f) && -> StatusOr<Ty> {
    if (!ok()) {
      std::invoke(std::forward<F>(f), as_status());
    }
    return std::move(*this);
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(F &&f) const && -> StatusOr<Ty> {
    if (ok()) {
      return std::move(*this);
    }
    return std::invoke(std::forward<F>(f), as_status());
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(F &&f) const && -> StatusOr<Ty> {
    if (!ok()) {
      std::invoke(std::forward<F>(f), as_status());
    }
    return std::move(*this);
  }
#  endif
  /// @deprecated just uses operator=(StatusOr &&that) instead.
  [[clang::reinitializes]] constexpr inline auto reset(const Ty &value = {})
      AC_NOEXCEPT {
    my_value = std::move(value);
    my_code = kOk;
    my_message.clear();
    return *this;
  }
  /// @brief Converts the StatusOr to a std::optional; ownership is transferred.
  constexpr inline auto to_optional(this auto &&self) AC_NOEXCEPT
      -> std::optional<value_type> {
    AC_STATUSOR_CONSUME_METHOD(to_optional)
    if (self.ok()) {
      return std::make_optional(std::forward<decltype(self)>(self).my_value);
    } else {
      return std::nullopt;
    }
  }

#  undef AC_STATUSOR_CONSUME_METHOD
#  undef AC_STATUSOR_DELETE
#  undef AC_DOLL_ASSERT
public:
  /// this is not optimized at all -- use with caution.
  constexpr inline void
  swap(StatusOr &that) noexcept(std::is_nothrow_swappable_v<value_type> &&
                                std::is_nothrow_swappable_v<base_type>) {
    if constexpr (std::is_swappable_v<value_type>) {
      base_type::swap(that.as_status());
      std::swap(my_value, that.my_value);
      return *this;
    } else {
      always_false<value_type>("value_type is not swappable");
    }
  }

  auto to_string(FormatPolicy policy = FormatPolicy::kDefault) const
      -> string_type {
    if (!this->ok()) {
      if (policy == FormatPolicy::kBrief)
        return my_message;
      else
        return Format("{}: {}", base_type::to_string(code()), my_message);
    }

    if constexpr (std::is_base_of_v<Printable, Ty>)
      return my_value.to_string(policy);
    else if constexpr (requires { Format("{}", my_value); })
      return Format("{}", my_value);
    else
      return Format("StatusOr<{}>: <unformattable>", typeid(Ty).name());
  }

private:
  value_type my_value;
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
