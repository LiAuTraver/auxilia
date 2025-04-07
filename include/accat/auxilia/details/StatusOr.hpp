#pragma once
// clang-format off
//! @file StatusOr.hpp
//! @brief A class that represents a value or an error status.
//! For more information of the class, please refer to the documentation of the
//! @file Status.hpp

#ifndef ACCAT_AUXILIA_STATUSOR_HPP
#define ACCAT_AUXILIA_STATUSOR_HPP
#include "./macros.hpp"
// clang-format on
#  include "./config.hpp"
#  include "./format.hpp"
#  include "./Status.hpp"
#  include "./Monostate.hpp"
namespace accat::auxilia {

/// @brief A class that represents the status of a function call, or a
/// value.
///         it's designed to be as identical as possible to the
///         `absl::StatusOr` class.
/// @tparam Ty the type of the value
template <typename Ty> class StatusOr : public Status {
  static_assert(std::is_same_v<std::remove_reference_t<Ty>, Ty> &&
                    std::is_nothrow_move_assignable_v<Ty> &&
                    std::is_nothrow_move_constructible_v<Ty>,
                "StatusOr should not be used with reference types.");
  static_assert(Storable<Ty>,
                "StatusOr should not be used with non-storable types.");
  static_assert(!is_specialization_v<Ty, StatusOr>,
                "Please do NOT store a StatusOr in a StatusOr.");

public:
  using base_type = Status;
  using value_type = Ty;
  using rvoff_value_t = std::remove_cv_t<Ty>;

public:
  AC_NODISCARD
  constexpr StatusOr() = default;
  AC_NODISCARD
  StatusOr(const Status &status) : base_type(status) {}
  AC_NODISCARD
  StatusOr(Status &&status) : base_type(std::move(status)) {}
  AC_NODISCARD
  StatusOr(const value_type &value) : base_type(kOk), my_value(value) {}
  AC_NODISCARD
  StatusOr(value_type &&value) : base_type(kOk), my_value(std::move(value)) {}
  AC_NODISCARD
  StatusOr(const Status &status, const value_type &value)
      : base_type(status), my_value(value) {}
  AC_NODISCARD
  StatusOr(Status &&status, value_type &&value)
      : base_type(std::move(status)), my_value(std::move(value)) {}
  AC_NODISCARD
  StatusOr(const StatusOr &that) : base_type(that), my_value(that.my_value) {}
  AC_NODISCARD
  StatusOr(StatusOr &&that) noexcept
      : base_type(std::move(that)), my_value(std::move(that.my_value)) {}
  StatusOr &operator=(const StatusOr &that) {
    base_type::operator=(that);
    my_value = that.my_value;
    return *this;
  }
  StatusOr &operator=(StatusOr &&that) noexcept {
    base_type::operator=(std::move(that));
    my_value = std::move(that.my_value);
    return *this;
  }
  ~StatusOr() = default;

public:
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline auto value(this auto &&self) {
    contract_assert(self.ok() or self.is_return(),
                    "Cannot dereference a status that is not OK.");
    return self.my_value;
  }
#  else
  AC_NODISCARD
  inline value_type value() & {
    contract_assert(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline const value_type value() const & {
    contract_assert(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline value_type value() && {
    contract_assert(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
  AC_NODISCARD
  inline const value_type &&value() const && {
    contract_assert(ok() or is_return(),
                    "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD AC_CONSTEXPR20 inline value_type
  value_or(this auto &&self, rvoff_value_t &&default_value) {
    return self.ok() ? self.my_value
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
  inline constexpr auto &operator*(this auto &&self) AC_NOEXCEPT {
    precondition(self.ok() or self.is_return(),
                 "Cannot dereference a status that is not OK.");
    return self.my_value;
  }
  AC_NODISCARD
  inline constexpr auto &&operator*() && AC_NOEXCEPT {
    precondition(ok() or is_return(),
                 "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
#  else
  AC_NODISCARD
  inline constexpr value_type &operator*() & AC_NOEXCEPT {
    precondition(ok() or is_return(),
                 "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline constexpr const value_type &operator*() const &AC_NOEXCEPT {
    precondition(ok() or is_return(),
                 "Cannot dereference a status that is not OK.");
    return my_value;
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline constexpr auto operator->(this auto &&self)
      AC_NOEXCEPT->decltype(auto) {
    return std::addressof(self.my_value);
  }
#  else
  AC_NODISCARD
  inline constexpr auto operator->() & AC_NOEXCEPT->value_type * {
    return std::addressof(my_value);
  }
  AC_NODISCARD
  inline constexpr auto operator->() const & AC_NOEXCEPT->const value_type * {
    return std::addressof(my_value);
  }
  AC_NODISCARD
  inline constexpr auto operator->() && AC_NOEXCEPT->value_type * {
    return std::addressof(std::move(my_value));
  }
  AC_NODISCARD
  inline constexpr auto operator->() const && AC_NOEXCEPT->const value_type * {
    return std::addressof(std::move(my_value));
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD AC_FLATTEN inline constexpr auto as_status(this auto &&self)
      AC_NOEXCEPT -> decltype(auto) {
    return (static_cast<base_type>(self));
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
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  /// @brief Calls the function `f` with the value stored in the StatusOr
  /// if it is OK, otherwise do nothing and return the StatusOr itself.
  /// @param f Ty -> StatusOr<Uy>
  /// @return a StatusOr<Uy> that is the result of the function call
  template <typename F, typename R = std::invoke_result_t<F, Ty>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(this auto &&self, F &&f)
      -> std::conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
    if (!self.ok()) {
      return {self.as_status()};
    }
    return std::invoke(std::forward<F>(f), self.my_value);
  }
#  else
  template <typename F, typename R = std::invoke_result_t<F, Ty>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(F &&f) & -> std::
      conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
    if (!ok()) {
      return {as_status()};
    }
    return std::invoke(std::forward<F>(f), my_value);
  }

  template <typename F, typename R = std::invoke_result_t<F, Ty>>
    requires is_specialization_v<R, StatusOr> || std::is_same_v<R, Status>
  auto and_then(F &&f) const & -> std::
      conditional_t<is_specialization_v<R, StatusOr>, R, StatusOr<Ty>> {
    if (!ok()) {
      return {as_status()};
    }
    return std::invoke(std::forward<F>(f), my_value);
  }

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
  /// @param f Status -> StatusOr<Ty>
  /// @return a StatusOr<Ty> that is the result of the function call
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, string_type>
  auto or_else(this auto &&self, F &&f) -> StatusOr<Ty> {
    if (self.ok()) {
      return self;
    }
    return std::invoke(std::forward<F>(f), self.as_status());
  }
#  else
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, string_type>
  auto or_else(F &&f) & -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f), as_status());
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, string_type>
  auto or_else(F &&f) const & -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f), as_status());
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, string_type>
  auto or_else(F &&f) && -> StatusOr<Ty> {
    if (ok()) {
      return std::move(*this);
    }
    return std::invoke(std::forward<F>(f), as_status());
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, string_type>
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
  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  auto transform(this auto &&self, F &&f)
      -> StatusOr<std::invoke_result_t<F, Ty>> {
    AC_DOLL_ASSERT(transform)
    if (!self.ok()) {
      return {self.as_status()};
    }
    return {std::invoke(std::forward<F>(f), self.my_value)};
  }
  /// @copydoc transform
  /// @param f Ty -> void
  /// @return StatusOr<Monostate>
  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             std::is_void_v<std::invoke_result_t<F, Ty>>
  auto transform(this auto &&self, F &&f) -> StatusOr<Monostate> {
    if (!self.ok()) {
      return {self.as_status()};
    }
    std::invoke(std::forward<F>(f), self.my_value);
    return {};
  }

#  else
  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  auto transform(F &&f) & -> StatusOr<std::invoke_result_t<F, Ty>> {
    AC_DOLL_ASSERT(transform)
    if (!ok()) {
      return {as_status()};
    }
    return {std::invoke(std::forward<F>(f), my_value)};
  }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             std::is_void_v<std::invoke_result_t<F, Ty>>
  auto transform(F &&f) & -> StatusOr<Monostate> {
    if (!ok()) {
      return {as_status()};
    }
    std::invoke(std::forward<F>(f), my_value);
    return {};
  }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  auto transform(F &&f) const & -> StatusOr<std::invoke_result_t<F, Ty>> {
    AC_DOLL_ASSERT(transform)
    if (!ok()) {
      return {as_status()};
    }
    return {std::invoke(std::forward<F>(f), my_value)};
  }

  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             std::is_void_v<std::invoke_result_t<F, Ty>>
  auto transform(F &&f) const & -> StatusOr<Monostate> {
    if (!ok()) {
      return {as_status()};
    }
    std::invoke(std::forward<F>(f), my_value);
    return {};
  }

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
  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(this auto &&self, F &&f) -> StatusOr<Ty> {
    if (self.ok()) {
      return self;
    }
    return std::invoke(std::forward<F>(f), self.as_status());
  }
  /// @copydoc transform_error
  /// @param f Status -> void
  /// @return StatusOr<Ty> with err unmodified
  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(this auto &&self, F &&f) -> StatusOr<Ty> {
    if (!self.ok()) {
      std::invoke(std::forward<F>(f), self.as_status());
    }
    return self;
  }
#  else
  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(F &&f) & -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f), as_status());
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(F &&f) & -> StatusOr<Ty> {
    if (!ok()) {
      std::invoke(std::forward<F>(f), as_status());
    }
    return *this;
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(F &&f) const & -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f), as_status());
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(F &&f) const & -> StatusOr<Ty> {
    if (!ok()) {
      std::invoke(std::forward<F>(f), as_status());
    }
    return *this;
  }

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
#  undef AC_DOLL_ASSERT
  /// @deprecated just uses operator=(StatusOr &&that) instead.
  [[clang::reinitializes]] inline auto reset(const Ty &value = {}) AC_NOEXCEPT {
    my_value = std::move(value);
    my_code = kOk;
    my_message.clear();
    return *this;
  }

public:
  auto to_string(FormatPolicy policy = FormatPolicy::kDefault) const
      -> string_type {
    if (!this->ok())
      return auxilia::format("StatusOr<{}> {{ code: {}, message: \"{}\" }}",
                             typeid(Ty).name(),
                             raw_code(),
                             my_message);

    else if constexpr (std::is_base_of_v<Printable, Ty>)
      return my_value.to_string(policy);
#  if __has_include(<fmt/format.h>)
    else if constexpr (fmt::is_formattable<Ty>::value)
      return fmt::format("{}", my_value);
#  endif
    else
      return auxilia::format("StatusOr<{}>: <unformattable>",
                             typeid(Ty).name());
  }

private:
  value_type my_value;
};
} // namespace accat::auxilia

#endif // ACCAT_AUXILIA_STATUSOR_HPP
