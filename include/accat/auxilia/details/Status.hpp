#pragma once
// clang-format off
//! @file Status.hpp
//! @brief A class that represents the status of a function call.
//! @copyright Ancillarycat & The Abseil Authors
//! @note Part of the contents of this header are derived in part from Google's Abseil Common Libraries.

#ifndef ACCAT_AUXILIA_STATUS_HPP
#define ACCAT_AUXILIA_STATUS_HPP

#include "./macros.hpp"

// NOTE:
// The contents of this header are derived in part from Googles' Abseil library under the following license:
/////////////////////////////////////// Apache License 2.0 ////////////////////////////////////////////////

///////////////////////////////// BEGINNING OF ABSEIL COPYRIGHT ///////////////////////////////////////////

// Copyright 2019 The Abseil Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// -----------------------------------------------------------------------------
// File: status.h
// -----------------------------------------------------------------------------
//
// This header file defines the Abseil `status` library, consisting of:
//
//   * An `absl::Status` class for holding error handling information
//   * A set of canonical `absdes, and associated
//     utilities for generating and propagating status codes.
//   * A set of helper functions for creating status codes and checking their
//     values
//
// Within Google, `absl::Status` is the primary mechanism for communicating
// errors in C++, and is used to represent error state in both in-process
// library calls as well as RPC calls. Some of these errors may be recoverable,
// but others may not. Most functions that can produce a recoverable error
// should be designed to return an `absl::Status` (or `absl::StatusOr`).
//
// Example:
//
// absl::Status myFunction(absl::string_view fname, ...) {
//   ...
//   // encounter error
//   if (error condition) {
//     return absl::InvalidArgumentError("bad mode");
//   }
//   // else, return OK
//   return absl::OkStatus();
// }
//
// An `absl::Status` is designed to either return "OK" or one of a number of
// different error codes, corresponding to typical error conditions.
// In almost all cases, when using `absl::Status` you should use the canonical
// error codes (of type `absed in this header file.
// These canonical codes are understood across the codebase and will be
// accepted across all API and RPC boundaries.

///////////////////////////////// END OF ABSEIL COPYRIGHT ///////////////////////////////////////////

// clang-format on
#  include "./config.hpp"
#  include "./format.hpp"
#  include "./Monostate.hpp"

EXPORT_AUXILIA
namespace accat::auxilia {
/// @brief A class that represents the status of a function call. it's
/// designed to be as identical as possible to the `absl::Status`
/// class, for `absl::Status` seems to fail to compile with clang++ on
/// Windows.
/// @todo We should implement a <Val, Err, Ret> pattern; `Ret` just
/// like `throw` or `return to ...`
class Status {
public:
  /// @enum Code
  // clang-format off
  enum class AC_NODISCARD_REASON(
      "Discarding Status Code is strongly discouraged.") Code : uint8_t {

  /// kOK (gRPC code "OK") does not indicate an error; this value is returned on
  /// success. It is typical to check for this value before proceeding on any
  /// given call across an API or RPC boundary.
  ///
  /// @note To check this value, use the
  /// `absl::Status::ok()` member function rather than inspecting the raw code.
  kOk = 0,

  /// kCancelled (gRPC code "CANCELLED") indicates the operation was cancelled,
  /// typically by the caller.
  kCancelled = 1,

  /// kUnknown (gRPC code "UNKNOWN") indicates an unknown error occurred. In
  /// general, more specific errors should be raised, if possible. Errors raised
  /// by APIs that do not return enough error information may be converted to
  /// this error.
  kUnknown = 2,

  /// kInvalidArgument (gRPC code "INVALID_ARGUMENT") indicates the caller
  /// specified an invalid argument, such as a malformed filename. Note that use
  /// of such errors should be narrowly limited to indicate the invalid nature of
  /// the arguments themselves. Errors with validly formed arguments that may
  /// cause errors with the state of the receiving system should be denoted with
  /// `kFailedPrecondition` instead.
  kInvalidArgument = 3,

  /// kDeadlineExceeded (gRPC code "DEADLINE_EXCEEDED") indicates a deadline
  /// expired before the operation could complete.
  ///
  /// @note For operations that may change state within a system, this error may be
  /// returned even if the operation has completed successfully.
  ///       For example, a successful response from a server
  /// could have been delayed long enough for the deadline to expire.
  kDeadlineExceeded = 4,

  /// kNotFound (gRPC code "NOT_FOUND") indicates some requested entity (such as
  /// a file or directory) was not found.
  ///
  /// @remark `kNotFound` is useful if a request should be denied for an entire class of
  /// users, such as during a gradual feature rollout or undocumented allow list.
  /// If a request should be denied for specific sets of users, such as through
  /// user-based access control, use `kPermissionDenied` instead.
  kNotFound = 5,

  /// kAlreadyExists (gRPC code "ALREADY_EXISTS") indicates that the entity a
  /// caller attempted to create (such as a file or directory) is already
  /// present.
  kAlreadyExists = 6,

  /// kPermissionDenied (gRPC code "PERMISSION_DENIED") indicates that the caller
  /// does not have permission to execute the specified operation. Note that this
  /// error is different than an error due to an *un*authenticated user. This
  /// error code does not imply the request is valid or the requested entity
  /// exists or satisfies any other pre-conditions.
  ///
  /// @remark `kPermissionDenied` must not be used for rejections caused by exhausting
  /// some resource. Instead, use `kResourceExhausted` for those errors.
  /// `kPermissionDenied` must not be used if the caller cannot be identified.
  /// Instead, use `kUnauthenticated` for those errors.
  kPermissionDenied = 7,

  /// kResourceExhausted (gRPC code "RESOURCE_EXHAUSTED") indicates some resource
  /// has been exhausted, perhaps a per-user quota, or perhaps the entire file
  /// system is out of space.
  kResourceExhausted = 8,

  /// kFailedPrecondition (gRPC code "FAILED_PRECONDITION") indicates that the
  /// operation was rejected because the system is not in a state required for
  /// the operation's execution. For example, a directory to be deleted may be
  /// non-empty, an "rmdir" operation is applied to a non-directory, etc.
  ///
  /// @remark Some guidelines that may help a service implementer in deciding between
  /// `kFailedPrecondition`, `kAborted`, and `kUnavailable`:
  ///
  ///  - Use `kUnavailable` if the client can retry just the failing call.
  ///  - Use `kAborted` if the client should retry at a higher transaction
  ///      level (such as when a client-specified test-and-set fails, indicating
  ///      the client should restart a read-modify-write sequence).
  ///  - Use `kFailedPrecondition` if the client should not retry until
  ///      the system state has been explicitly fixed. For example, if a "rmdir"
  ///      fails because the directory is non-empty, `kFailedPrecondition`
  ///      should be returned since the client should not retry unless
  ///      the files are deleted from the directory.
  kFailedPrecondition = 9,

  /// kAborted (gRPC code "ABORTED") indicates the operation was aborted,
  /// typically due to a concurrency issue such as a sequencer check failure or a
  /// failed transaction.
  ///
  /// @note See the guidelines above for deciding between `kFailedPrecondition`,
  /// `kAborted`, and `kUnavailable`.
  kAborted = 10,

  /// kOutOfRange (gRPC code "OUT_OF_RANGE") indicates the operation was
  /// attempted past the valid range, such as seeking or reading past an
  /// end-of-file.
  ///
  /// @note Unlike `kInvalidArgument`, this error indicates a problem that may
  /// be fixed if the system state changes. For example, a 32-bit file
  /// system will generate `kInvalidArgument` if asked to read at an
  /// offset that is not in the range [0,2^32-1], but it will generate
  /// `kOutOfRange` if asked to read from an offset past the current
  /// file size.
  /// <br>
  /// There is a fair bit of overlap between `kFailedPrecondition` and
  /// `kOutOfRange`.  We recommend using `kOutOfRange` (the more specific
  /// error) when it applies so that callers who are iterating through
  /// a space can easily look for an `kOutOfRange` error to detect when
  /// they are done.
  kOutOfRange = 11,

  /// kUnimplemented (gRPC code "UNIMPLEMENTED") indicates the operation is not
  /// implemented or supported in this service. In this case, the operation
  /// should not be re-attempted.
  kUnimplemented = 12,

  /// kInternal (gRPC code "INTERNAL") indicates an internal error has occurred
  /// and some invariants expected by the underlying system have not been
  /// satisfied. This error code is reserved for serious errors.
  kInternal = 13,

  /// kUnavailable (gRPC code "UNAVAILABLE") indicates the service is currently
  /// unavailable and that this is most likely a transient condition. An error
  /// such as this can be corrected by retrying with a backoff scheme. Note that
  /// it is not always safe to retry non-idempotent operations.
  ///
  /// @note See the guidelines above for deciding between `kFailedPrecondition`,
  /// `kAborted`, and `kUnavailable`.
  kUnavailable = 14,

  /// kDataLoss (gRPC code "DATA_LOSS") indicates that unrecoverable data loss or
  /// corruption has occurred. As this error is serious, proper alerting should
  /// be attached to errors such as this.
  kDataLoss = 15,

  /// kUnauthenticated (gRPC code "UNAUTHENTICATED") indicates that the request
  /// does not have valid authentication credentials for the operation. Correct
  /// the authentication and try again.
  kUnauthenticated = 16,

  /// The purpose of this enumerated value is to force people who handle status
  /// codes with `switch()` statements to *not* simply enumerate all possible
  /// values, but instead provide a "default:" case. Providing such a default
  /// case ensures that code will compile when new codes are added.
  ///
  /// @note this error code entry should not be used and you should not rely on
  /// its value, which may change.
  kDoNotUseReservedForFutureExpansionUseDefaultInSwitchInstead_ = 20,

  /// kReturning indicates that the function is returning.
  /// @note This is just a workaround, for I don't come up with an idea to
  ///   handle the returning status. Use with caution.
  kReturning = 21,

  kParseError = 22,
  kLexError = 23,

  /// kMovedFrom indicates that the status has been moved from.
  kMovedFrom = (std::numeric_limits<uint8_t>::max)()
  };
  // clang-format on

public:
  using enum Code;

public:
  AC_NODISCARD
  constexpr Status() = default;
  AC_NODISCARD AC_CONSTEXPR20
  Status(const Code code, const string_view message = "<no message provided>")
      : my_code(code), my_message(message) {}
  AC_NODISCARD
  Status(Status &&that) noexcept
      : my_code(that.my_code), my_message(std::move(that.my_message)) {
    that.my_code = kMovedFrom;
    that.my_message = "This status has been moved from.";
  }
  // AC_NODISCARD
  Status(const Status &that) = default;
  auto operator=(const Status &that) -> Status & = default;
  // AC_NODISCARD
  Status &operator=(Status &&that) noexcept {
    my_code = that.my_code;
    my_message = std::move(that.my_message);
    that.my_code = kMovedFrom;
    that.my_message = "status accessed after moved from."s;
    return *this;
  }
  /// @brief Logical OR operator.
  /// @note Useful for chaining status checks rather than
  /// a bunch of `if` statements.
  AC_NODISCARD
  inline constexpr auto operator||(const Status &that) -> Status {
    if (this->ok())
      return *this;
    return that;
  }
  /// @brief Logical AND operator.
  AC_NODISCARD
  inline constexpr auto operator&&(const Status &that) -> Status {
    if (!this->ok())
      return *this;
    return that;
  }
  inline constexpr auto operator&=(const Status &that) -> Status {
    if (!this->ok())
      return *this;
    return that;
  }
  constexpr ~Status() = default;

public:
  AC_NODISCARD
  inline AC_CONSTEXPR20 explicit operator bool() const AC_NOEXCEPT {
    return this->ok();
  }
  AC_NODISCARD constexpr bool ok() const AC_NOEXCEPT {
    return my_code == kOk;
  }
  AC_NODISCARD AC_CONSTEXPR20 bool is_return() const AC_NOEXCEPT {
    return my_code == kReturning;
  }
  AC_NODISCARD
  Code code() const AC_NOEXCEPT {
    return my_code;
  }
  AC_NODISCARD
  auto raw_code() const AC_NOEXCEPT {
    return static_cast<std::underlying_type_t<Code>>(my_code);
  }
  AC_NODISCARD string_view message() const [[clang::lifetimebound]] {
    return my_message;
  }
  void ignore_error() const AC_NOEXCEPT {
    if (ok())
      return;
    contract_assert(ok(), "Ignoring an error status.");
  }

protected:
  Code my_code{};
  string my_message{};
};

/// @brief A class that represents the status of a function call, or a
/// value.
///         it's designed to be as identical as possible to the
///         `absl::StatusOr` class.
/// @tparam Ty the type of the value
template <typename Ty> class StatusOr : public Status, Printable {
  static_assert(std::is_same_v<std::remove_reference_t<Ty>, Ty> &&
                    std::is_nothrow_move_assignable_v<Ty> &&
                    std::is_nothrow_move_constructible_v<Ty>,
                "StatusOr should not be used with reference types.");

public:
  using base_type = Status;
  using value_type = Ty;

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
  inline value_type value(this auto &&self) {
    contract_assert(self.ok() or self.code() == Status::kReturning,
                    "Cannot dereference a status that is not OK.");
    return self.my_value;
  }
#  else
  AC_NODISCARD
  inline value_type value() & {
    contract_assert(ok() or code() == Status::kReturning,
                    "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline value_type value() const & {
    contract_assert(ok() or code() == Status::kReturning,
                    "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline value_type value() && {
    contract_assert(ok() or code() == Status::kReturning,
                    "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline value_type value_or(this auto &&self,
                             const value_type &default_value) {
    return self.ok() ? self.my_value : default_value;
  }
#  else
  AC_NODISCARD
  inline value_type value_or(const value_type &default_value) & {
    return ok() ? my_value : default_value;
  }
  AC_NODISCARD
  inline value_type value_or(const value_type &default_value) const & {
    return ok() ? my_value : default_value;
  }
  AC_NODISCARD
  inline value_type value_or(const value_type &default_value) && {
    return ok() ? std::move(my_value) : default_value;
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline constexpr value_type operator*(this auto &&self) AC_NOEXCEPT {
    precondition(self.ok() or self.code() == Status::kReturning,
                 "Cannot dereference a status that is not OK.");
    return self.my_value;
  }
#  else
  AC_NODISCARD
  inline constexpr value_type operator*() & AC_NOEXCEPT {
    precondition(ok() or code() == Status::kReturning,
                 "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline constexpr value_type operator*() const &AC_NOEXCEPT {
    precondition(ok() or code() == Status::kReturning,
                 "Cannot dereference a status that is not OK.");
    return my_value;
  }
  AC_NODISCARD
  inline constexpr value_type operator*() && AC_NOEXCEPT {
    precondition(ok() or code() == Status::kReturning,
                 "Cannot dereference a status that is not OK.");
    return std::move(my_value);
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD
  inline constexpr auto
  operator->(this auto &&self) AC_NOEXCEPT->decltype(auto) {
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
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  AC_NODISCARD AC_FLATTEN inline constexpr base_type
  as_status(this auto &&self) AC_NOEXCEPT {
    return static_cast<std::remove_reference_t<decltype(self)>::base_type>(
        self);
  }
#  else
  AC_NODISCARD AC_FLATTEN inline constexpr base_type as_status() & AC_NOEXCEPT {
    return static_cast<base_type &>(*this);
  }
  AC_NODISCARD AC_FLATTEN inline constexpr base_type
  as_status() const &AC_NOEXCEPT {
    return static_cast<const base_type &>(*this);
  }
  AC_NODISCARD AC_FLATTEN inline constexpr base_type as_status() &&
      AC_NOEXCEPT {
    return static_cast<base_type &&>(*this);
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Ty>
  auto and_then(this auto &&self, F &&f) -> StatusOr<Ty> {
    if (!self.ok()) {
      return {self.as_status()};
    }
    return std::invoke(std::forward<F>(f), self.my_value);
  }
#  else
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Ty>
  auto and_then(F &&f) & -> StatusOr<Ty> {
    if (!ok()) {
      return {as_status()};
    }
    return std::invoke(std::forward<F>(f), my_value);
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Ty>
  auto and_then(F &&f) const & -> StatusOr<Ty> {
    if (!ok()) {
      return {as_status()};
    }
    return std::invoke(std::forward<F>(f), my_value);
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F, Ty>
  auto and_then(F &&f) && -> StatusOr<Ty> {
    if (!ok()) {
      return {std::move(*this).as_status()};
    }
    return std::invoke(std::forward<F>(f), std::move(my_value));
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F>
  auto or_else(this auto &&self, F &&f) -> StatusOr<Ty> {
    if (self.ok()) {
      return self;
    }
    return std::invoke(std::forward<F>(f));
  }
#  else
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F>
  auto or_else(F &&f) & -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f));
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F>
  auto or_else(F &&f) const & -> StatusOr<Ty> {
    if (ok()) {
      return *this;
    }
    return std::invoke(std::forward<F>(f));
  }
  template <typename F>
    requires std::is_invocable_r_v<StatusOr<Ty>, F>
  auto or_else(F &&f) && -> StatusOr<Ty> {
    if (ok()) {
      return std::move(*this);
    }
    return std::invoke(std::forward<F>(f));
  }
#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  template <typename F>
    requires std::is_invocable_v<F, Ty> &&
             (!std::is_void_v<std::invoke_result_t<F, Ty>>)
  auto transform(this auto &&self, F &&f)
      -> StatusOr<std::invoke_result_t<F, Ty>> {
    if (!self.ok()) {
      return {self.as_status()};
    }
    return {std::invoke(std::forward<F>(f), self.my_value)};
  }
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

#  endif

#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(this auto &&self, F &&f) -> StatusOr<Ty> {
    if (self.ok()) {
      return self;
    }
    return std::invoke(std::forward<F>(f), self.as_status());
  }
  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(this auto &&self, F &&f) -> StatusOr<Monostate> {
    if (self.ok()) {
      return self;
    }
    std::invoke(std::forward<F>(f), self.as_status());
    return {};
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
  auto transform_error(F &&f) & -> StatusOr<Monostate> {
    if (ok()) {
      return *this;
    }
    std::invoke(std::forward<F>(f), as_status());
    return {};
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
  auto transform_error(F &&f) const & -> StatusOr<Monostate> {
    if (ok()) {
      return *this;
    }
    std::invoke(std::forward<F>(f), as_status());
    return {};
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             (!std::is_void_v<std::invoke_result_t<F, base_type>>)
  auto transform_error(F &&f) && -> StatusOr<Ty> {
    if (ok()) {
      return std::move(*this);
    }
    return std::invoke(std::forward<F>(f), std::move(*this).as_status());
  }

  template <typename F>
    requires std::is_invocable_v<F, base_type> &&
             std::is_void_v<std::invoke_result_t<F, base_type>>
  auto transform_error(F &&f) && -> StatusOr<Monostate> {
    if (ok()) {
      return std::move(*this);
    }
    std::invoke(std::forward<F>(f), std::move(*this).as_status());
    return {};
  }
#  endif
  /// @deprecated just uses operator=(StatusOr &&that) instead.
  [[clang::reinitializes]] inline auto reset(Ty &&value = {}) AC_NOEXCEPT {
    my_value = std::move(value);
    my_code = kOk;
    my_message.clear();
    return *this;
  }

public:
  auto to_string(FormatPolicy policy = FormatPolicy::kDefault) const
      -> string_type {
    if (!this->ok())
      return fmt::format("StatusOr<{}> {{ code: {}, message: \"{}\" }}"
                         " value: {}",
                         typeid(Ty).name(),
                         my_code,
                         my_message,
                         my_value);

    else if constexpr (std::is_base_of_v<Printable, Ty>)
      return my_value.to_string(policy);

    else
      return fmt::format("StatusOr<{}>. Type is unformattable.",
                         typeid(Ty).name());
  }

private:
  value_type my_value;
};
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
OkStatus(string_view message = "") AC_NOEXCEPT {
  return {Status::kOk, message};
}

// New overloads for other status codes using string_view messages:
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
Cancelled(string_view message = "") AC_NOEXCEPT {
  return {Status::kCancelled, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
UnknownError(string_view message = "") AC_NOEXCEPT {
  return {Status::kUnknown, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
InvalidArgumentError(string_view message = "") AC_NOEXCEPT {
  return {Status::kInvalidArgument, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
DeadlineExceededError(string_view message = "") AC_NOEXCEPT {
  return {Status::kDeadlineExceeded, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
NotFoundError(string_view message = "") AC_NOEXCEPT {
  return {Status::kNotFound, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
AlreadyExistsError(string_view message = "") AC_NOEXCEPT {
  return {Status::kAlreadyExists, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
PermissionDeniedError(string_view message = "") AC_NOEXCEPT {
  return {Status::kPermissionDenied, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
ResourceExhaustedError(string_view message = "") AC_NOEXCEPT {
  return {Status::kResourceExhausted, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
FailedPreconditionError(string_view message = "") AC_NOEXCEPT {
  return {Status::kFailedPrecondition, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
AbortedError(string_view message = "") AC_NOEXCEPT {
  return {Status::kAborted, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
OutOfRangeError(string_view message = "") AC_NOEXCEPT {
  return {Status::kOutOfRange, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
UnimplementedError(string_view message = "") AC_NOEXCEPT {
  return {Status::kUnimplemented, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
InternalError(string_view message = "") AC_NOEXCEPT {
  return {Status::kInternal, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
UnavailableError(string_view message = "") AC_NOEXCEPT {
  return {Status::kUnavailable, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
DataLossError(string_view message = "") AC_NOEXCEPT {
  return {Status::kDataLoss, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
UnauthenticatedError(string_view message = "") AC_NOEXCEPT {
  return {Status::kUnauthenticated, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
ReturnMe(string_view message = "") AC_NOEXCEPT {
  return {Status::kReturning, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
ParseError(string_view message = "") AC_NOEXCEPT {
  return {Status::kParseError, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static AC_CONSTEXPR20 Status
LexError(string_view message = "") AC_NOEXCEPT {
  return {Status::kLexError, message};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
OkStatus(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kOk,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}
template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
OkStatus(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kOk,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}
template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
Cancelled(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kCancelled,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
Cancelled(const fmt::text_style &ts,
          fmt::format_string<Args...> fmt,
          Args &&...args) {
  return {Status::kCancelled,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnknownError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnknown,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnknownError(const fmt::text_style &ts,
             fmt::format_string<Args...> fmt,
             Args &&...args) {
  return {Status::kUnknown,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
InvalidArgumentError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kInvalidArgument,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
InvalidArgumentError(const fmt::text_style &ts,
                     fmt::format_string<Args...> fmt,
                     Args &&...args) {
  return {Status::kInvalidArgument,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
DeadlineExceededError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kDeadlineExceeded,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
DeadlineExceededError(const fmt::text_style &ts,
                      fmt::format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kDeadlineExceeded,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
NotFoundError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kNotFound,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
NotFoundError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kNotFound,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
AlreadyExistsError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kAlreadyExists,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
AlreadyExistsError(const fmt::text_style &ts,
                   fmt::format_string<Args...> fmt,
                   Args &&...args) {
  return {Status::kAlreadyExists,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
PermissionDeniedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kPermissionDenied,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
PermissionDeniedError(const fmt::text_style &ts,
                      fmt::format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kPermissionDenied,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
ResourceExhaustedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kResourceExhausted,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
ResourceExhaustedError(const fmt::text_style &ts,
                       fmt::format_string<Args...> fmt,
                       Args &&...args) {
  return {Status::kResourceExhausted,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
FailedPreconditionError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kFailedPrecondition,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
FailedPreconditionError(const fmt::text_style &ts,
                        fmt::format_string<Args...> fmt,
                        Args &&...args) {
  return {Status::kFailedPrecondition,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
AbortedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kAborted,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
AbortedError(const fmt::text_style &ts,
             fmt::format_string<Args...> fmt,
             Args &&...args) {
  return {Status::kAborted,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
OutOfRangeError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kOutOfRange,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
OutOfRangeError(const fmt::text_style &ts,
                fmt::format_string<Args...> fmt,
                Args &&...args) {
  return {Status::kOutOfRange,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnimplementedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnimplemented,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnimplementedError(const fmt::text_style &ts,
                   fmt::format_string<Args...> fmt,
                   Args &&...args) {
  return {Status::kUnimplemented,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
InternalError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kInternal,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
InternalError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kInternal,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnavailableError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnavailable,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnavailableError(const fmt::text_style &ts,
                 fmt::format_string<Args...> fmt,
                 Args &&...args) {
  return {Status::kUnavailable,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
DataLossError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kDataLoss,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
DataLossError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kDataLoss,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnauthenticatedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnauthenticated,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
UnauthenticatedError(const fmt::text_style &ts,
                     fmt::format_string<Args...> fmt,
                     Args &&...args) {
  return {Status::kUnauthenticated,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
ReturnMe(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kReturning,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
ReturnMe(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kReturning,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
ParseError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kParseError,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
ParseError(const fmt::text_style &ts,
           fmt::format_string<Args...> fmt,
           Args &&...args) {
  return {Status::kParseError,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
LexError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kLexError,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static Status
LexError(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kLexError,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

#  define AC_RETURN_IF_NOT(_status_)                                           \
    AC_UTILS_AMBIGUOUS_ELSE_BLOCKER                                            \
    if (auto _ac_utils_status_return_ = (_status_))                            \
      ;                                                                        \
    else {                                                                     \
      return _ac_utils_status_return_;                                         \
    }
#  define return_if_not(_status_) AC_RETURN_IF_NOT(_status_)
} // namespace accat::auxilia
#endif
