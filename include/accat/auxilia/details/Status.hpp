#pragma once
// clang-format off
//! @file Status.hpp
//! @brief A class that represents the status of a function call.
//! @copyright Ancillarycat & The Abseil Authors
//! @note Part of the contents of this header are derived in part from Google's Abseil Common Libraries.

#ifndef ACCAT_AUXILIA_STATUS_HPP
#define ACCAT_AUXILIA_STATUS_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>

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
// absl::Status myFunction(absl::std::string_view fname, ...) {
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

EXPORT_AUXILIA
namespace accat::auxilia {
/// @brief A class that represents the status of a function call. it's
/// designed to be as identical as possible to the `absl::Status`
/// class, for `absl::Status` seems to fail to compile with clang++ on
/// Windows.
/// @todo We should implement a <Val, Err, Ret> pattern; `Ret` just
/// like `throw` or `return to ...`
class Status : public Printable {
public:
  /// @enum Code
  // clang-format off
  enum class AC_NODISCARD_REASON_(
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
  AC_NODISCARD_
  constexpr Status() = default;
  AC_NODISCARD_ AC_CONSTEXPR20_ Status(const Code code,
                                       const std::string_view message = {})
      : my_code(code), my_message(message) {}
  AC_NODISCARD_
  Status(Status &&that) noexcept
      : my_code(that.my_code), my_message(std::move(that.my_message)) {
    that.my_code = kMovedFrom;
    AC_DEBUG_ONLY(that.my_message = "This status has been moved from.");
  }
  // AC_NODISCARD_
  Status(const Status &that) = default;
  auto operator=(const Status &that) -> Status & = default;
  // AC_NODISCARD_
  Status &operator=(Status &&that) noexcept {
    my_code = that.my_code;
    my_message = std::move(that.my_message);
    that.my_code = kMovedFrom;
    AC_DEBUG_ONLY(that.my_message = "status accessed after moved from.");
    return *this;
  }
  /// @brief Logical OR operator.
  /// @note Useful for chaining status checks rather than
  /// a bunch of `if` statements.
  AC_NODISCARD_
  inline constexpr auto operator||(const Status &that) -> Status {
    if (this->ok())
      return *this;
    return that;
  }
  /// @brief Logical AND operator.
  AC_NODISCARD_
  inline constexpr auto operator&&(const Status &that) -> Status {
    if (!this->ok())
      return *this;
    return that;
  }
  inline constexpr auto operator&=(const Status &that) -> Status {
    if (!this->ok())
      return *this;
    *this = that;
    return *this;
  }
  inline constexpr auto operator&=(Status &&that) -> Status {
    if (!this->ok())
      return *this;
    *this = std::move(that);
    return *this;
  }
  inline constexpr ~Status() = default;

public:
  AC_NODISCARD_
  inline AC_CONSTEXPR20_ explicit operator bool() const AC_NOEXCEPT {
    return this->ok();
  }
  AC_NODISCARD_ constexpr bool ok() const AC_NOEXCEPT { return my_code == kOk; }
  AC_NODISCARD_ AC_CONSTEXPR20_ bool is_return() const AC_NOEXCEPT {
    return my_code == kReturning;
  }
  AC_NODISCARD_
  Code code() const AC_NOEXCEPT { return my_code; }
  AC_NODISCARD_
  auto raw_code() const AC_NOEXCEPT {
    return static_cast<std::underlying_type_t<Code>>(my_code);
  }
  AC_NODISCARD_ std::string_view message() const [[clang::lifetimebound]] {
    return my_message;
  }
  inline void ignore_error() const AC_NOEXCEPT {
    AC_DEBUG_ONLY(
        if (!this->ok()) AC_DEBUG_LOGGING(
            warn, "Ignoring a Status which is not ok: {}", my_message);)
    // else do nothing
  }

public:
  inline auto to_string(const FormatPolicy = FormatPolicy::kDefault) const
      -> string_type {
    return auxilia::format("Status {}: {}", raw_code(), my_message);
  }
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  inline auto rvalue(this auto &&self) { return std::move(self); }
#  else
  inline auto rvalue() & { return std::move(*this); }
  inline auto rvalue() const & { return *this; }
  inline auto rvalue() && { return std::move(*this); }
  inline auto rvalue() const && { return std::move(*this); }
#  endif

protected:
  Code my_code{};
  string_type my_message{};
};

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
OkStatus(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kOk, message};
}

// New overloads for other status codes using std::string_view messages:
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
Cancelled(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kCancelled, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnknownError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kUnknown, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
InvalidArgumentError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kInvalidArgument, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
DeadlineExceededError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kDeadlineExceeded, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
NotFoundError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kNotFound, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
AlreadyExistsError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kAlreadyExists, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
PermissionDeniedError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kPermissionDenied, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ResourceExhaustedError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kResourceExhausted, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
FailedPreconditionError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kFailedPrecondition, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
AbortedError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kAborted, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
OutOfRangeError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kOutOfRange, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnimplementedError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kUnimplemented, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
InternalError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kInternal, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnavailableError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kUnavailable, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
DataLossError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kDataLoss, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnauthenticatedError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kUnauthenticated, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ReturnMe(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kReturning, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ParseError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kParseError, message};
}

AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
LexError(std::string_view message = "") AC_NOEXCEPT {
  return {Status::kLexError, message};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
OkStatus(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kOk,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}
template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
Cancelled(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kCancelled,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnknownError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnknown,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
InvalidArgumentError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kInvalidArgument,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
DeadlineExceededError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kDeadlineExceeded,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
NotFoundError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kNotFound,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
AlreadyExistsError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kAlreadyExists,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
PermissionDeniedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kPermissionDenied,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ResourceExhaustedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kResourceExhausted,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
FailedPreconditionError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kFailedPrecondition,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
AbortedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kAborted,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
OutOfRangeError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kOutOfRange,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnimplementedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnimplemented,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
InternalError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kInternal,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnavailableError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnavailable,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
DataLossError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kDataLoss,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnauthenticatedError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnauthenticated,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ReturnMe(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kReturning,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ParseError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kParseError,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
LexError(auxilia::format_string<Args...> fmt, Args &&...args) {
  return {Status::kLexError,
          auxilia::format(fmt, std::forward<decltype(args)>(args)...)};
}
#  if !AC_USE_STD_FMT
template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_
    Status OkStatus(const fmt::text_style &ts,
                    fmt::format_string<Args...> fmt,
                    Args &&...args) {
  return {Status::kOk,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
Cancelled(const fmt::text_style &ts,
          fmt::format_string<Args...> fmt,
          Args &&...args) {
  return {Status::kCancelled,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnknownError(const fmt::text_style &ts,
             fmt::format_string<Args...> fmt,
             Args &&...args) {
  return {Status::kUnknown,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
InvalidArgumentError(const fmt::text_style &ts,
                     fmt::format_string<Args...> fmt,
                     Args &&...args) {
  return {Status::kInvalidArgument,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
DeadlineExceededError(const fmt::text_style &ts,
                      fmt::format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kDeadlineExceeded,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
NotFoundError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kNotFound,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
AlreadyExistsError(const fmt::text_style &ts,
                   fmt::format_string<Args...> fmt,
                   Args &&...args) {
  return {Status::kAlreadyExists,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
PermissionDeniedError(const fmt::text_style &ts,
                      fmt::format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kPermissionDenied,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ResourceExhaustedError(const fmt::text_style &ts,
                       fmt::format_string<Args...> fmt,
                       Args &&...args) {
  return {Status::kResourceExhausted,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
FailedPreconditionError(const fmt::text_style &ts,
                        fmt::format_string<Args...> fmt,
                        Args &&...args) {
  return {Status::kFailedPrecondition,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
AbortedError(const fmt::text_style &ts,
             fmt::format_string<Args...> fmt,
             Args &&...args) {
  return {Status::kAborted,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
OutOfRangeError(const fmt::text_style &ts,
                fmt::format_string<Args...> fmt,
                Args &&...args) {
  return {Status::kOutOfRange,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnimplementedError(const fmt::text_style &ts,
                   fmt::format_string<Args...> fmt,
                   Args &&...args) {
  return {Status::kUnimplemented,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
InternalError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kInternal,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnavailableError(const fmt::text_style &ts,
                 fmt::format_string<Args...> fmt,
                 Args &&...args) {
  return {Status::kUnavailable,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
DataLossError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kDataLoss,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
UnauthenticatedError(const fmt::text_style &ts,
                     fmt::format_string<Args...> fmt,
                     Args &&...args) {
  return {Status::kUnauthenticated,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ReturnMe(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kReturning,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
ParseError(const fmt::text_style &ts,
           fmt::format_string<Args...> fmt,
           Args &&...args) {
  return {Status::kParseError,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}

template <typename... Args>
AC_NODISCARD_ AC_FORCEINLINE_ AC_FLATTEN_ static AC_CONSTEXPR20_ Status
LexError(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kLexError,
          fmt::format(ts, fmt, std::forward<decltype(args)>(args)...)};
}
#  endif

#  define AC_RETURN_IF_NOT(_status_)                                           \
    AC_AMBIGUOUS_ELSE_BLOCKER                                                  \
    if (auto _ac_auxilia_status_return_ = (_status_))                          \
      ;                                                                        \
    else {                                                                     \
      return _ac_auxilia_status_return_;                                       \
    }
} // namespace accat::auxilia
#endif // ACCAT_AUXILIA_STATUS_HPP
