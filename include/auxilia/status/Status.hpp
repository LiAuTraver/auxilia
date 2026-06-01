#pragma once
// clang-format off
//! @file Status.hpp
//! @brief A class that represents the status of a function call.
//! @copyright Ancillarycat & The Abseil Authors
//! @note Part of the contents of this header are derived in part from Google's Abseil Common Libraries.

#ifndef AUXILIA_STATUS_HPP
#define AUXILIA_STATUS_HPP

#include <limits>
#include <type_traits>
#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <iostream>

#if !__has_include(<spdlog/spdlog.h>)
#include <syncstream>
#endif

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
#  include "auxilia/base/macros.hpp"
#  include "auxilia/base/config.hpp"
#  include "auxilia/base/format.hpp"

EXPORT_AUXILIA
namespace auxilia {
/// @brief A class that represents the status of a function call. it's
/// designed to be as similiar as possible to the `absl::Status` class, for
/// `absl::Status` shipped in my vcpkg seems to mysteriously fail to compile
/// with clang++ on Windows.
class Status : public Printable {
public:
  /// @enum Code
  // clang-format off
  enum class AC_NODISCARD_REASON(
      "Discarding Status Code is strongly discouraged.") Code : uint8_t {

  /// kOK (gRPC code `OK`) does not indicate an error; this value is returned on
  /// success. It is typical to check for this value before proceeding on any
  /// given call across an API or RPC boundary.
  ///
  /// @note To check this value, use the
  /// `absl::Status::ok()` member function rather than inspecting the raw code.
  kOk = 0,

  /// kCancelled (gRPC code `CANCELLED`) indicates the operation was cancelled,
  /// typically by the caller.
  kCancelled = 1,

  /// kUnknown (gRPC code `UNKNOWN`) indicates an unknown error occurred. In
  /// general, more specific errors should be raised, if possible. Errors raised
  /// by APIs that do not return enough error information may be converted to
  /// this error.
  kUnknown = 2,

  /// kInvalidArgument (gRPC code `INVALID_ARGUMENT`) indicates the caller
  /// specified an invalid argument, such as a malformed filename. Note that use
  /// of such errors should be narrowly limited to indicate the invalid nature of
  /// the arguments themselves. Errors with validly formed arguments that may
  /// cause errors with the state of the receiving system should be denoted with
  /// `kFailedPrecondition` instead.
  kInvalidArgument = 3,

  /// kDeadlineExceeded (gRPC code `DEADLINE_EXCEEDED`) indicates a deadline
  /// expired before the operation could complete.
  ///
  /// @note For operations that may change state within a system, this error may be
  /// returned even if the operation has completed successfully.
  ///       For example, a successful response from a server
  /// could have been delayed long enough for the deadline to expire.
  kDeadlineExceeded = 4,

  /// kNotFound (gRPC code `NOT_FOUND`) indicates some requested entity (such as
  /// a file or directory) was not found.
  ///
  /// @remark `kNotFound` is useful if a request should be denied for an entire class of
  /// users, such as during a gradual feature rollout or undocumented allow list.
  /// If a request should be denied for specific sets of users, such as through
  /// user-based access control, use `kPermissionDenied` instead.
  kNotFound = 5,

  /// kAlreadyExists (gRPC code `ALREADY_EXISTS`) indicates that the entity a
  /// caller attempted to create (such as a file or directory) is already
  /// present.
  kAlreadyExists = 6,

  /// kPermissionDenied (gRPC code `PERMISSION_DENIED`) indicates that the caller
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

  /// kResourceExhausted (gRPC code `RESOURCE_EXHAUSTED`) indicates some resource
  /// has been exhausted, perhaps a per-user quota, or perhaps the entire file
  /// system is out of space.
  kResourceExhausted = 8,

  /// kFailedPrecondition (gRPC code `FAILED_PRECONDITION`) indicates that the
  /// operation was rejected because the system is not in a state required for
  /// the operation's execution. For example, a directory to be deleted may be
  /// non-empty, an `rmdir` operation is applied to a non-directory, etc.
  ///
  /// @remark Some guidelines that may help a service implementer in deciding between
  /// `kFailedPrecondition`, `kAborted`, and `kUnavailable`:
  ///
  ///  - Use `kUnavailable` if the client can retry just the failing call.
  ///  - Use `kAborted` if the client should retry at a higher transaction
  ///      level (such as when a client-specified test-and-set fails, indicating
  ///      the client should restart a read-modify-write sequence).
  ///  - Use `kFailedPrecondition` if the client should not retry until
  ///      the system state has been explicitly fixed. For example, if a `rmdir`
  ///      fails because the directory is non-empty, `kFailedPrecondition`
  ///      should be returned since the client should not retry unless
  ///      the files are deleted from the directory.
  kFailedPrecondition = 9,

  /// kAborted (gRPC code `ABORTED`) indicates the operation was aborted,
  /// typically due to a concurrency issue such as a sequencer check failure or a
  /// failed transaction.
  ///
  /// @note See the guidelines above for deciding between `kFailedPrecondition`,
  /// `kAborted`, and `kUnavailable`.
  kAborted = 10,

  /// kOutOfRange (gRPC code `OUT_OF_RANGE`) indicates the operation was
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

  /// kUnimplemented (gRPC code `UNIMPLEMENTED`) indicates the operation is not
  /// implemented or supported in this service. In this case, the operation
  /// should not be re-attempted.
  kUnimplemented = 12,

  /// kInternal (gRPC code `INTERNAL`) indicates an internal error has occurred
  /// and some invariants expected by the underlying system have not been
  /// satisfied. This error code is reserved for serious errors.
  kInternal = 13,

  /// kUnavailable (gRPC code `UNAVAILABLE`) indicates the service is currently
  /// unavailable and that this is most likely a transient condition. An error
  /// such as this can be corrected by retrying with a backoff scheme. Note that
  /// it is not always safe to retry non-idempotent operations.
  ///
  /// @note See the guidelines above for deciding between `kFailedPrecondition`,
  /// `kAborted`, and `kUnavailable`.
  kUnavailable = 14,

  /// kDataLoss (gRPC code `DATA_LOSS`) indicates that unrecoverable data loss or
  /// corruption has occurred. As this error is serious, proper alerting should
  /// be attached to errors such as this.
  kDataLoss = 15,

  /// kUnauthenticated (gRPC code `UNAUTHENTICATED`) indicates that the request
  /// does not have valid authentication credentials for the operation. Correct
  /// the authentication and try again.
  kUnauthenticated = 16,

  /// The purpose of this enumerated value is to force people who handle status
  /// codes with `switch()` statements to *not* simply enumerate all possible
  /// values, but instead provide a `default:` case. Providing such a default
  /// case ensures that code will compile when new codes are added.
  ///
  /// @note this error code entry should not be used and you should not rely on
  /// its value, which may change.
  kDoNotUseReservedForFutureExpansionUseDefaultInSwitchInstead_ = 20,


  kReturning  /* [[deprecated("just a workaround for the sake of some of my project")]] */ = 21,
  kParseError /* [[deprecated("just a workaround for the sake of some of my project")]] */ = 22,
  kLexError   /* [[deprecated("just a workaround for the sake of some of my project")]] */ = 23,

  /// kMovedFrom indicates that the status has been moved from.
  kMovedFrom = (std::numeric_limits<uint8_t>::max)()
  };
  // clang-format on
  static constexpr const char *to_string(const Code code) noexcept {
    switch (code) {
    case kOk:
      return "OK";
    case kCancelled:
      return "Cancelled";
    case kUnknown:
      return "Unknown";
    case kInvalidArgument:
      return "Invalid Argument";
    case kDeadlineExceeded:
      return "Deadline Exceeded";
    case kNotFound:
      return "Not Found";
    case kAlreadyExists:
      return "Already Exists";
    case kPermissionDenied:
      return "Permission Denied";
    case kResourceExhausted:
      return "Resource Exhausted";
    case kFailedPrecondition:
      return "Failed Precondition";
    case kAborted:
      return "Aborted";
    case kOutOfRange:
      return "Out of Range";
    case kUnimplemented:
      return "Unimplemented";
    case kInternal:
      return "Internal";
    case kUnavailable:
      return "Unavailable";
    case kDataLoss:
      return "Data Loss";
    case kUnauthenticated:
      return "Unauthenticated";
    case kReturning:
      return "Returning";
    case kParseError:
      return "Parse Error";
    case kLexError:
      return "Lex Error";
    default:
      AC_UNREACHABLE("unknown status code");
    }
  }

public:
  using enum Code;

public:
  AC_NODISCARD
  constexpr Status() = default;
  AC_NODISCARD AC_CONSTEXPR20 Status(const Code code, std::nullptr_t = nullptr)
      : my_code(code), my_message() {}
  AC_NODISCARD AC_CONSTEXPR20 Status(const Code code, std::string &&message)
      : my_message(message), my_code(code) {}
  AC_NODISCARD AC_CONSTEXPR20 Status(const Code code, const char *const message)
      : my_message(message), my_code(code) {}
  AC_NODISCARD
  Status(Status &&that) noexcept
      : my_message(std::move(that.my_message)), my_code(that.my_code) {
    that.my_code = kMovedFrom;
    AC_DEBUG_ONLY(that.my_message = "This status has been moved from.");
  }
  // AC_NODISCARD
  Status(const Status &that) = default;
  auto operator=(const Status &that) -> Status & = default;
  // AC_NODISCARD
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
  AC_NODISCARD
  inline AC_CONSTEXPR20 explicit operator bool() const AC_NOEXCEPT {
    return this->ok();
  }
  AC_NODISCARD constexpr bool ok() const noexcept { return my_code == kOk; }
  AC_NODISCARD AC_CONSTEXPR20 bool is_return() const noexcept {
    return my_code == kReturning;
  }
  AC_NODISCARD
  auto code() const noexcept { return my_code; }
  AC_NODISCARD
  auto raw_code() const noexcept { return std::to_underlying(my_code); }
  AC_NODISCARD std::string_view message() const [[clang::lifetimebound]] {
    return my_message;
  }
  AC_NODISCARD auto &raw_message() const noexcept { return my_message; }
  inline void ignore_error() const AC_NOEXCEPT {
    AC_DEBUG_ONLY(
        if (!this->ok()) AC_DEBUG_LOGGING(
            warn, "Ignoring a Status which is not ok: {}", my_message);)
    // else do nothing
  }
  /// @brief Logs the message if it's not empty.
  inline void log(auto &&logger) const {
    if constexpr (requires { logger << my_message; })
      logger << my_message << '\n';
    else if (ok())
      if (message().empty())
        return;
      else if constexpr (requires { logger.info(my_message); })
        logger.info(my_message);
      else if constexpr (requires { logger->info(my_message); })
        logger->info(my_message);
      else
        static_assert(false, "unsupported");
    else if constexpr (requires { logger.error(my_message); })
      logger.error(my_message);
    else if constexpr (requires { logger->error(my_message); })
      logger->error(my_message);
    else
      static_assert(false, "unsupported");
  }
  inline void log() const {
#  if __has_include(<spdlog/spdlog.h>)
    log(spdlog::default_logger());
#  else
    log(ok() ? std::osyncstream(std::cout) : std::osyncstream(std::cerr));
#  endif
  }
  /// @brief Logs the error message if the status is not ok.
  inline void log_err(auto &&logger) const {
    if (ok())
      return;
    if constexpr (requires { logger << my_message; })
      logger << my_message << '\n';
    else if constexpr (requires { logger.error(my_message); })
      logger.error(my_message);
    else if constexpr (requires { logger->error(my_message); })
      logger->error(my_message);
    else
      static_assert(false, "unsupported");
  }
  /// @brief Logs the error message if the status is not ok.
  inline void log_err() const {
#  if __has_include(<spdlog/spdlog.h>)
    log_err(spdlog::default_logger());
#  else
    log_err(std::osyncstream(std::cerr));
#  endif
  }

  AC_CONSTEXPR20 inline auto
  swap(Status &that) noexcept(std::is_nothrow_swappable_v<Status::Code> &&
                              std::is_nothrow_swappable_v<Status::string_type>)
      -> void {
    std::swap(my_code, that.my_code);
    std::swap(my_message, that.my_message);
  }

public:
  inline auto to_string(const FormatPolicy = FormatPolicy::kDefault) const
      -> string_type {
    return Format("{}: {}", to_string(code()), my_message);
  }
#  if AC_HAS_EXPLICIT_THIS_PARAMETER
  [[deprecated("use std::move instead")]]
  /// @brief just a shorthand to move the StatusOr object.
  inline decltype(auto) rvalue(this auto &&self) noexcept {
    return std::move(self);
  }
#  else
  [[deprecated("use std::move instead")]]
  inline decltype(auto) rvalue() & noexcept {
    return std::move(*this);
  }
  [[deprecated("use std::move instead")]]
  inline decltype(auto) rvalue() && noexcept {
    return std::move(*this);
  }
#  endif

protected:
  string_type my_message{};
  Code my_code{};
};

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OkStatus(const char *const message = "") AC_NOEXCEPT {
  return {Status::kOk, message};
}

// New overloads for other status codes using std::string_view messages:
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
Cancelled(const char *const message = "") AC_NOEXCEPT {
  return {Status::kCancelled, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnknownError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kUnknown, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InvalidArgumentError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kInvalidArgument, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DeadlineExceededError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kDeadlineExceeded, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
NotFoundError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kNotFound, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AlreadyExistsError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kAlreadyExists, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
PermissionDeniedError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kPermissionDenied, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ResourceExhaustedError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kResourceExhausted, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
FailedPreconditionError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kFailedPrecondition, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AbortedError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kAborted, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OutOfRangeError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kOutOfRange, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnimplementedError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kUnimplemented, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InternalError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kInternal, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnavailableError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kUnavailable, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DataLossError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kDataLoss, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnauthenticatedError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kUnauthenticated, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ReturnMe(const char *const message = "") AC_NOEXCEPT {
  return {Status::kReturning, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ParseError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kParseError, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
LexError(const char *const message = "") AC_NOEXCEPT {
  return {Status::kLexError, message};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OkStatus(std::string &&message) AC_NOEXCEPT {
  return {Status::kOk, std::forward<std::string>(message)};
}

// New overloads for other status codes using std::string_view messages:
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
Cancelled(std::string &&message) AC_NOEXCEPT {
  return {Status::kCancelled, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnknownError(std::string &&message) AC_NOEXCEPT {
  return {Status::kUnknown, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InvalidArgumentError(std::string &&message) AC_NOEXCEPT {
  return {Status::kInvalidArgument, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DeadlineExceededError(std::string &&message) AC_NOEXCEPT {
  return {Status::kDeadlineExceeded, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
NotFoundError(std::string &&message) AC_NOEXCEPT {
  return {Status::kNotFound, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AlreadyExistsError(std::string &&message) AC_NOEXCEPT {
  return {Status::kAlreadyExists, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
PermissionDeniedError(std::string &&message) AC_NOEXCEPT {
  return {Status::kPermissionDenied, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ResourceExhaustedError(std::string &&message) AC_NOEXCEPT {
  return {Status::kResourceExhausted, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
FailedPreconditionError(std::string &&message) AC_NOEXCEPT {
  return {Status::kFailedPrecondition, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AbortedError(std::string &&message) AC_NOEXCEPT {
  return {Status::kAborted, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OutOfRangeError(std::string &&message) AC_NOEXCEPT {
  return {Status::kOutOfRange, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnimplementedError(std::string &&message) AC_NOEXCEPT {
  return {Status::kUnimplemented, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InternalError(std::string &&message) AC_NOEXCEPT {
  return {Status::kInternal, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnavailableError(std::string &&message) AC_NOEXCEPT {
  return {Status::kUnavailable, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DataLossError(std::string &&message) AC_NOEXCEPT {
  return {Status::kDataLoss, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnauthenticatedError(std::string &&message) AC_NOEXCEPT {
  return {Status::kUnauthenticated, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ReturnMe(std::string &&message) AC_NOEXCEPT {
  return {Status::kReturning, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ParseError(std::string &&message) AC_NOEXCEPT {
  return {Status::kParseError, std::forward<std::string>(message)};
}

AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
LexError(std::string &&message) AC_NOEXCEPT {
  return {Status::kLexError, std::forward<std::string>(message)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OkStatus(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kOk, Format(fmt, std::forward<Args>(args)...)};
}
template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
Cancelled(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kCancelled, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnknownError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnknown, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InvalidArgumentError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kInvalidArgument, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DeadlineExceededError(AC_STD_OR_FMT format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kDeadlineExceeded, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
NotFoundError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kNotFound, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AlreadyExistsError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kAlreadyExists, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
PermissionDeniedError(AC_STD_OR_FMT format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kPermissionDenied, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ResourceExhaustedError(AC_STD_OR_FMT format_string<Args...> fmt,
                       Args &&...args) {
  return {Status::kResourceExhausted, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
FailedPreconditionError(AC_STD_OR_FMT format_string<Args...> fmt,
                        Args &&...args) {
  return {Status::kFailedPrecondition,
          Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AbortedError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kAborted, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OutOfRangeError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kOutOfRange, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnimplementedError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnimplemented, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InternalError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kInternal, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnavailableError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnavailable, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DataLossError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kDataLoss, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnauthenticatedError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kUnauthenticated, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ReturnMe(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kReturning, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ParseError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kParseError, Format(fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
LexError(AC_STD_OR_FMT format_string<Args...> fmt, Args &&...args) {
  return {Status::kLexError, Format(fmt, std::forward<Args>(args)...)};
}
#  if !AC_USE_STD_FMT
template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20
    Status OkStatus(const fmt::text_style &ts,
                    fmt::format_string<Args...> fmt,
                    Args &&...args) {
  return {Status::kOk, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
Cancelled(const fmt::text_style &ts,
          fmt::format_string<Args...> fmt,
          Args &&...args) {
  return {Status::kCancelled,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnknownError(const fmt::text_style &ts,
             fmt::format_string<Args...> fmt,
             Args &&...args) {
  return {Status::kUnknown, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InvalidArgumentError(const fmt::text_style &ts,
                     fmt::format_string<Args...> fmt,
                     Args &&...args) {
  return {Status::kInvalidArgument,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DeadlineExceededError(const fmt::text_style &ts,
                      fmt::format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kDeadlineExceeded,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
NotFoundError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kNotFound, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AlreadyExistsError(const fmt::text_style &ts,
                   fmt::format_string<Args...> fmt,
                   Args &&...args) {
  return {Status::kAlreadyExists,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
PermissionDeniedError(const fmt::text_style &ts,
                      fmt::format_string<Args...> fmt,
                      Args &&...args) {
  return {Status::kPermissionDenied,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ResourceExhaustedError(const fmt::text_style &ts,
                       fmt::format_string<Args...> fmt,
                       Args &&...args) {
  return {Status::kResourceExhausted,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
FailedPreconditionError(const fmt::text_style &ts,
                        fmt::format_string<Args...> fmt,
                        Args &&...args) {
  return {Status::kFailedPrecondition,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
AbortedError(const fmt::text_style &ts,
             fmt::format_string<Args...> fmt,
             Args &&...args) {
  return {Status::kAborted, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
OutOfRangeError(const fmt::text_style &ts,
                fmt::format_string<Args...> fmt,
                Args &&...args) {
  return {Status::kOutOfRange,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnimplementedError(const fmt::text_style &ts,
                   fmt::format_string<Args...> fmt,
                   Args &&...args) {
  return {Status::kUnimplemented,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
InternalError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kInternal, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnavailableError(const fmt::text_style &ts,
                 fmt::format_string<Args...> fmt,
                 Args &&...args) {
  return {Status::kUnavailable,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
DataLossError(const fmt::text_style &ts,
              fmt::format_string<Args...> fmt,
              Args &&...args) {
  return {Status::kDataLoss, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
UnauthenticatedError(const fmt::text_style &ts,
                     fmt::format_string<Args...> fmt,
                     Args &&...args) {
  return {Status::kUnauthenticated,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ReturnMe(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kReturning,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
ParseError(const fmt::text_style &ts,
           fmt::format_string<Args...> fmt,
           Args &&...args) {
  return {Status::kParseError,
          fmt::format(ts, fmt, std::forward<Args>(args)...)};
}

template <typename... Args>
AC_NODISCARD AC_FORCEINLINE AC_FLATTEN static inline AC_CONSTEXPR20 Status
LexError(const fmt::text_style &ts,
         fmt::format_string<Args...> fmt,
         Args &&...args) {
  return {Status::kLexError, fmt::format(ts, fmt, std::forward<Args>(args)...)};
}
#  endif

#  define AC_RETURN_IF_NOT(_status_)                                           \
    AC_AMBIGUOUS_ELSE_BLOCKER                                                  \
    if (auto _ac_auxilia_status_return_ = (_status_))                          \
      ;                                                                        \
    else {                                                                     \
      return std::move(_ac_auxilia_status_return_);                            \
    }
} // namespace auxilia
#endif // AUXILIA_STATUS_HPP
