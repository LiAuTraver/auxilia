#pragma once

#include "os.hpp"

namespace auxilia::net {
class io_context {
public:
  using native_handle_type = ::WSADATA;

private:
  native_handle_type wsa_data_;
  bool initialized_;

public:
  constexpr io_context() noexcept
      : wsa_data_(native_handle_type{}), initialized_(false) {}
  ~io_context() noexcept {
    if (initialized_)
      ::WSACleanup();
  }

public:
  Status initialize() noexcept {
    if (initialized_)
      return AlreadyExistsError("io_context is already initialized.");

    if (::WSAStartup(MAKEWORD(2, 2), &wsa_data_) != 0)
      return details::wsa_error();

    initialized_ = true;
    return {};
  }
  auto &&native_handle(this auto &&self) noexcept { return self.wsa_data_; }
};
} // namespace auxilia::net
