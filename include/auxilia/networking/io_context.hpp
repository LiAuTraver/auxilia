#pragma once

#include "auxilia/status/Status.hpp"
#include "os.hpp"
#ifdef _WIN32
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
  ~io_context() noexcept { shutdown(); }

public:
  Status initialize() noexcept {
    if (initialized_)
      return AlreadyExistsError("io_context is already initialized.");

    if (::WSAStartup(MAKEWORD(2, 2), &wsa_data_) != 0)
      return details::wsa_error();

    initialized_ = true;
    return {};
  }
  Status shutdown() noexcept {
    if (!initialized_)
      return UnavailableError(
          "io_context is not initialized, or failed to initialize.");
    if (::WSACleanup() != 0)
      return details::wsa_error();
    initialized_ = false;
    return {};
  }
  auto &&native_handle(this auto &&self) noexcept { return self.wsa_data_; }
};
} // namespace auxilia::net
#elif defined(__linux__)
namespace auxilia::net {
class io_context {
public:
  using native_handle_type = void;
  inline Status initialize() noexcept { return {}; }
  inline Status shutdown() noexcept { return {}; }
  inline auto &&native_handle(this auto &&self) noexcept { return self; }
};
} // namespace auxilia::net
#else
#  error "unsupported"
#endif
