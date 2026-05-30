#pragma once

#include <atomic>
#include <cstddef>

#include "auxilia/status/Status.hpp"
#include "os.hpp"

#ifdef _WIN32
namespace auxilia::net {
class io_context {
public:
  using native_handle_type = ::WSADATA;
  using iocp_handle_type = ::HANDLE;

private:
  native_handle_type wsa_data_;
  iocp_handle_type iocp_;
  std::atomic<bool> stopped_;
  bool initialized_;

public:
  constexpr io_context() noexcept
      : wsa_data_(), iocp_(INVALID_HANDLE_VALUE), stopped_(true),
        initialized_(false) {}
  ~io_context() noexcept { shutdown(); }

public:
  Status initialize() noexcept {
    if (initialized_)
      return AlreadyExistsError("io_context is already initialized.");

    if (::WSAStartup(MAKEWORD(2, 2), &wsa_data_) != 0)
      return details::wsa_error();

    iocp_ = ::CreateIoCompletionPort(INVALID_HANDLE_VALUE, nullptr, 0, 0);
    if (!iocp_) {
      AC_DEFER { ::WSACleanup(); };
      return details::win_error();
    }

    stopped_.store(false, std::memory_order_release);
    initialized_ = true;
    return {};
  }
  Status shutdown() noexcept {
    if (!initialized_)
      return UnavailableError(
          "io_context is not initialized, or failed to initialize.");
    stop();
    if (iocp_ && iocp_ != INVALID_HANDLE_VALUE) {
      ::CloseHandle(iocp_);
      iocp_ = INVALID_HANDLE_VALUE;
    }
    if (::WSACleanup() != 0)
      return details::wsa_error();
    stopped_.store(true, std::memory_order::release);
    initialized_ = false;
    return {};
  }
  Status associate(const details::raw_socket_t socket,
                   const ULONG_PTR key = 0) noexcept {
    if (!initialized_ || !iocp_ || iocp_ == INVALID_HANDLE_VALUE)
      return UnavailableError(
          "io_context is not initialized or IOCP is not available.");
    if (socket == details::invalid_socket)
      return details::make_ctor_error();
    auto handle = ::CreateIoCompletionPort(
        reinterpret_cast<HANDLE>(socket), iocp_, key, 0);
    if (!handle)
      return details::win_error();
    return {};
  }
  std::optional<details::iocp_completion>
  wait(const DWORD timeout_ms = INFINITE) noexcept {
    if (!initialized_ || iocp_ == INVALID_HANDLE_VALUE)
      return std::nullopt;

    DWORD bytes = 0;
    ULONG_PTR key = 0;
    OVERLAPPED *overlapped = nullptr;
    const auto ok = ::GetQueuedCompletionStatus(
        iocp_, &bytes, &key, &overlapped, timeout_ms);
    if (!overlapped)
      return std::nullopt;

    details::iocp_completion out = {
        .op = reinterpret_cast<details::iocp_operation *>(overlapped),
        .bytes = bytes,
        .error = ok ? ERROR_SUCCESS : ::GetLastError(),
    };
    return std::make_optional(std::move(out));
  }
  void run() noexcept {
    while (!stopped_.load(std::memory_order::acquire)) {
      if (auto completion = wait();
          completion && completion->op && completion->op->complete)
        completion->op->complete(
            completion->op, completion->bytes, completion->error);
    }
  }
  void stop(const size_t wake_count = 1) noexcept {
    if (!initialized_ || iocp_ == INVALID_HANDLE_VALUE)
      return;
    stopped_.store(true, std::memory_order::release);
    for (size_t i = 0; i < wake_count; ++i)
      ::PostQueuedCompletionStatus(iocp_, 0, 0, nullptr);
  }
  bool stopped() const noexcept {
    return stopped_.load(std::memory_order_acquire);
  }
  auto &&native_handle(this auto &&self) noexcept { return self.wsa_data_; }
  auto iocp_handle() const noexcept { return iocp_; }
};
} // namespace auxilia::net
#elif defined(__linux__)
namespace auxilia::net {
class io_context {
public:
  using native_handle_type = void;
  using iocp_handle_type = void;
  inline Status initialize() noexcept { return {}; }
  inline Status shutdown() noexcept { return {}; }
  inline Status associate(details::raw_socket_t, size_t = 0) noexcept {
    return {};
  }
  inline bool wait(details::iocp_completion &, unsigned long = 0) noexcept {
    return false;
  }
  inline void run() noexcept {}
  inline void stop(size_t = 1) noexcept {}
  inline bool stopped() const noexcept { return true; }
  inline auto &&native_handle(this auto &&self) noexcept { return self; }
  inline auto iocp_handle() const noexcept { return nullptr; }
};
} // namespace auxilia::net
#else
#  error "unsupported"
#endif
