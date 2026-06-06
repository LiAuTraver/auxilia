#pragma once

#ifdef _WIN32

#  if __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif

namespace auxilia::net::details {
class io_context {
public:
  using native_handle_type = ::HANDLE;

private:
  ::WSADATA wsa_data_;
  native_handle_type iocp_;
  std::atomic<bool> stopped_;
  bool initialized_;

public:
  constexpr io_context() noexcept
      : wsa_data_(), iocp_(INVALID_HANDLE_VALUE), stopped_(true),
        initialized_(false) {}
  ~io_context() noexcept { shutdown().log_err(); }

public:
  Status initialize() noexcept {
    if (initialized_)
      return AlreadyExistsError("io_context is already initialized.");

    if (::WSAStartup(MAKEWORD(2, 2), &wsa_data_) != 0)
      return wsa_error();

    iocp_ = ::CreateIoCompletionPort(INVALID_HANDLE_VALUE, nullptr, 0, 0);
    if (!iocp_) {
      AC_DEFER { ::WSACleanup(); };
      return win_error();
    }

    stopped_.store(false, std::memory_order::release);
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
      return wsa_error();
    stopped_.store(true, std::memory_order::release);
    initialized_ = false;
    return {};
  }
  Status associate(const SOCKET socket, const ULONG_PTR key = 0) noexcept {
    if (!initialized_ || !iocp_ || iocp_ == INVALID_HANDLE_VALUE)
      return UnavailableError(
          "io_context is not initialized or IOCP is not available.");
    if (socket == INVALID_SOCKET)
      return InvalidArgumentError("socket is in a invalid state.");

    auto handle = ::CreateIoCompletionPort(
        reinterpret_cast<HANDLE>(socket), iocp_, key, 0);
    if (!handle)
      return win_error();
    return {};
  }
  std::optional<iocp::completion>
  wait(const DWORD timeout_ms = INFINITE) noexcept {
    if (!initialized_ || iocp_ == INVALID_HANDLE_VALUE)
      return std::nullopt;

    ULONG_PTR key = 0;
    iocp::completion out;

    out.error =
        ::GetQueuedCompletionStatus(iocp_,
                                    &out.bytes,
                                    &key,
                                    reinterpret_cast<OVERLAPPED **>(&out.op),
                                    timeout_ms)
            ? ERROR_SUCCESS
            : ::GetLastError();

    // FIXME: shall we check whether op is nullptr or leave it to caller?
    if (!out.op)
      return std::nullopt;

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
    return stopped_.load(std::memory_order::acquire);
  }
  auto &&native_handle(this auto &&self) noexcept { return self.iocp_; }
};
} // namespace auxilia::net::details

#endif
