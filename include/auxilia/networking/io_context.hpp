#pragma once

#include <atomic>
#include <cstddef>
#include <optional>

#include <cerrno>
#include <cstring>

#include "auxilia/status/Status.hpp"
#include "os.hpp"

#ifdef _WIN32
namespace auxilia::net {
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
  ~io_context() noexcept { shutdown().log(); }

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
      return InvalidArgumentError("socket is in a invalid state.");

    auto handle = ::CreateIoCompletionPort(
        reinterpret_cast<HANDLE>(socket), iocp_, key, 0);
    if (!handle)
      return details::win_error();
    return {};
  }
  std::optional<details::iocp::completion>
  wait(const DWORD timeout_ms = INFINITE) noexcept {
    if (!initialized_ || iocp_ == INVALID_HANDLE_VALUE)
      return std::nullopt;

    ULONG_PTR key = 0;
    details::iocp::completion out;

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
} // namespace auxilia::net
#elif defined(__linux__)
namespace auxilia::net {
class io_context {
public:
  using native_handle_type = details::epoll::handle_t;

private:
  native_handle_type epoll_ = details::epoll::invalid;
  details::epoll::fd_t wake_fd_ = details::epoll::invalid_eventfd;
  std::atomic<bool> stopped_ = true;
  bool initialized_ = false;
  details::epoll::dispatcher wake_dispatcher_{};

  static void wake_dispatch(void *self, uint32_t) noexcept {
    if (!self || static_cast<io_context *>(self)->wake_fd_ ==
                     details::epoll::invalid_eventfd)
      return;
    details::epoll::eventfd_drain(static_cast<io_context *>(self)->wake_fd_);
  }

public:
  constexpr io_context() noexcept = default;
  ~io_context() noexcept { shutdown().log_err(); }

public:
  Status initialize() noexcept {
    if (initialized_)
      return AlreadyExistsError("io_context is already initialized.");

    if (auto epoll = details::epoll::create())
      epoll_ = *std::move(epoll);
    else
      return std::move(epoll).as_status();

    if (auto wake_fd = details::epoll::eventfd_create()) {
      wake_fd_ = *std::move(wake_fd);
    } else {
      details::closesocket(epoll_).log_err();
      epoll_ = details::epoll::invalid;
      return std::move(wake_fd).as_status();
    }

    wake_dispatcher_.self = this;
    wake_dispatcher_.dispatch = io_context::wake_dispatch;
    const auto wake_events = static_cast<uint32_t>(EPOLLIN);
    if (auto status = details::epoll::add(
            epoll_, wake_fd_, wake_events, &wake_dispatcher_);
        !status) {

      details::closesocket(wake_fd_).log_err();
      details::closesocket(epoll_).log_err();

      wake_fd_ = details::epoll::invalid_eventfd;
      epoll_ = details::epoll::invalid;
      return std::move(status).as_status();
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
    if (wake_fd_ != details::epoll::invalid_eventfd) {
      details::closesocket(wake_fd_).log_err();
      wake_fd_ = details::epoll::invalid_eventfd;
    }
    if (epoll_ != details::epoll::invalid) {
      details::closesocket(epoll_).log_err();
      epoll_ = details::epoll::invalid;
    }
    stopped_.store(true, std::memory_order::release);
    initialized_ = false;
    return {};
  }
  [[deprecated(
      "NOTE: the linux part of I/O Epoll-related async is assisted by AI and I'm still learning and trying to grasp."
      "And it has seems deadlock bug!")]]
  Status associate(const details::raw_socket_t socket,
                   const size_t key = 0) noexcept {
    if (!initialized_ || epoll_ == details::epoll::invalid)
      return UnavailableError(
          "io_context is not initialized or epoll is not available.");
    if (socket == details::invalid_socket)
      return InvalidArgumentError("socket is in a invalid state.");

    const uint32_t events =
        EPOLLIN | EPOLLOUT | EPOLLERR | EPOLLHUP | EPOLLRDHUP | EPOLLET;

    return details::epoll::add(
               epoll_, socket, events, reinterpret_cast<void *>(key))
        .as_status();
  }
  std::optional<details::epoll::completion>
  wait(const int timeout_ms = -1) noexcept {
    if (!initialized_ || epoll_ == details::epoll::invalid)
      return std::nullopt;

    ::epoll_event event{};

    return details::epoll::wait_one(epoll_, &event, timeout_ms)
        .transform_error<Status::log_err_>()
        .to_optional()
        .transform([event = std::move(event)](auto &&) {
          return details::epoll::completion{
              .dispatcher =
                  static_cast<details::epoll::dispatcher *>(event.data.ptr),
              .events = event.events,
          };
        });
  }
  void run() noexcept {
    while (!stopped_.load(std::memory_order::acquire)) {
      if (auto completion = wait(); completion && completion->dispatcher &&
                                    completion->dispatcher->dispatch)
        completion->dispatcher->dispatch(completion->dispatcher->self,
                                         completion->events);
    }
  }
  void stop(const size_t wake_count = 1) noexcept {
    if (!initialized_ || epoll_ == details::epoll::invalid)
      return;
    stopped_.store(true, std::memory_order::release);
    if (wake_fd_ == details::epoll::invalid_eventfd)
      return;
    for (size_t i = 0; i < wake_count; ++i)
      details::epoll::eventfd_signal(wake_fd_, 1);
  }
  [[nodiscard]] bool stopped() const noexcept {
    return stopped_.load(std::memory_order::acquire);
  }
  auto &&native_handle(this auto &&self) noexcept { return self.epoll_; }
};
} // namespace auxilia::net
#else
#  error "unsupported"
#endif
