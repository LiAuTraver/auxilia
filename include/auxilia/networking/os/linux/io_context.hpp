#pragma once
#ifdef __linux__

#  include "auxilia/status/StatusOr.hpp"

#  ifdef __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif
namespace auxilia::net::details {
class io_context {
public:
  using native_handle_type = epoll::handle_t;

private:
  native_handle_type epoll_ = epoll::invalid;
  epoll::fd_t wake_fd_ = epoll::invalid_eventfd;
  std::atomic<bool> stopped_ = true;
  bool initialized_ = false;
  epoll::dispatcher wake_dispatcher_{};

  static void wake_dispatch(void *self, uint32_t) noexcept {
    if (!self ||
        static_cast<io_context *>(self)->wake_fd_ == epoll::invalid_eventfd)
      return;
    epoll::eventfd_drain(static_cast<io_context *>(self)->wake_fd_);
  }

public:
  constexpr io_context() noexcept = default;
  ~io_context() noexcept { shutdown().log_err(); }

public:
  Status initialize() noexcept {
    if (initialized_)
      return AlreadyExistsError("io_context is already initialized.");

    if (auto epoll = epoll::create())
      epoll_ = *std::move(epoll);
    else
      return std::move(epoll).as_status();

    if (auto wake_fd = epoll::eventfd_create()) {
      wake_fd_ = *std::move(wake_fd);
    } else {
      closesocket(epoll_).log_err();
      epoll_ = epoll::invalid;
      return std::move(wake_fd).as_status();
    }

    wake_dispatcher_.self = this;
    wake_dispatcher_.dispatch = io_context::wake_dispatch;
    const auto wake_events = static_cast<uint32_t>(EPOLLIN);
    if (auto status =
            epoll::add(epoll_, wake_fd_, wake_events, &wake_dispatcher_);
        !status) {

      closesocket(wake_fd_).log_err();
      closesocket(epoll_).log_err();

      wake_fd_ = epoll::invalid_eventfd;
      epoll_ = epoll::invalid;
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
    if (wake_fd_ != epoll::invalid_eventfd) {
      closesocket(wake_fd_).log_err();
      wake_fd_ = epoll::invalid_eventfd;
    }
    if (epoll_ != epoll::invalid) {
      closesocket(epoll_).log_err();
      epoll_ = epoll::invalid;
    }
    stopped_.store(true, std::memory_order::release);
    initialized_ = false;
    return {};
  }
  Status associate(const raw_socket_t socket, const size_t key = 0) noexcept {
    if (!initialized_ || epoll_ == epoll::invalid)
      return UnavailableError(
          "io_context is not initialized or epoll is not available.");
    if (socket == invalid_socket)
      return InvalidArgumentError("socket is in a invalid state.");

    const uint32_t events =
        EPOLLIN | EPOLLOUT | EPOLLERR | EPOLLHUP | EPOLLRDHUP | EPOLLET;

    return epoll::add(epoll_, socket, events, reinterpret_cast<void *>(key))
        .as_status();
  }
  std::optional<epoll::completion> wait(const int timeout_ms = -1) noexcept {
    if (!initialized_ || epoll_ == epoll::invalid)
      return std::nullopt;

    ::epoll_event event{};

    return epoll::wait_one(epoll_, &event, timeout_ms)
        .transform_error<Status::log_err_>()
        .to_optional()
        .transform([event = std::move(event)](auto &&) {
          return epoll::completion{
              .dispatcher = static_cast<epoll::dispatcher *>(event.data.ptr),
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
    if (!initialized_ || epoll_ == epoll::invalid)
      return;
    stopped_.store(true, std::memory_order::release);
    if (wake_fd_ == epoll::invalid_eventfd)
      return;
    for (size_t i = 0; i < wake_count; ++i)
      epoll::eventfd_signal(wake_fd_, 1);
  }
  [[nodiscard]] bool stopped() const noexcept {
    return stopped_.load(std::memory_order::acquire);
  }
  auto &&native_handle(this auto &&self) noexcept { return self.epoll_; }
};
} // namespace auxilia::net::details
#endif
