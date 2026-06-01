#pragma once
#ifdef __linux__
#  include <cstdint>
#  include <fcntl.h>
#  include <netinet/in.h>
#  include <sys/epoll.h>
#  include <sys/eventfd.h>
#  include <sys/socket.h>
#  include <sys/types.h>
#  include <unistd.h>

#  include "auxilia/status/StatusOr.hpp"

#  ifdef __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif

#  include "error.hpp"

namespace auxilia::net::details::epoll {
struct dispatcher {
  using dispatch_fn = void (*)(void *, uint32_t) noexcept;
  void *self = nullptr;
  dispatch_fn dispatch = nullptr;
};
struct completion {
  dispatcher *dispatcher = nullptr;
  uint32_t events = 0;
};
using handle_t = int;
using fd_t = int;
static constexpr auto invalid = -1;
static constexpr auto invalid_eventfd = -1;
AC_FORCEINLINE inline StatusOr<handle_t> create() {
  if (auto handle = ::epoll_create1(EPOLL_CLOEXEC); handle == invalid)
    return {make_create_error()};
  else
    return handle;
}
AC_FORCEINLINE inline StatusOr<int> mod(const handle_t ep,
                                        const raw_socket_t fd,
                                        const uint32_t events,
                                        void *const data) {
  ::epoll_event ev{.events = events, .data = {.ptr = data}};
  if (auto result = ::epoll_ctl(ep, EPOLL_CTL_MOD, fd, &ev); result != 0)
    return {make_ctl_error()};
  else
    return result;
}
AC_FORCEINLINE inline StatusOr<int> add(const handle_t ep,
                                        const raw_socket_t fd,
                                        const uint32_t events,
                                        void *const data) {
  ::epoll_event ev{.events = events, .data = {.ptr = data}};
  if (auto result = ::epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev); result != 0)
    if (errno != EEXIST)
      return {make_ctl_error()};
    else
      return details::epoll::mod(ep, fd, events, data);
  else
    return result;
}
AC_FORCEINLINE inline StatusOr<int> del(const handle_t ep,
                                        const raw_socket_t fd) {
  if (auto result = ::epoll_ctl(ep, EPOLL_CTL_DEL, fd, nullptr); result != 0)
    return {make_ctl_error()};
  else
    return result;
}
AC_FORCEINLINE inline StatusOr<int>
wait_one(const handle_t ep, ::epoll_event *const event, const int timeout_ms) {
  if (auto result = ::epoll_wait(ep, event, 1, timeout_ms); result < 0)
    return {make_wait_error()};
  else
    return result;
}

AC_FORCEINLINE inline StatusOr<fd_t> eventfd_create() {
  if (auto fd = ::eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC | EFD_SEMAPHORE);
      fd == invalid_eventfd)
    return {make_eventfd_create_error()};
  else
    return fd;
}
AC_FORCEINLINE inline int eventfd_signal(const fd_t fd,
                                         const ::eventfd_t value = 1) {
  return ::eventfd_write(fd, value);
}
AC_FORCEINLINE inline int eventfd_drain(const fd_t fd) {
  ::eventfd_t value = 0;
  return ::eventfd_read(fd, &value);
}
AC_FORCEINLINE inline int set_nonblocking(const raw_socket_t fd,
                                          const bool enabled = true) {
  const auto flags = ::fcntl(fd, F_GETFL, 0);
  if (flags == -1)
    return -1;
  const int next = enabled ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK);
  return ::fcntl(fd, F_SETFL, next);
}
} // namespace auxilia::net::details::epoll

#endif
