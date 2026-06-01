#pragma once

#include <algorithm>
#include <atomic>
#include <cerrno>
#include <cstddef>
#include <cstring>
#include <deque>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "auxilia/base/macros.hpp"
#include "auxilia/base/format.hpp"
#include "auxilia/status/Status.hpp"
#include "auxilia/status/StatusOr.hpp"
#include "auxilia/meta/container_traits.hpp"

#include "os.hpp"
#include "ip.hpp"
#include "os/windows/iocp.hpp"
#include "protocol.hpp"
#include <cstring>
#include <functional>

#include "io_context.hpp"
#include "endpoint.hpp"
namespace auxilia::net::details {

template <typename Protocol> class socket_base : Printable {

public:
  using protocol_type = Protocol;
  using native_handle_type = details::raw_socket_t;
  using endpoint_type = protocol_type::endpoint_type;
  using address_type = ip::address;
  using bytes_view_type = std::string_view;
  using bytes_type = std::string; // a vec of bytes
  using socket_type = protocol_type::socket_type;
  static_assert(InternetProtocol<protocol_type>);
  static_assert(container_traits<bytes_type>::is_reservable);
  static_assert(container_traits<bytes_type>::is_contiguous);

public:
  inline socket_base(
      io_context *context = nullptr,
      native_handle_type handle = invalid_socket,
      std::optional<endpoint_type> remote_endpoint = std::nullopt) noexcept;
  inline socket_base(io_context &context,
                     const native_handle_type handle = invalid_socket) noexcept
      : socket_base(&context, handle) {}
  inline socket_base(io_context *context, const ip::family family) noexcept
      : socket_base(context,
                    details::socket(family, protocol_type::socket_kind())) {}
  inline socket_base(io_context &context, const ip::family family) noexcept
      : socket_base(&context,
                    details::socket(family, protocol_type::socket_kind())) {}
  socket_base(const socket_base &) = delete;
  socket_base &operator=(const socket_base &) = delete;
  inline socket_base(socket_base &&that) noexcept;
  socket_base &operator=(socket_base &&that) noexcept;

  AC_NODISCARD_REASON("the `recv` method returns the received bytes. ")
  inline decltype(auto) recv(this auto &&self, const size_t max_size = 0x0400) {
    return self.do_recv(max_size);
  }

protected:
  inline ~socket_base() noexcept { close().log(); }

public:
  inline static StatusOr<socket_type> v4(io_context *context = nullptr);
  inline static StatusOr<socket_type> v6(io_context *context = nullptr);

public:
  inline Status close() const;
  constexpr auto native_handle() const noexcept { return handle_; }
  constexpr auto context() const noexcept { return context_; }
  constexpr auto is_valid() const noexcept {
    return handle_ != details::invalid_socket;
  }
  template <ip::family family = ip::family::v4>
  inline auto bind(const endpoint_type::port_type port = 0) {
    return bind(endpoint_type::template unspecified<family>(port));
  }
  Status bind(const endpoint_type &endpoint);

  inline auto remote_endpoint() const noexcept { return remote_endpoint_; }
  auto to_string(const FormatPolicy policy) const { return Format(handle_); }

public:
  Status connect(const endpoint_type &endpoint);

public:
  struct {
    static auto operator()(auto &&self) { return self.recv(); }
    static auto operator()(auto &&self, const size_t max_size) {
      return self.recv(max_size);
    }
  }

  /// monadic shorthand
  static constexpr inline recv_;

  struct {
    static auto operator()(auto &&self, bytes_type &&bytes) {
      return self.send_bytes(std::move(bytes));
    }
  }

  /// monadic shorthand
  static constexpr inline send_bytes_;

protected:
  struct AC_EMPTY_BASES AC_NOVTABLE CallableMixin {
    template <typename... Args>
    inline constexpr decltype(auto)
    operator()(this auto &&self, Args &&...args) noexcept(
        noexcept(self.handler(std::forward<Args>(args)...))) {
      if (self.handler)
        self.handler(std::forward<Args>(args)...);
    }
  };
#ifdef _WIN32
  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE iocp_base : CallableMixin,
                                                details::iocp_operation {
    static_assert(
        is_specialization_v<Handler, std::move_only_function>,
        "only support move only function right now, "
        "since I really dont know how to handle function ref or copyable function.");
    using handler_type = Handler;
    using buffer_type = ::WSABUF;

    bytes_type buffer;
    buffer_type wsa_buf;
    handler_type handler;
    socket_type *self;

  protected:
    using iocp_operation::iocp_operation;
  };
  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE iocp_send_base : iocp_base<Handler> {
    using base_type = iocp_base<Handler>;
    inline iocp_send_base(socket_type *const self,
                          base_type::complete_fn fn,
                          Handler &&handler,
                          bytes_type &&buffer) noexcept
        : base_type(fn) {
      this->buffer = std::move(buffer);
      this->wsa_buf = {
          .len = static_cast<ULONG>(this->buffer.size()),
          .buf = this->buffer.data(),
      };
      this->handler = std::move(handler);
      this->self = self;
    }
  };
  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE iocp_recv_base : iocp_base<Handler> {
    using base_type = iocp_base<Handler>;
    inline iocp_recv_base(socket_type *const self,
                          base_type::complete_fn fn,
                          Handler &&handler,
                          const size_t max_size) noexcept
        : base_type(fn) {
      this->buffer.resize(max_size);
      this->wsa_buf = {
          .len = static_cast<ULONG>(this->buffer.size()),
          .buf = this->buffer.data(),
      };
      this->handler = std::move(handler);
      this->self = self;
      this->flags = 0;
    }
    DWORD flags;
  };

  template <typename Op, typename StartFn, typename ErrorFn>
  static Status
  start_overlapped(Op *op, StartFn &&start_fn, ErrorFn &&error_fn) {
    if (start_fn() == SOCKET_ERROR && ::WSAGetLastError() != WSA_IO_PENDING) {
      delete op;
      return error_fn();
    }
    return {};
  }
  template <typename Op>
  static void
  finish_send(Op *op, const DWORD bytes, const DWORD error) noexcept {

    AC_DEFER { delete op; };

    if (error != ERROR_SUCCESS) {
      ::WSASetLastError(static_cast<int>(error));
      (*op)(details::make_send_error());
    } else {
      (*op)(static_cast<size_t>(bytes));
    }
  }
#elif defined(__linux__)
  struct AC_EMPTY_BASES AC_NOVTABLE epoll_socket_state;

  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE epoll_base : CallableMixin {
    using handler_type = Handler;
    bytes_type buffer;
    handler_type handler;
    socket_type *self;

  protected:
    inline ~epoll_base() noexcept = default;
  };
  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE epoll_send_base : epoll_base<Handler> {
    using base_type = epoll_base<Handler>;
    inline epoll_send_base(socket_type *const self,
                           base_type::handler_type &&handler,
                           bytes_type &&buffer) noexcept {
      this->self = self;
      this->handler = std::move(handler);
      this->buffer = std::move(buffer);
      this->offset = 0;
    }
    size_t offset;
  };
  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE epoll_recv_base : epoll_base<Handler> {
    using base_type = epoll_base<Handler>;
    inline epoll_recv_base(socket_type *const self,
                           base_type::handler_type &&handler,
                           const size_t max_size) noexcept {
      this->self = self;
      this->handler = std::move(handler);
      this->buffer.resize(max_size);
    }
  };

  struct AC_EMPTY_BASES AC_NOVTABLE epoll_operation {
    using on_fn =
        std::move_only_function<bool(void *, details::raw_socket_t, uint32_t)>;
    using cancel_fn = std::move_only_function<void(void *, const Status &)>;
    using destroy_fn = std::move_only_function<void(void *)>;
    inline epoll_operation(void *self = nullptr,
                           on_fn &&on_op = nullptr,
                           cancel_fn &&on_cancel = nullptr,
                           destroy_fn &&on_destroy = nullptr) noexcept
        : self(self), on_op(std::move(on_op)), on_cancel(std::move(on_cancel)),
          on_destroy(std::move(on_destroy)) {}
    inline epoll_operation(const epoll_operation &) = delete;
    inline epoll_operation &operator=(const epoll_operation &) = delete;
    inline epoll_operation(epoll_operation &&that) noexcept
        : self(that.self), on_op(std::exchange(that.on_op, nullptr)),
          on_cancel(std::exchange(that.on_cancel, nullptr)),
          on_destroy(std::exchange(that.on_destroy, nullptr)) {}
    inline epoll_operation &operator=(epoll_operation &&that) noexcept {
      if (this != &that) {
        reset();
        self = std::exchange(that.self, nullptr);
        on_op = std::exchange(that.on_op, nullptr);
        on_cancel = std::exchange(that.on_cancel, nullptr);
        on_destroy = std::exchange(that.on_destroy, nullptr);
      }
      return *this;
    }
    void reset() noexcept {
      if (on_destroy && self)
        on_destroy(self);
      AC_DEBUG_ONLY([&] {
        self = nullptr;
        on_op = nullptr;
        on_cancel = nullptr;
        on_destroy = nullptr;
      }();)
    }
    void cancel(const Status &error) noexcept {
      if (on_cancel && self)
        on_cancel(self, error);
    }

    void *self;
    on_fn on_op;
    cancel_fn on_cancel;
    destroy_fn on_destroy;

  protected:
    inline ~epoll_operation() noexcept { reset(); }
    bool call_on_op(const details::raw_socket_t fd,
                    const uint32_t events) noexcept {
      return on_op ? on_op(self, fd, events) : true;
    }
  };

  struct AC_EMPTY_BASES AC_NOVTABLE read_operation : epoll_operation {
    using base_type = epoll_operation;
    using base_type::epoll_operation;
    inline auto read(const details::raw_socket_t fd,
                     const uint32_t events) noexcept {
      return this->call_on_op(fd, events);
    }

    template <typename Op, typename... Args>
    static read_operation make(Args &&...args) {
      return {
          new Op(std::forward<Args>(args)...),
          [](auto self, auto fd, auto events) noexcept {
            return static_cast<Op *>(self)->on_read(fd, events);
          },
          [](auto self, auto error) noexcept {
            static_cast<Op *>(self)->cancel(error);
          },
          [](auto self) noexcept { delete static_cast<Op *>(self); },
      };
    }
  };
  struct AC_EMPTY_BASES AC_NOVTABLE write_operation : epoll_operation {
    using base_type = epoll_operation;
    using base_type::epoll_operation;

    inline auto write(const details::raw_socket_t fd,
                      const uint32_t events) noexcept {
      return this->call_on_op(fd, events);
    }

    template <typename Op, typename... Args>
    static write_operation make(Args &&...args) {
      return {
          new Op(std::forward<Args>(args)...),
          [](auto self, auto fd, auto events) noexcept {
            return static_cast<Op *>(self)->on_write(fd, events);
          },
          [](auto self, auto error) noexcept {
            static_cast<Op *>(self)->cancel(error);
          },
          [](auto self) noexcept { delete static_cast<Op *>(self); },
      };
    }
  };
  struct AC_EMPTY_BASES AC_NOVTABLE epoll_socket_state {
    raw_socket_t fd;
    std::mutex mutex;
    std::deque<read_operation> read_ops;
    std::deque<write_operation> write_ops;
    std::atomic_flag busy = ATOMIC_FLAG_INIT;
    std::atomic<bool> non_blocking{false};
    bool closing = false;
    details::epoll::dispatcher dispatcher{};

    explicit epoll_socket_state(const raw_socket_t handle) noexcept
        : fd(handle) {
      dispatcher.self = this;
      dispatcher.dispatch = epoll_socket_state::dispatch;
    }

    Status ensure_nonblocking() noexcept {
      bool expected = false;
      if (!non_blocking.compare_exchange_strong(
              expected, true, std::memory_order::acq_rel))
        return {};
      if (details::epoll::set_nonblocking(fd, true) != 0) {
        non_blocking.store(false, std::memory_order::release);
        return UnknownError(::strerror(errno));
      }
      return {};
    }
    template <typename Op, typename... Args>
    void enqueue_read(Args &&...args) noexcept {
      read_operation op =
          read_operation::template make<Op>(std::forward<Args>(args)...);
      {
        std::scoped_lock lock(mutex);
        if (closing) {
          op.cancel(OkStatus("socket closed"));
          return;
        }
        read_ops.emplace_back(std::move(op));
      }
      on_event(EPOLLIN);
    }
    template <typename Op, typename... Args>
    void enqueue_write(Args &&...args) noexcept {
      write_operation op =
          write_operation::template make<Op>(std::forward<Args>(args)...);
      {
        std::scoped_lock lock(mutex);
        if (closing) {
          op.cancel(OkStatus("socket closed"));
          return;
        }
        write_ops.emplace_back(std::move(op));
      }
      on_event(EPOLLOUT);
    }
    void cancel_all(const Status &error) noexcept {
      std::deque<read_operation> reads;
      std::deque<write_operation> writes;
      {
        std::scoped_lock lock(mutex);
        if (closing)
          return;
        closing = true;
        reads.swap(read_ops);
        writes.swap(write_ops);
      }
      std::ranges::for_each(reads, [&error](auto &&op) { op.cancel(error); });
      std::ranges::for_each(writes, [&error](auto &&op) { op.cancel(error); });
    }

    static void dispatch(void *self, const uint32_t events) noexcept {
      if (!self)
        return;
      static_cast<epoll_socket_state *>(self)->on_event(events);
    }
    void on_event(const uint32_t events) noexcept {
      if (closing)
        return;
      if (busy.test_and_set(std::memory_order::acquire))
        return;
      AC_DEFER { busy.clear(std::memory_order::release); };

      constexpr uint32_t read_mask = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLRDHUP;
      constexpr uint32_t write_mask = EPOLLOUT | EPOLLERR | EPOLLHUP;

      if (events & read_mask)
        drain_reads(events);
      if (events & write_mask)
        drain_writes(events);
    }

    void drain_reads(const uint32_t events) noexcept {
      for (;;) {
        read_operation op;
        {
          std::scoped_lock lock(mutex);
          if (read_ops.empty())
            break;
          op = std::move(read_ops.front());
          read_ops.pop_front();
        }
        if (!op.read(fd, events)) {
          std::scoped_lock lock(mutex);
          read_ops.emplace_front(std::move(op));
          break;
        }
      }
    }
    void drain_writes(const uint32_t events) noexcept {
      for (;;) {
        write_operation op;
        {
          std::scoped_lock lock(mutex);
          if (write_ops.empty())
            break;
          op = std::move(write_ops.front());
          write_ops.pop_front();
        }
        if (!op.write(fd, events)) {
          std::scoped_lock lock(mutex);
          write_ops.emplace_front(std::move(op));
          break;
        }
      }
    }
  };
#endif

protected:
  io_context *context_;
  native_handle_type handle_;
  /// FIXME: race condition in async operations
  std::optional<endpoint_type> remote_endpoint_;
#ifdef __linux__
  mutable std::unique_ptr<epoll_socket_state> linux_state_;
#endif
};
template <typename Protocol>
inline socket_base<Protocol>::socket_base(
    io_context *context,
    const native_handle_type handle,
    std::optional<endpoint_type> remote_endpoint) noexcept
    : context_(context), handle_(handle),
      remote_endpoint_(std::move(remote_endpoint)) {
  if (handle_ == invalid_socket)
    return;
  AC_RUNTIME_ASSERT(context_, "no context while handle is valid")
#ifdef __linux__
  int opt = 1;
  ::setsockopt(handle_, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
  linux_state_ = std::make_unique<epoll_socket_state>(handle_);
  context_
      ->associate(handle_, reinterpret_cast<size_t>(&linux_state_->dispatcher))
      .log();
#else
  const char opt = 1;
  ::setsockopt(handle_, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
  context_->associate(handle_).log();
#endif
}
template <typename Protocol>
inline socket_base<Protocol>::socket_base(socket_base &&that) noexcept
    : context_(std::exchange(that.context_, nullptr)),
      handle_(std::exchange(that.handle_, invalid_socket)),
      remote_endpoint_(std::move(that.remote_endpoint_))
#ifdef __linux__
      ,
      linux_state_(std::exchange(that.linux_state_, nullptr))
#endif
{
}
template <typename Protocol>
inline socket_base<Protocol> &
socket_base<Protocol>::operator=(socket_base &&that) noexcept {
  if (this != &that) {
    close().log();
    context_ = std::exchange(that.context_, nullptr);
    handle_ = std::exchange(that.handle_, invalid_socket);
    remote_endpoint_ = std::move(that.remote_endpoint_);
#ifdef __linux__
    linux_state_ = std::exchange(that.linux_state_, nullptr);
#endif
  }
  return *this;
}
template <typename Protocol>
StatusOr<typename socket_base<Protocol>::socket_type> inline socket_base<
    Protocol>::v4(io_context *context) {
  if (auto self = socket_type(context, ip::family::v4); self.is_valid())
      [[likely]]
    return {std::move(self)};
  else [[unlikely]]
    return {details::make_ctor_error()};
}
template <typename Protocol>
StatusOr<typename socket_base<Protocol>::socket_type> inline socket_base<
    Protocol>::v6(io_context *context) {
  if (auto self = socket_type(context, ip::family::v6); self.is_valid())
      [[likely]]
    return {std::move(self)};
  else [[unlikely]]
    return {details::make_ctor_error()};
}
template <typename Protocol>
inline Status socket_base<Protocol>::close() const {
  if (is_valid()) [[likely]] {
#ifdef __linux__
    if (linux_state_)
      linux_state_->cancel_all(OkStatus("socket closed"));
    if (context_ && context_->native_handle() != details::epoll::invalid)
      details::epoll::del(context_->native_handle(), handle_);
#endif
    return details::closesocket(handle_);
  } else
    return {};
}
template <typename Protocol>
inline Status socket_base<Protocol>::bind(const endpoint_type &endpoint) {
  if (details::bind(handle_, endpoint.data(), endpoint.size()) != -1) [[likely]]
    return {};
  else [[unlikely]]
    return details::make_bind_error();
}
template <typename Protocol>
inline Status socket_base<Protocol>::connect(const endpoint_type &endpoint) {
  if (details::connect(handle_, endpoint.data(), endpoint.size()) != -1)
      [[likely]] {
    remote_endpoint_.emplace(endpoint);
    return {};
  } else [[unlikely]]
    return details::make_connect_error();
}
} // namespace auxilia::net::details
namespace auxilia::net {
template <typename FakeT> class socket {
  consteval socket() noexcept { always_false<socket<FakeT>>("not supported"); }
};
template <> class socket<tcp> : public details::socket_base<tcp> {

public:
  using base_type = socket_base<tcp>;
  using base_type::socket_base;
  friend base_type;
  socket(socket &&socket) noexcept
      : base_type::socket_base(std::move(socket)) {}
  socket &operator=(socket &&socket) noexcept {
    base_type::operator=(std::move(socket));
    return *this;
  }
  ~socket() = default;

public:
  Status listen(const int backlog = 0) {
    return details::listen(handle_, backlog);
  }

  struct {
    static auto operator()(auto &&self) { return self.listen(); }
    static auto operator()(auto &&self, const int backlog) {
      return self.listen(backlog);
    }
  }
  /// monadic shorthand
  static constexpr inline listen_;

protected:
  StatusOr<bytes_type> do_recv(const size_t max_size) {
    if (!remote_endpoint_)
      return UnavailableError(
          "Remote endpoint is not set. make sure you called accept() and it succeeded.");

    bytes_type buffer;
    auto success = true;
    buffer.resize_and_overwrite(
        max_size,
        [this, &success](char *buf, const size_t buf_size) noexcept -> size_t {
          auto res = details::recv(handle_, buf, buf_size, 0);
          if (res < 0) {
            success = false;
            return 0;
          } else
            return res;
        });
    if (success)
      return {std::move(buffer)};
    else
      return details::make_recv_error();
  }

public:
  AC_NODISCARD_REASON(
      "the `accept` method returns a new socket that you should use to communicate with the client.")
  inline StatusOr<socket> accept() {
    details::socket_len_type len = sizeof(details::socket_storage_type);
    details::socket_storage_type storage;
    if (auto acc = details::accept(
            handle_, reinterpret_cast<details::sockaddr_t *>(&storage), &len);
        acc == -1)
      return details::make_accept_error();
    else if (len == sizeof(details::sockaddr_in4_t) ||
             len == sizeof(details::sockaddr_in6_t))
      return {{context_, acc, reinterpret_cast<endpoint_type &&>(storage)}};
    else
      return UnavailableError(
          "Unknown or unsupported address family of the accepted socket.");
  }
  Status send_bytes(bytes_type &&bytes) {
    if (details::send(handle_, std::move(bytes), 0, nullptr, 0) != -1)
        [[likely]]
      return {};
    else [[unlikely]]
      return details::make_send_error();
  }

public:
  using recv_handler = std::move_only_function<void(StatusOr<bytes_type>)>;
  using send_handler = std::move_only_function<void(StatusOr<size_t>)>;

  Status async_recv(const size_t max_size, recv_handler handler) {
#ifdef _WIN32
    // lifetime of `op` outlives this function, ends in `handle_recv`
    auto *op = new recv_operation{
        this, socket::handle_recv, std::move(handler), max_size};

    DWORD bytes = 0;
    return base_type::start_overlapped(
        op,
        [&] {
          return ::WSARecv(handle_,
                           &op->wsa_buf,
                           1,
                           &bytes,
                           &op->flags,
                           &op->overlapped,
                           nullptr);
        },
        details::make_recv_error);
#elif defined(__linux__)
    if (!context_ || !linux_state_ || !is_valid())
      return UnavailableError("io_context is not available.");
    if (auto status = linux_state_->ensure_nonblocking(); !status)
      return status;
    linux_state_->template enqueue_read<recv_operation>(
        this, std::move(handler), max_size);
    return {};
#else
    (void)max_size;
    (void)handler;
    return UnavailableError("async_recv is not supported.");
#endif
  }

  Status async_send(bytes_type bytes, send_handler handler) {
#ifdef _WIN32
    auto *op = new send_operation{
        this,
        socket::handle_send,
        std::move(handler),
        std::move(bytes),
    };

    DWORD bytes_sent = 0;
    return base_type::start_overlapped(
        op,
        [&] {
          return ::WSASend(handle_,
                           &op->wsa_buf,
                           1,
                           &bytes_sent,
                           0,
                           &op->overlapped,
                           nullptr);
        },
        details::make_recv_error);
#elif defined(__linux__)
    if (!context_ || !linux_state_ || !is_valid())
      return UnavailableError("io_context is not available.");
    if (auto status = linux_state_->ensure_nonblocking(); !status)
      return status;

    linux_state_->template enqueue_write<send_operation>(
        this, std::move(handler), std::move(bytes));
    return {};
#else
    (void)bytes;
    (void)handler;
    return UnavailableError("async_send is not supported.");
#endif
  }

private:
#ifdef _WIN32
  struct AC_EMPTY_BASES AC_NOVTABLE recv_operation
      : base_type::template iocp_recv_base<recv_handler> {
    using iocp_recv_base::iocp_recv_base;
  };
  struct AC_EMPTY_BASES AC_NOVTABLE send_operation
      : base_type::template iocp_send_base<send_handler> {
    using iocp_send_base::iocp_send_base;
  };

  static void handle_recv(details::iocp_operation *base,
                          DWORD bytes,
                          DWORD error) noexcept {
    auto *op = static_cast<recv_operation *>(base);
    AC_DEFER { delete op; };
    if (error != ERROR_SUCCESS) {
      ::WSASetLastError(static_cast<int>(error));
      (*op)(details::make_recv_error());

      return;
    }
    op->buffer.resize(bytes);
    (*op)(std::move(op->buffer));
  }

  static void handle_send(details::iocp_operation *base,
                          DWORD bytes,
                          DWORD error) noexcept {
    auto *op = static_cast<send_operation *>(base);
    base_type::finish_send(op, bytes, error);
  }
#elif defined(__linux__)
  struct AC_EMPTY_BASES AC_NOVTABLE recv_operation
      : base_type::epoll_recv_base<recv_handler> {
    using epoll_recv_base::epoll_recv_base;

    bool on_read(const details::raw_socket_t fd,
                 const uint32_t events) noexcept {
      (void)events;
      const auto res = details::recv(fd, buffer.data(), buffer.size(), 0);
      if (res < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
          return false;
        (*this)(details::make_recv_error());
        return true;
      }
      buffer.resize(static_cast<size_t>(res));
      (*this)(std::move(buffer));
      return true;
    }
    void cancel(const Status &error) noexcept { (*this)(error); }
  };
  struct AC_EMPTY_BASES AC_NOVTABLE send_operation
      : base_type::epoll_send_base<send_handler> {
    using epoll_send_base::epoll_send_base;

    bool on_write(const details::raw_socket_t fd,
                  const uint32_t events) noexcept {
      (void)events;
      if (buffer.empty()) {
        (*this)(static_cast<size_t>(0));
        return true;
      }
      const auto remaining = buffer.size() - offset;
      const auto res =
          details::send(fd, buffer.data() + offset, remaining, 0, nullptr, 0);
      if (res < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
          return false;
        (*this)(details::make_send_error());
        return true;
      }
      offset += static_cast<size_t>(res);
      if (offset < buffer.size())
        return false;
      (*this)(static_cast<size_t>(buffer.size()));
      return true;
    }
    void cancel(const Status &error) noexcept { (*this)(error); }
  };
#endif
};
template <> class socket<udp> : public details::socket_base<udp> {

public:
  using base_type = socket_base<udp>;
  using base_type::socket_base;
  friend base_type;
  socket(socket &&socket) noexcept
      : base_type::socket_base(std::move(static_cast<base_type &&>(socket))) {}
  socket &operator=(socket &&socket) noexcept {
    base_type::operator=(std::move(static_cast<base_type &&>(socket)));
    return *this;
  }
  ~socket() = default;

public:
  Status send_bytes(bytes_type &&bytes) {
    if (!remote_endpoint_)
      return UnavailableError("Remote endpoint cache not available.");

    if (details::send(handle_,
                      std::move(bytes),
                      0,
                      remote_endpoint_->data(),
                      remote_endpoint_->size()) != -1) [[likely]]
      return {};
    else [[unlikely]]
      return details::make_send_error();
  }
  Status send_bytes(bytes_type &&bytes, const endpoint_type &endpoint) {
    if (details::send(
            handle_, std::move(bytes), 0, endpoint.data(), endpoint.size()) !=
        -1) [[likely]] {
      remote_endpoint_.emplace(endpoint);
      return {};
    } else [[unlikely]]
      return details::make_send_error();
  }

public:
  using recv_handler =
      std::move_only_function<void(StatusOr<bytes_type>, endpoint_type)>;
  using send_handler = std::move_only_function<void(StatusOr<size_t>)>;

  Status async_recv_from(const size_t max_size, recv_handler handler) {
#ifdef _WIN32

    auto *op = new recv_operation{
        this, socket::handle_recv, std::move(handler), max_size};

    DWORD bytes = 0;
    return base_type::start_overlapped(
        op,
        [&] {
          return ::WSARecvFrom(
              handle_,
              &op->wsa_buf,
              1,
              &bytes,
              &op->flags,
              // this is de jure UB but de facto fine in C++,
              // network code has been doing this for decades
              reinterpret_cast<details::sockaddr_t *>(&op->storage),
              &op->storage_len,
              &op->overlapped,
              nullptr);
        },
        details::make_recv_error);
#elif defined(__linux__)
    if (!context_ || !linux_state_ || !is_valid())
      return UnavailableError("io_context is not available.");
    if (auto status = linux_state_->ensure_nonblocking(); !status)
      return status;
    linux_state_->template enqueue_read<recv_operation>(
        this, std::move(handler), max_size);
    return {};
#else
    (void)max_size;
    (void)handler;
    return UnavailableError("async_recv_from is not supported.");
#endif
  }

  Status async_send_to(bytes_type bytes,
                       const endpoint_type &endpoint,
                       send_handler handler) {
#ifdef _WIN32

    auto *op = new send_operation{
        this,
        socket::handle_send,
        std::move(handler),
        std::move(bytes),
        endpoint,
    };
    // race condition maybe vvv
    // remote_endpoint_.emplace(endpoint);
    DWORD bytes_sent = 0;
    return base_type::start_overlapped(
        op,
        [&] {
          return ::WSASendTo(handle_,
                             &op->wsa_buf,
                             1,
                             &bytes_sent,
                             0,
                             op->endpoint.data(),
                             op->endpoint.size(),
                             &op->overlapped,
                             nullptr);
        },
        details::make_recv_error);
#elif defined(__linux__)
    if (!context_ || !linux_state_ || !is_valid())
      return UnavailableError("io_context is not available.");
    if (auto status = linux_state_->ensure_nonblocking(); !status)
      return status;

    linux_state_->template enqueue_write<send_operation>(
        this, std::move(handler), std::move(bytes), endpoint);
    return {};
#else
    (void)bytes;
    (void)endpoint;
    (void)handler;
    return UnavailableError("async_send_to is not supported.");
#endif
  }

  Status async_send(bytes_type bytes, send_handler handler) {
    if (!remote_endpoint_)
      return UnavailableError("Remote endpoint cache not available.");
    return async_send_to(
        std::move(bytes), *remote_endpoint_, std::move(handler));
  }

private:
#ifdef _WIN32
  struct AC_EMPTY_BASES AC_NOVTABLE recv_operation
      : base_type::template iocp_recv_base<recv_handler> {
    inline recv_operation(socket_type *const self,
                          complete_fn fn,
                          recv_handler &&handler,
                          const size_t max_size) noexcept
        : iocp_recv_base(self, fn, std::move(handler), max_size), storage({}),
          storage_len(sizeof(details::socket_storage_type)) {}
    details::sockaddr_storage_t storage;
    details::socket_len_type storage_len;
  };
  struct AC_EMPTY_BASES AC_NOVTABLE send_operation
      : base_type::template iocp_send_base<send_handler> {
    inline send_operation(socket_type *const self,
                          complete_fn fn,
                          send_handler &&handler,
                          bytes_type &&buffer,
                          const endpoint_type &endpoint) noexcept
        : iocp_send_base(self, fn, std::move(handler), std::move(buffer)),
          endpoint(endpoint) {}
    endpoint_type endpoint;
  };

  static void handle_recv(details::iocp_operation *base,
                          const DWORD bytes,
                          const DWORD error) noexcept {
    auto *op = static_cast<recv_operation *>(base);
    AC_DEFER { delete op; };
    if (error != ERROR_SUCCESS) {
      ::WSASetLastError(static_cast<int>(error));
      (*op)(details::make_recv_error(), endpoint_type::unspecified());
      return;
    }
    if (op->storage_len != sizeof(details::sockaddr_in4_t) &&
        op->storage_len != sizeof(details::sockaddr_in6_t)) {
      (*op)(UnavailableError("Unknown address family of the accepted socket."),
            endpoint_type::unspecified());
      return;
    }
    op->buffer.resize(bytes);
    endpoint_type sender(std::move(op->storage), op->storage_len);
    AC_RUNTIME_ASSERT(op->self);
    op->self->remote_endpoint_.emplace(sender);
    (*op)(std::move(op->buffer), std::move(sender));
  }

  static void handle_send(details::iocp_operation *base,
                          const DWORD bytes,
                          const DWORD error) noexcept {
    base_type::finish_send(static_cast<send_operation *>(base), bytes, error);
  }
#elif defined(__linux__)
  struct AC_EMPTY_BASES AC_NOVTABLE recv_operation
      : base_type::epoll_recv_base<recv_handler> {
    using epoll_recv_base::epoll_recv_base;
    inline recv_operation(socket_type *const self,
                          base_type::handler_type &&handler,
                          const size_t max_size) noexcept
        : epoll_recv_base(self, std::move(handler), max_size), storage({}),
          storage_len(sizeof(details::socket_storage_type)) {}

    details::sockaddr_storage_t storage;
    details::socket_len_type storage_len;

    bool on_read(const details::raw_socket_t fd,
                 const uint32_t events) noexcept {
      (void)events;
      storage_len = sizeof(details::socket_storage_type);
      const auto res =
          details::recv(fd,
                        buffer.data(),
                        buffer.size(),
                        0,
                        reinterpret_cast<details::sockaddr_t *>(&storage),
                        &storage_len);
      if (res < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
          return false;
        (*this)(details::make_recv_error(), endpoint_type::unspecified());
        return true;
      }
      if (storage_len != sizeof(details::sockaddr_in4_t) &&
          storage_len != sizeof(details::sockaddr_in6_t)) {
        (*this)(
            UnavailableError("Unknown address family of the accepted socket."),
            endpoint_type::unspecified());
        return true;
      }
      buffer.resize(static_cast<size_t>(res));
      endpoint_type sender(std::move(storage), storage_len);
      AC_RUNTIME_ASSERT(self)
      self->remote_endpoint_.emplace(sender);
      (*this)(std::move(buffer), std::move(sender));
      return true;
    }
    void cancel(const Status &error) noexcept {
      (*this)(error, endpoint_type::unspecified());
    }
  };
  struct AC_EMPTY_BASES AC_NOVTABLE send_operation
      : base_type::epoll_send_base<send_handler> {
    endpoint_type endpoint;

    inline send_operation(socket_type *const self,
                          send_handler &&handler,
                          bytes_type &&buffer,
                          const endpoint_type &endpoint) noexcept
        : epoll_send_base(self, std::move(handler), std::move(buffer)),
          endpoint(endpoint) {}

    bool on_write(const details::raw_socket_t fd,
                  const uint32_t events) noexcept {
      (void)events;
      if (buffer.empty()) {
        (*this)(0);
        return true;
      }
      const auto res = details::send(fd,
                                     buffer.data() + offset,
                                     buffer.size() - offset,
                                     0,
                                     endpoint.data(),
                                     endpoint.size());
      if (res < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
          return false;
        (*this)(details::make_send_error());
        return true;
      }
      offset += static_cast<size_t>(res);
      if (offset < buffer.size())
        return false;
      (*this)(static_cast<size_t>(buffer.size()));
      return true;
    }
    void cancel(const Status &error) noexcept { (*this)(error); }
  };
#endif

protected:
  StatusOr<bytes_type> do_recv(const size_t max_size) {
    details::socket_len_type len = sizeof(details::socket_storage_type);
    details::socket_storage_type storage;

    bytes_type buffer;
    bool success = true;
    buffer.resize_and_overwrite(
        max_size,
        [&, this](char *buf, const size_t buf_size) noexcept -> size_t {
          auto res =
              details::recv(handle_,
                            buf,
                            buf_size,
                            0,
                            reinterpret_cast<details::sockaddr_t *>(&storage),
                            &len);
          if (res < 0) {
            success = false;
            return 0;
          } else if (len != sizeof(details::sockaddr_in4_t) &&
                     len != sizeof(details::sockaddr_in6_t)) {
            success = false;
            return 0;
          }

          remote_endpoint_.emplace(reinterpret_cast<endpoint_type &&>(storage));
          return res;
        });
    if (success)
      return {std::move(buffer)};
    else
      return details::make_recv_error();
  }
};
} // namespace auxilia::net
