#pragma once

#include <array>
#include <cstddef>
#include <cstring>
#include <exception>
#include <memory>
#include <mutex>
#include <new>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <functional>

#include "auxilia/base/macros.hpp"
#include "auxilia/base/config.hpp"
#include "auxilia/base/format.hpp"
#include "auxilia/status/Status.hpp"
#include "auxilia/status/StatusOr.hpp"
#include "auxilia/meta/container_traits.hpp"

#include "os.hpp"
#include "ip.hpp"
#include "protocol.hpp"
#include "io_context.hpp"
#include "endpoint.hpp"
#include "awaitable.hpp"

namespace auxilia::net::details {

template <typename Protocol> class socket_base : Printable {

public:
  using protocol_type = Protocol;
  using native_handle_type = details::raw_socket_t;
  using endpoint_type = protocol_type::endpoint;
  using address_type = ip::address;
  using bytes_view_type = std::string_view;
  using bytes_type = std::string; // a vec of bytes
  using socket_type = protocol_type::socket;
  static_assert(InternetProtocol<protocol_type>);
  static_assert(container_traits<bytes_type>::is_reservable);
  static_assert(container_traits<bytes_type>::is_contiguous);

public:
  inline explicit(false)
      socket_base(io_context *context = nullptr,
                  native_handle_type handle = invalid_socket) noexcept;
  inline explicit(false)
      socket_base(io_context &context,
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
  inline ~socket_base() noexcept { close().log_err(); }

public:
  inline static StatusOr<socket_type> v4(io_context *context = nullptr);
  inline static StatusOr<socket_type> v6(io_context *context = nullptr);

public:
  AC_NODISCARD inline Status close() const;
  AC_NODISCARD constexpr auto native_handle() const noexcept { return handle_; }
  AC_NODISCARD constexpr auto context() const noexcept { return context_; }
  AC_NODISCARD constexpr auto is_valid() const noexcept {
    return handle_ != details::invalid_socket;
  }
  template <ip::family family = ip::family::v4>
  AC_NODISCARD inline auto bind(const endpoint_type::port_type port = 0) {
    return bind(endpoint_type::template unspecified<family>(port));
  }
  AC_NODISCARD Status bind(const endpoint_type &endpoint) {
    return details::bind(handle_, endpoint.data(), endpoint.size());
  }
  AC_NODISCARD auto to_string(const FormatPolicy) const {
    return Format(handle_);
  }

public:
  Status connect(const endpoint_type &endpoint);

public:
  struct {
    static decltype(auto) operator()(auto &&self) { return self.recv(); }
    static decltype(auto) operator()(auto &&self, const size_t max_size) {
      return self.recv(max_size);
    }
  }

  /// monadic shorthand
  static constexpr inline recv_;

  struct {
    static decltype(auto) operator()(auto &&self, bytes_type &&bytes) {
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
                                                details::iocp::operation {
    static_assert(
        is_specialization_v<Handler, std::move_only_function>,
        "only support move only function right now, "
        "since I really dont know how to handle function ref or copyable function "
        "due to it's ownership and lifetime problem.");
    using handler_type = Handler;
    using buffer_type = ::WSABUF;

    bytes_type buffer;
    buffer_type wsa_buf;
    handler_type handler;
    socket_type *self;

  protected:
    using iocp::operation::operation;
  };
  template <typename Handler>
  struct AC_EMPTY_BASES AC_NOVTABLE iocp_send_base : iocp_base<Handler> {
    using base_type = iocp_base<Handler>;
    inline iocp_send_base(socket_type *const self,
                          base_type::complete_fn complete_fn,
                          Handler &&handler,
                          bytes_type &&buffer) noexcept
        : base_type(complete_fn) {
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
                          base_type::complete_fn complete_fn,
                          Handler &&handler,
                          const size_t max_size) noexcept
        : base_type(complete_fn) {
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
    if (start_fn() == details::socket_error &&
        ::WSAGetLastError() != WSA_IO_PENDING) {
      AC_DEFER { delete op; };
      if constexpr (std::invocable<ErrorFn>)
        return error_fn();
      else if constexpr (std::invocable<ErrorFn, Op *>)
        if constexpr (std::is_same_v<std::invoke_result_t<ErrorFn, Op *>,
                                     Status>)
          return error_fn(op);
        else
          static_assert(false, "invalid error function");
      else
        static_assert(false, "invalid error function");
    }
    return {};
  }
  template <typename Op>
  static void
  finish_send(Op *op, const DWORD bytes, const DWORD error) noexcept {

    AC_DEFER { delete op; };

    if (error != ERROR_SUCCESS) {
      // ::WSASetLastError(static_cast<int>(error));
      (*op)(details::make_send_error());
    } else {
      (*op)(static_cast<size_t>(bytes));
    }
  }
#elif defined(__linux__)
  template <typename Handler> struct epoll_base : CallableMixin {
    using handler_type = Handler;
    bytes_type buffer;
    handler_type handler;
    socket_type *self;

  protected:
    inline ~epoll_base() noexcept = default;
  };
  template <typename Handler> struct epoll_send_base : epoll_base<Handler> {
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
  template <typename Handler> struct epoll_recv_base : epoll_base<Handler> {
    using base_type = epoll_base<Handler>;
    inline epoll_recv_base(socket_type *const self,
                           base_type::handler_type &&handler,
                           const size_t max_size) noexcept {
      this->self = self;
      this->handler = std::move(handler);
      this->buffer.resize(max_size);
    }
  };

  struct epoll_operation {
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
      AC_DEBUG_BLOCK {
        self = nullptr;
        on_op = nullptr;
        on_cancel = nullptr;
        on_destroy = nullptr;
      };
    }
    void cancel(const Status &error) noexcept {
      if (on_cancel && self)
        on_cancel(self, error);
    }

  protected:
    void *self;
    on_fn on_op;
    cancel_fn on_cancel;
    destroy_fn on_destroy;

  protected:
    inline ~epoll_operation() noexcept { reset(); }
    bool call_on_op(const details::raw_socket_t fd,
                    const uint32_t events) noexcept {
      return on_op && self ? on_op(self, fd, events) : true;
    }
  };

  struct read_operation : epoll_operation {
    using base_type = epoll_operation;
    inline read_operation() noexcept : base_type() {}
    template <typename Op, typename... Args>
    inline read_operation(std::in_place_type_t<Op>, Args &&...args) noexcept {
      this->self = new (std::nothrow) Op(std::forward<Args>(args)...);
      this->on_op = [](auto self, auto fd, auto events) noexcept {
        return static_cast<Op *>(self)->on_read(fd, events);
      };
      this->on_cancel = [](auto self, auto error) noexcept {
        static_cast<Op *>(self)->cancel(error);
      };
      this->on_destroy = [](auto self) noexcept {
        delete static_cast<Op *>(self);
      };
    }
    inline auto read(const details::raw_socket_t fd,
                     const uint32_t events) noexcept {
      return this->call_on_op(fd, events);
    }
  };
  struct write_operation : epoll_operation {
    using base_type = epoll_operation;
    inline write_operation() noexcept : base_type() {}
    template <typename Op, typename... Args>
    inline write_operation(std::in_place_type_t<Op>, Args &&...args) noexcept {
      this->self = new (std::nothrow) Op(std::forward<Args>(args)...);
      this->on_op = [](auto self, auto fd, auto events) noexcept {
        return static_cast<Op *>(self)->on_write(fd, events);
      };
      this->on_cancel = [](auto self, auto error) noexcept {
        static_cast<Op *>(self)->cancel(error);
      };
      this->on_destroy = [](auto self) noexcept {
        delete static_cast<Op *>(self);
      };
    }

    inline auto write(const details::raw_socket_t fd,
                      const uint32_t events) noexcept {
      return this->call_on_op(fd, events);
    }
  };
  struct epoll_socket_state {
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
      auto op =
          read_operation(std::in_place_type<Op>, std::forward<Args>(args)...);
      {
        std::scoped_lock lock(mutex);
        if (closing) {
          op.cancel(Cancelled("socket closed"));
          return;
        }
        read_ops.emplace_back(std::move(op));
      }
      on_event(EPOLLIN);
    }
    template <typename Op, typename... Args>
    void enqueue_write(Args &&...args) noexcept {
      auto op =
          write_operation(std::in_place_type<Op>, std::forward<Args>(args)...);
      {
        std::scoped_lock lock(mutex);
        if (closing) {
          op.cancel(Cancelled("socket closed"));
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
      if (self)
        static_cast<epoll_socket_state *>(self)->on_event(events);
    }
    void on_event(const uint32_t events) noexcept {
      if (closing || busy.test_and_set(std::memory_order::acquire))
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
#else
#  error unsupported
#endif

protected:
  io_context *context_;
  native_handle_type handle_;

#ifdef __linux__
  /// FIXME: not good obviously.
  mutable std::unique_ptr<epoll_socket_state> epoll_state_;
#endif
};
template <typename Protocol>
inline socket_base<Protocol>::socket_base(
    io_context *context, const native_handle_type handle) noexcept
    : context_(context), handle_(handle) {
  if (handle_ == invalid_socket)
    return;
  AC_RUNTIME_ASSERT(context_, "no context while handle is valid")
#ifdef __linux__
  constexpr int opt = 1;
  ::setsockopt(handle_, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
  epoll_state_ = std::make_unique<epoll_socket_state>(handle_);
  context_
      ->associate(handle_, reinterpret_cast<size_t>(&epoll_state_->dispatcher))
      .log_err();
#elif defined(_WIN32)
  constexpr char opt = 1;
  ::setsockopt(handle_, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
  context_->associate(handle_).log_err();
#else
#  error unsupported
#endif
}
template <typename Protocol>
inline socket_base<Protocol>::socket_base(socket_base &&that) noexcept
    : context_(std::exchange(that.context_, nullptr)),
      handle_(std::exchange(that.handle_, invalid_socket))
#ifdef __linux__
      ,
      epoll_state_(std::exchange(that.epoll_state_, nullptr))
#endif
{
}
template <typename Protocol>
inline socket_base<Protocol> &
socket_base<Protocol>::operator=(socket_base &&that) noexcept {
  if (this != &that) {
    close().log_err();
    context_ = std::exchange(that.context_, nullptr);
    handle_ = std::exchange(that.handle_, invalid_socket);
#ifdef __linux__
    epoll_state_ = std::exchange(that.epoll_state_, nullptr);
#endif
  }
  return *this;
}
template <typename Protocol>
inline auto socket_base<Protocol>::v4(io_context *context)
    -> StatusOr<socket_type> {
  if (auto self = socket_type(context, ip::family::v4); self.is_valid())
      [[likely]]
    return {std::move(self)};
  else [[unlikely]]
    return {details::make_ctor_error()};
}
template <typename Protocol>
inline auto socket_base<Protocol>::v6(io_context *context)
    -> StatusOr<socket_type> {
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
    if (epoll_state_)
      epoll_state_->cancel_all(Cancelled("socket closed"));
    if (context_ && context_->native_handle() != details::epoll::invalid)
      details::epoll::del(context_->native_handle(), handle_).log_err();
#endif
    return details::closesocket(handle_);
  } else
    return {};
}
template <typename Protocol>
inline Status socket_base<Protocol>::connect(const endpoint_type &endpoint) {
  if (details::connect(handle_, endpoint.data(), endpoint.size()) != -1)
      [[likely]]
    return {};
  else [[unlikely]]
    return details::make_connect_error();
}
} // namespace auxilia::net::details
namespace auxilia::net {
template <typename FakeT> class socket {
  consteval socket() noexcept { always_false<socket<FakeT>>("not supported"); }
};
template <> class socket<tcp> : public details::socket_base<tcp> {
  /// FIXME: race condition in async operations
  std::optional<endpoint_type> remote_endpoint_;

public:
  using base_type = socket_base<tcp>;
  using base_type::socket_base;
  friend base_type;
  inline explicit(false) socket(io_context *context,
                                native_handle_type handle,
                                endpoint_type endpoint) noexcept
      : base_type::socket_base(context, handle), remote_endpoint_(endpoint) {}
  socket(socket &&socket) noexcept : base_type::socket_base(std::move(socket)) {
    remote_endpoint_ = std::move(socket.remote_endpoint_);
  }
  socket &operator=(socket &&socket) noexcept {
    base_type::operator=(std::move(socket));
    remote_endpoint_ = std::move(socket.remote_endpoint_);
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
      return FailedPreconditionError(
          "Remote endpoint is not set. make sure you called accept() and it succeeded.");

    bytes_type buffer;
    auto success = true;
    buffer.resize_and_overwrite(
        max_size,
        [this, &success](char *buf, const size_t buf_size) noexcept -> size_t {
          if (auto res = details::recv(handle_, buf, buf_size); res < 0) {
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
        acc == details::invalid_socket)
      return details::make_accept_error();
    else
      return make_accepted_socket(context_, acc, std::move(storage), len);
  }

  struct {
    static decltype(auto) operator()(auto &&self) { return self.accept(); }
  } static constexpr inline accept_;

  Status send_bytes(bytes_type &&bytes) {
    if (details::send(handle_, std::move(bytes), 0, nullptr, 0) != -1)
        [[likely]]
      return {};
    else [[unlikely]]
      return details::make_send_error();
  }

public:
  using accept_handler = std::move_only_function<void(StatusOr<socket>)>;
  using recv_handler = std::move_only_function<void(StatusOr<bytes_type>)>;
  using send_handler = std::move_only_function<void(StatusOr<size_t>)>;
  using accept_awaitable = net::awaitable<socket>;
  using recv_awaitable = net::awaitable<bytes_type>;
  using send_awaitable = net::awaitable<size_t>;

private:
  static StatusOr<socket>
  make_accepted_socket(io_context *const context,
                       const native_handle_type handle,
                       details::socket_storage_type &&storage,
                       const details::socket_len_type len) {
    if (auto endpoint = endpoint_type::from_native(std::move(storage), len))
        [[likely]]
      return socket(context, handle, *std::move(endpoint));

    details::closesocket(handle).log_err();
    return UnavailableError(
        "Unknown or unsupported address family of the accepted socket.");
  }

#ifdef _WIN32
  struct AC_EMPTY_BASES AC_NOVTABLE accept_operation
      : base_type::template iocp_base<accept_handler> {
    using iocp_base::iocp_base;
    static constexpr DWORD address_bytes =
        sizeof(details::socket_storage_type) + 16;
    native_handle_type accepted = details::invalid_socket;
    std::array<char, address_bytes * 2> address_buffer{};
    LPFN_GETACCEPTEXSOCKADDRS get_sockaddrs = nullptr;

    inline accept_operation(
        socket_type *const self,
        const complete_fn complete_fn,
        accept_handler &&handler,
        const native_handle_type accepted,
        const LPFN_GETACCEPTEXSOCKADDRS get_sockaddrs) noexcept
        : iocp_base(complete_fn), accepted(accepted),
          get_sockaddrs(get_sockaddrs) {
      this->handler = std::move(handler);
      this->self = self;
    }
    inline ~accept_operation() noexcept {
      if (accepted != details::invalid_socket)
        details::closesocket(accepted).log_err();
    }
  };

  struct AC_EMPTY_BASES AC_NOVTABLE recv_operation
      : base_type::template iocp_recv_base<recv_handler> {
    using iocp_recv_base::iocp_recv_base;
  };
  struct AC_EMPTY_BASES AC_NOVTABLE send_operation
      : base_type::template iocp_send_base<send_handler> {
    using iocp_send_base::iocp_send_base;
  };

  static void handle_recv(details::iocp::operation *base,
                          const DWORD bytes,
                          const DWORD error) noexcept {
    auto *op = static_cast<recv_operation *>(base);
    AC_DEFER { delete op; };
    if (error != ERROR_SUCCESS) {
      // ::WSASetLastError(static_cast<int>(error));
      (*op)(details::make_recv_error());
    } else {
      op->buffer.resize(bytes);
      (*op)(std::move(op->buffer));
    }
  }

  static void handle_send(details::iocp::operation *base,
                          const DWORD bytes,
                          const DWORD error) noexcept {
    base_type::finish_send(static_cast<send_operation *>(base), bytes, error);
  }

  static void handle_accept(details::iocp::operation *base,
                            [[maybe_unused]] const DWORD bytes,
                            const DWORD error) noexcept {
    auto *op = static_cast<accept_operation *>(base);
    AC_DEFER { delete op; };
    if (error != ERROR_SUCCESS) {
      (*op)(details::make_accept_error());
      return;
    }

    auto accepted = std::exchange(op->accepted, details::invalid_socket);
    if (::setsockopt(accepted,
                     SOL_SOCKET,
                     SO_UPDATE_ACCEPT_CONTEXT,
                     reinterpret_cast<const char *>(&op->self->handle_),
                     sizeof(op->self->handle_)) == details::socket_error) {
      details::closesocket(accepted).log_err();
      (*op)(details::make_accept_error());
      return;
    }

    details::sockaddr_t *local = nullptr;
    details::sockaddr_t *remote = nullptr;
    int local_len = 0;
    int remote_len = 0;
    op->get_sockaddrs(op->address_buffer.data(),
                      0,
                      accept_operation::address_bytes,
                      accept_operation::address_bytes,
                      &local,
                      &local_len,
                      &remote,
                      &remote_len);

    if (!remote || (remote_len != sizeof(details::sockaddr_in4_t) &&
                    remote_len != sizeof(details::sockaddr_in6_t)))
        [[unlikely]] {
      details::closesocket(accepted).log_err();
      (*op)(UnavailableError(
          "Unknown or unsupported address family of the accepted socket."));
      return;
    }

    details::socket_storage_type storage{};
    std::memcpy(&storage, remote, static_cast<size_t>(remote_len));
    (*op)(make_accepted_socket(
        op->self->context_,
        accepted,
        std::move(storage),
        static_cast<details::socket_len_type>(remote_len)));
  }
#elif defined(__linux__)
  struct accept_operation : base_type::template epoll_base<accept_handler> {
    inline accept_operation(socket_type *const self,
                            accept_handler &&handler) noexcept {
      this->self = self;
      this->handler = std::move(handler);
    }

    bool on_read(const details::raw_socket_t fd,
                 [[maybe_unused]] const uint32_t events) noexcept {
      details::socket_len_type len = sizeof(details::socket_storage_type);
      details::socket_storage_type storage{};
      auto accepted = details::accept(
          fd, reinterpret_cast<details::sockaddr_t *>(&storage), &len);
      if (accepted < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
          return false;
        (*this)(details::make_accept_error());
        return true;
      }

      (*this)(make_accepted_socket(
          this->self->context_, accepted, std::move(storage), len));
      return true;
    }
    void cancel(const Status &error) noexcept { (*this)(error); }
  };

  struct recv_operation : base_type::template epoll_recv_base<recv_handler> {
    using epoll_recv_base::epoll_recv_base;

    bool on_read(const details::raw_socket_t fd,
                 [[maybe_unused]] const uint32_t events) noexcept {
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
  struct send_operation : base_type::template epoll_send_base<send_handler> {
    using epoll_send_base::epoll_send_base;

    bool on_write(const details::raw_socket_t fd,
                  [[maybe_unused]] const uint32_t events) noexcept {
      if (buffer.empty()) {
        (*this)(static_cast<size_t>(0));
        return true;
      }
      const auto res = details::send(
          fd, buffer.data() + offset, buffer.size() - offset, 0, nullptr, 0);
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
#else
#  error unsupported
#endif
public:
  using accept_error_handler =
      std::move_only_function<Status(accept_operation *)>;
  using recv_error_handler = std::move_only_function<Status(recv_operation *)>;
  using send_error_handler = std::move_only_function<Status(send_operation *)>;

public:
  accept_awaitable async_accept(accept_error_handler &&error_fn) {
    return accept_awaitable([this, error_fn = std::move(error_fn)](
                                accept_handler handler) mutable -> Status {
      return async_accept(std::move(handler), std::move(error_fn));
    });
  }
  accept_awaitable async_accept() {
    return accept_awaitable([this](accept_handler handler) mutable -> Status {
      return async_accept(std::move(handler), details::make_accept_error);
    });
  }

  recv_awaitable async_recv(const size_t max_size,
                            recv_error_handler &&error_fn) {
    return recv_awaitable([this, max_size, error_fn = std::move(error_fn)](
                              recv_handler handler) mutable -> Status {
      return async_recv(max_size, std::move(handler), std::move(error_fn));
    });
  }
  recv_awaitable async_recv(const size_t max_size) {
    return recv_awaitable([this,
                           max_size](recv_handler handler) mutable -> Status {
      return async_recv(max_size, std::move(handler), details::make_recv_error);
    });
  }

  send_awaitable async_send(bytes_type bytes, send_error_handler &&error_fn) {
    return send_awaitable(
        [this, bytes = std::move(bytes), error_fn = std::move(error_fn)](
            send_handler handler) mutable -> Status {
          return async_send(
              std::move(bytes), std::move(handler), std::move(error_fn));
        });
  }
  send_awaitable async_send(bytes_type bytes) {
    return send_awaitable([this, bytes = std::move(bytes)](
                              send_handler handler) mutable -> Status {
      return async_send(
          std::move(bytes), std::move(handler), details::make_send_error);
    });
  }

  template <typename ErrorFn = decltype(details::make_accept_error)>
  Status async_accept(accept_handler handler,
                      ErrorFn &&error_fn = details::make_accept_error) {
#ifdef _WIN32
    if (!context_ || !is_valid())
      return FailedPreconditionError("io_context is not available.");

    details::socket_storage_type local{};
    details::socket_len_type local_len = sizeof(local);
    if (::getsockname(handle_,
                      reinterpret_cast<details::sockaddr_t *>(&local),
                      &local_len) == details::socket_error)
      return details::make_accept_error();

    const auto family = static_cast<ip::family>(local.ss_family);
    if (family != ip::family::v4 && family != ip::family::v6)
      return UnavailableError(
          "Unknown or unsupported address family of the listening socket.");

    GUID accept_guid = WSAID_ACCEPTEX;
    GUID sockaddrs_guid = WSAID_GETACCEPTEXSOCKADDRS;
    LPFN_ACCEPTEX accept_ex = nullptr;
    LPFN_GETACCEPTEXSOCKADDRS get_sockaddrs = nullptr;
    DWORD ioctl_bytes = 0;
    if (::WSAIoctl(handle_,
                   SIO_GET_EXTENSION_FUNCTION_POINTER,
                   &accept_guid,
                   sizeof(accept_guid),
                   &accept_ex,
                   sizeof(accept_ex),
                   &ioctl_bytes,
                   nullptr,
                   nullptr) == details::socket_error ||
        ::WSAIoctl(handle_,
                   SIO_GET_EXTENSION_FUNCTION_POINTER,
                   &sockaddrs_guid,
                   sizeof(sockaddrs_guid),
                   &get_sockaddrs,
                   sizeof(get_sockaddrs),
                   &ioctl_bytes,
                   nullptr,
                   nullptr) == details::socket_error)
      return details::make_accept_error();

    auto accepted = details::socket(family, tcp::socket_kind());
    if (accepted == details::invalid_socket)
      return details::make_ctor_error();

    auto *op = new (std::nothrow) accept_operation(this,
                                                   socket::handle_accept,
                                                   std::move(handler),
                                                   accepted,
                                                   get_sockaddrs);
    if (!op) {
      details::closesocket(accepted).log_err();
      return ResourceExhaustedError(
          "Insufficient memory to construct iocp accept operation");
    }

    DWORD bytes = 0;
    return base_type::start_overlapped(
        op,
        [&] {
          return accept_ex(handle_,
                           op->accepted,
                           op->address_buffer.data(),
                           0,
                           accept_operation::address_bytes,
                           accept_operation::address_bytes,
                           &bytes,
                           &op->overlapped)
                     ? 0
                     : details::socket_error;
        },
        std::forward<ErrorFn>(error_fn));
#elif defined(__linux__)
    if (!context_ || !epoll_state_ || !is_valid())
      return FailedPreconditionError("io_context is not available.");
    if (auto status = epoll_state_->ensure_nonblocking(); !status)
      return status;
    epoll_state_->template enqueue_read<accept_operation>(this,
                                                          std::move(handler));
    return {};
#else
#  error unsupported
#endif
  }

  template <typename ErrorFn = decltype(details::make_recv_error)>
  Status async_recv(const size_t max_size,
                    recv_handler handler,
                    ErrorFn &&error_fn = details::make_recv_error) {
#ifdef _WIN32
    // lifetime of `op` outlives this function, ends in `handle_recv`
    auto *op = new (std::nothrow)
        recv_operation(this, socket::handle_recv, std::move(handler), max_size);
    if (!op)
      return ResourceExhaustedError(
          "Insufficient memory to construct iocp recv operation");
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
        std::forward<ErrorFn>(error_fn));
#elif defined(__linux__)
    if (!context_ || !epoll_state_ || !is_valid())
      return FailedPreconditionError("io_context is not available.");
    if (auto status = epoll_state_->ensure_nonblocking(); !status)
      return status;
    epoll_state_->template enqueue_read<recv_operation>(
        this, std::move(handler), max_size);
    return {};
#else
#  error unsupported
#endif
  }
  template <typename ErrorFn = decltype(details::make_send_error)>
  Status async_send(bytes_type bytes,
                    send_handler handler,
                    ErrorFn &&error_fn = details::make_send_error) {
#ifdef _WIN32
    auto *op = new (std::nothrow) send_operation(
        this, socket::handle_send, std::move(handler), std::move(bytes));
    if (!op)
      return ResourceExhaustedError(
          "Insufficient memory to construct iocp send operation");
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
        std::forward<ErrorFn>(error_fn));
#elif defined(__linux__)
    if (!context_ || !epoll_state_ || !is_valid())
      return FailedPreconditionError("io_context is not available.");
    if (auto status = epoll_state_->ensure_nonblocking(); !status)
      return status;

    epoll_state_->template enqueue_write<send_operation>(
        this, std::move(handler), std::move(bytes));
    return {};
#else
#  error unsupported
#endif
  }

public:
  AC_NODISCARD inline auto remote_endpoint() const noexcept {
    return remote_endpoint_;
  }
};
template <typename FakeT> class acceptor {
  consteval acceptor() noexcept {
    always_false<acceptor<FakeT>>("not supported");
  }
};
template <> class acceptor<tcp> : private socket<tcp> {
public:
  using socket_type = socket<tcp>;
  using native_handle_type = socket_type::native_handle_type;
  using endpoint_type = socket_type::endpoint_type;
  using accept_handler = socket_type::accept_handler;
  using accept_error_handler = socket_type::accept_error_handler;
  using accept_awaitable = socket_type::accept_awaitable;

public:
  using socket_type::socket_type;
  inline explicit(false) acceptor(socket_type &&socket) noexcept
      : socket_type(std::move(socket)) {}
  acceptor(acceptor &&) noexcept = default;
  acceptor &operator=(acceptor &&) noexcept = default;
  ~acceptor() = default;

public:
  AC_NODISCARD inline static StatusOr<acceptor>
  v4(io_context *context = nullptr) {
    acceptor self(context, ip::family::v4);
    if (self.is_valid()) [[likely]]
      return {std::move(self)};
    else [[unlikely]]
      return {details::make_ctor_error()};
  }
  AC_NODISCARD inline static StatusOr<acceptor>
  v6(io_context *context = nullptr) {
    acceptor self(context, ip::family::v6);
    if (self.is_valid()) [[likely]]
      return {std::move(self)};
    else [[unlikely]]
      return {details::make_ctor_error()};
  }

  using socket_type::accept;
  using socket_type::accept_;
  using socket_type::async_accept;
  using socket_type::bind;
  using socket_type::close;
  using socket_type::context;
  using socket_type::is_valid;
  using socket_type::listen;
  using socket_type::listen_;
  using socket_type::native_handle;
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
  Status send_bytes(bytes_type &&bytes, const endpoint_type &endpoint) {
    if (details::send(
            handle_, std::move(bytes), 0, endpoint.data(), endpoint.size()) !=
        -1) [[likely]]
      return {};
    else [[unlikely]]
      return details::make_send_error();
  }

public:
  using recv_handler =
      std::move_only_function<void(StatusOr<bytes_type>, endpoint_type)>;
  using send_handler = std::move_only_function<void(StatusOr<size_t>)>;
  struct recv_from_result {
    bytes_type bytes;
    endpoint_type sender;
  };
  using recv_from_awaitable = net::awaitable<recv_from_result>;
  using send_awaitable = net::awaitable<size_t>;

private:
private:
#ifdef _WIN32
  struct AC_EMPTY_BASES AC_NOVTABLE recv_operation
      : base_type::template iocp_recv_base<recv_handler> {
    inline recv_operation(socket_type *const self,
                          const complete_fn complete_fn,
                          recv_handler &&handler,
                          const size_t max_size) noexcept
        : iocp_recv_base(self, complete_fn, std::move(handler), max_size),
          storage({}), storage_len(sizeof(details::socket_storage_type)) {}
    details::sockaddr_storage_t storage;
    details::socket_len_type storage_len;
  };
  struct AC_EMPTY_BASES AC_NOVTABLE send_operation
      : base_type::template iocp_send_base<send_handler> {
    inline send_operation(socket_type *const self,
                          const complete_fn complete_fn,
                          send_handler &&handler,
                          bytes_type &&buffer,
                          const endpoint_type &endpoint) noexcept
        : iocp_send_base(
              self, complete_fn, std::move(handler), std::move(buffer)),
          endpoint(endpoint) {}
    endpoint_type endpoint;
  };

  static void handle_recv(details::iocp::operation *base,
                          const DWORD bytes,
                          const DWORD error) noexcept {
    auto *op = static_cast<recv_operation *>(base);
    AC_DEFER { delete op; };
    if (error != ERROR_SUCCESS) {
      // ::WSASetLastError(static_cast<int>(error));
      (*op)(details::make_recv_error(), endpoint_type::unspecified());

    } else if (op->storage_len != sizeof(details::sockaddr_in4_t) &&
               op->storage_len != sizeof(details::sockaddr_in6_t))
        [[unlikely]] {
      (*op)(UnavailableError("Unknown address family of the accepted socket."),
            endpoint_type::unspecified());

    } else {
      op->buffer.resize(bytes);
      endpoint_type sender(std::move(op->storage), op->storage_len);
      AC_RUNTIME_ASSERT(op->self);
      (*op)(std::move(op->buffer), std::move(sender));
    }
  }

  static void handle_send(details::iocp::operation *base,
                          const DWORD bytes,
                          const DWORD error) noexcept {
    base_type::finish_send(static_cast<send_operation *>(base), bytes, error);
  }
#elif defined(__linux__)
  struct recv_operation : base_type::template epoll_recv_base<recv_handler> {
    using epoll_recv_base::epoll_recv_base;
    inline recv_operation(socket_type *const self,
                          base_type::handler_type &&handler,
                          const size_t max_size) noexcept
        : epoll_recv_base(self, std::move(handler), max_size), storage({}),
          storage_len(sizeof(details::socket_storage_type)) {}

    details::sockaddr_storage_t storage;
    details::socket_len_type storage_len;

    bool on_read(const details::raw_socket_t fd,
                 [[maybe_unused]] const uint32_t events) noexcept {
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
      (*this)(std::move(buffer), std::move(sender));
      return true;
    }
    void cancel(const Status &error) noexcept {
      (*this)(error, endpoint_type::unspecified());
    }
  };
  struct send_operation : base_type::template epoll_send_base<send_handler> {
    endpoint_type endpoint;

    inline send_operation(socket_type *const self,
                          send_handler &&handler,
                          bytes_type &&buffer,
                          const endpoint_type &endpoint) noexcept
        : epoll_send_base(self, std::move(handler), std::move(buffer)),
          endpoint(endpoint) {}

    bool on_write(const details::raw_socket_t fd,
                  [[maybe_unused]] const uint32_t events) noexcept {
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
public:
  using recv_error_handler = std::move_only_function<Status(recv_operation *)>;
  using send_error_handler = std::move_only_function<Status(send_operation *)>;

public:
  recv_from_awaitable async_recv_from(const size_t max_size,
                                      recv_error_handler &&error_fn) {
    return recv_from_awaitable([this, max_size, error_fn = std::move(error_fn)](
                                   typename recv_from_awaitable::handler_type
                                       handler) mutable -> Status {
      return async_recv_from(
          max_size,
          [handler = std::move(handler)](StatusOr<bytes_type> result,
                                         endpoint_type sender) mutable {
            if (!result)
              handler(std::move(result).as_status());
            else
              handler(recv_from_result{*std::move(result), std::move(sender)});
          },
          std::move(error_fn));
    });
  }

  recv_from_awaitable async_recv_from(const size_t max_size) {
    return recv_from_awaitable([this, max_size](
                                   typename recv_from_awaitable::handler_type
                                       handler) mutable -> Status {
      return async_recv_from(
          max_size,
          [handler = std::move(handler)](StatusOr<bytes_type> result,
                                         endpoint_type sender) mutable {
            if (!result)
              handler(std::move(result).as_status());
            else
              handler(recv_from_result{*std::move(result), std::move(sender)});
          },
          details::make_recv_error);
    });
  }

  send_awaitable async_send_to(bytes_type bytes,
                               const endpoint_type &endpoint,
                               send_error_handler &&error_fn) {
    return send_awaitable([this,
                           bytes = std::move(bytes),
                           endpoint,
                           error_fn = std::move(error_fn)](
                              send_handler handler) mutable -> Status {
      return async_send_to(
          std::move(bytes), endpoint, std::move(handler), std::move(error_fn));
    });
  }
  send_awaitable async_send_to(bytes_type bytes,
                               const endpoint_type &endpoint) {
    return send_awaitable([this, bytes = std::move(bytes), endpoint](
                              send_handler handler) mutable -> Status {
      return async_send_to(std::move(bytes),
                           endpoint,
                           std::move(handler),
                           details::make_send_error);
    });
  }

  template <typename ErrorFn = decltype(details::make_recv_error)>
  Status async_recv_from(const size_t max_size,
                         recv_handler handler,
                         ErrorFn &&error_fn = details::make_recv_error) {
#ifdef _WIN32
    auto *op = new (std::nothrow)
        recv_operation(this, socket::handle_recv, std::move(handler), max_size);
    if (!op)
      return ResourceExhaustedError(
          "Insufficient memory to construct iocp recv operation");

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
        std::forward<ErrorFn>(error_fn));
#elif defined(__linux__)
    if (!context_ || !epoll_state_ || !is_valid())
      return FailedPreconditionError("io_context is not available.");
    if (auto status = epoll_state_->ensure_nonblocking(); !status)
      return status;
    epoll_state_->template enqueue_read<recv_operation>(
        this, std::move(handler), max_size);
    return {};
#else
#  error unsupported
#endif
  }
  template <typename ErrorFn = decltype(details::make_send_error)>
  Status async_send_to(bytes_type bytes,
                       const endpoint_type &endpoint,
                       send_handler handler,
                       ErrorFn &&error_fn = details::make_send_error) {
#ifdef _WIN32

    auto *op = new (std::nothrow) send_operation(this,
                                                 socket::handle_send,
                                                 std::move(handler),
                                                 std::move(bytes),
                                                 endpoint);
    if (!op)
      return ResourceExhaustedError(
          "Insufficient memory to construct iocp send operation");

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
        std::forward<ErrorFn>(error_fn));
#elif defined(__linux__)
    if (!context_ || !epoll_state_ || !is_valid())
      return FailedPreconditionError("io_context is not available.");
    if (auto status = epoll_state_->ensure_nonblocking(); !status)
      return status;

    epoll_state_->template enqueue_write<send_operation>(
        this, std::move(handler), std::move(bytes), endpoint);
    return {};
#else
#  error unsupported
#endif
  }

protected:
  StatusOr<bytes_type> do_recv(const size_t max_size) {
    details::socket_len_type len = sizeof(details::socket_storage_type);
    details::socket_storage_type storage;

    bytes_type buffer;
    auto success = true;
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
          if ((res < 0) || (len != sizeof(details::sockaddr_in4_t) &&
                            len != sizeof(details::sockaddr_in6_t))) {
            success = false;
            return 0;
          }
          return res;
        });
    if (success)
      return {std::move(buffer)};
    else
      return details::make_recv_error();
  }
};
} // namespace auxilia::net
