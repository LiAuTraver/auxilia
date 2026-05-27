#pragma once

#include <cerrno>
#include <cstddef>
#include <cstring>
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
#include "tcp.hpp"
#include "endpoint.hpp"
namespace auxilia::net {
class io_context;
}
namespace auxilia::net::details {

template <typename Protocol> class socket_base : Printable {

public:
  using native_handle_type = details::raw_socket_t;
  using protocol_type = Protocol;
  using endpoint_type = protocol_type::endpoint_type;
  using address_type = ip::address;
  using bytes_view_type = std::string_view;
  using bytes_type = std::string;
  using socket_type = protocol_type::socket_t;
  static_assert(InternetProtocol<protocol_type>);
  static_assert(container_traits<bytes_type>::is_reservable);
  static_assert(container_traits<bytes_type>::is_contiguous);

public:
  constexpr socket_base(
      io_context *context = nullptr,
      const native_handle_type handle = invalid_socket) noexcept
      : context_(context), handle_(handle), remote_endpoint_(std::nullopt) {}
  constexpr socket_base(
      io_context &context,
      const native_handle_type handle = invalid_socket) noexcept
      : context_(&context), handle_(handle), remote_endpoint_(std::nullopt) {}
  inline socket_base(io_context *context, const ip::family family)
      : context_(context),
        handle_(details::socket(family, protocol_type::socket_type())),
        remote_endpoint_(std::nullopt) {}
  inline socket_base(io_context &context, const ip::family family)
      : context_(&context),
        handle_(details::socket(family, protocol_type::socket_type())),
        remote_endpoint_(std::nullopt) {}
  socket_base(const socket_base &) = delete;
  socket_base &operator=(const socket_base &) = delete;
  socket_base(socket_base &&that) noexcept
      : context_(std::exchange(that.context_, nullptr)),
        handle_(std::exchange(that.handle_, invalid_socket)),
        remote_endpoint_(std::move(that.remote_endpoint_)) {}

  socket_base &operator=(socket_base &&that) noexcept {
    if (this != &that) {
      close();
      context_ = std::exchange(that.context_, nullptr);
      handle_ = std::exchange(that.handle_, invalid_socket);
      remote_endpoint_ = std::move(that.remote_endpoint_);
    }
    return *this;
  }

  AC_NODISCARD_REASON("the `recv` method returns the received bytes. ")
  inline decltype(auto) recv(const size_t max_size = 0x0400) {
    return static_cast<socket_type *>(this)->do_recv(max_size);
  }

protected:
  inline ~socket_base() noexcept { close(); }
  socket_base(io_context *context,
              const native_handle_type handle,
              endpoint_type &&remote_endpoint) noexcept
      : context_(context), handle_(handle),
        remote_endpoint_(std::move(remote_endpoint)) {}

public:
  inline static StatusOr<socket_type> v4(io_context *context = nullptr) {
    if (auto self = socket_type(context, ip::family::v4); self.is_valid())
        [[likely]]
      return {std::move(self)};
    else [[unlikely]]
      return {details::make_ctor_error()};
  }
  inline static StatusOr<socket_type> v6(io_context *context = nullptr) {
    if (auto self = socket_type(context, ip::family::v6); self.is_valid())
        [[likely]]
      return {std::move(self)};
    else [[unlikely]]
      return {details::make_ctor_error()};
  }

public:
  Status close() const {
    if (is_valid()) [[likely]]
      if (details::closesocket(handle_) != -1) [[likely]]
        return {};
      else [[unlikely]]
        return details::make_close_error();
    else [[unlikely]]
      return {};
  }
  constexpr auto native_handle() const noexcept { return handle_; }
  constexpr auto context() const noexcept { return context_; }
  constexpr auto is_valid() const noexcept {
    return handle_ != details::invalid_socket;
  }
  template <ip::family family = ip::family::v4>
  auto bind(const endpoint_type::port_type port = 0) {
    return bind(endpoint_type::template unspecified<family>(port));
  }
  Status bind(const endpoint_type &endpoint) {
    if (details::bind(handle_, endpoint.data(), endpoint.size()) != -1)
        [[likely]]
      return {};
    else [[unlikely]]
      return details::make_bind_error();
  }

  inline auto remote_endpoint() const noexcept { return remote_endpoint_; }
  auto to_string(const FormatPolicy policy) const { return Format(handle_); }

public:
  Status connect(const endpoint_type &endpoint) {
    if (details::connect(handle_, endpoint.data(), endpoint.size()) != -1)
        [[likely]] {
      remote_endpoint_.emplace(endpoint);
      return {};
    } else [[unlikely]]
      return details::make_connect_error();
  }

protected:
  struct _recv {
    static auto operator()(auto &&self) { return self.recv(); }
    static auto operator()(auto &&self, const size_t max_size) {
      return self.recv(max_size);
    }
  };

public:
  static constexpr inline _recv recv_;

protected:
  io_context *context_;
  native_handle_type handle_;
  std::optional<endpoint_type> remote_endpoint_;
};
} // namespace auxilia::net::details
namespace auxilia::net {
template <typename> class socket;
template <> class socket<tcp> : public details::socket_base<tcp> {

public:
  using base_type = socket_base<tcp>;
  using base_type::socket_base;
  friend base_type;
  socket(socket &&socket) : base_type::socket_base(std::move(socket)) {}
  socket &operator=(socket &&socket) {
    base_type::operator=(std::move(socket));
    return *this;
  }
  ~socket() = default;

private:
  struct _listen {
    static auto operator()(auto &&self) { return self.listen(); }
    static auto operator()(auto &&self, const int backlog) {
      return self.listen(backlog);
    }
  };

public:
  Status listen(const int backlog = 0) {
    if (details::listen(handle_, backlog) != -1) [[likely]]
      return {};
    else [[unlikely]]
      return details::make_listen_error();
  }
  static constexpr inline _listen listen_;

protected:
  StatusOr<bytes_type> do_recv(const size_t max_size) {
    if (!remote_endpoint_)
      return UnavailableError(
          "Remote endpoint is not set. make sure you called accept() and it succeeded.");

    bytes_type buffer;
    bool success = true;
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
};
template <> class socket<udp> : public details::socket_base<udp> {

public:
  using base_type = socket_base<udp>;
  using base_type::socket_base;
  friend base_type;
  socket(socket &&socket)
      : base_type::socket_base(std::move(static_cast<base_type &&>(socket))) {}
  socket &operator=(socket &&socket) {
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
          } else if (len != sizeof(details::sockaddr_in4_t) ||
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
