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
template <typename Protocol> class socket : Printable {
  static_assert(InternetProtocol<Protocol>);
  // functional chaining methods. since function with default arguments cannot
  // be used as &Class::method, this is an overloaded one.
#define AC_SOCKET_FUNCTIONAL_METHOD(_name_)                                    \
  /* functional chaining method */                                             \
  static constexpr inline auto _name_##_ = [](auto &&self) {                   \
    return self._name_();                                                      \
  }

public:
  using native_handle_type = details::raw_socket_t;
  using protocol_type = Protocol;
  using endpoint_type = protocol_type::endpoint_type;
  using address_type = ip::address;
  using bytes_view_type = std::string_view;
  using bytes_type = std::string;
  static_assert(container_traits<bytes_type>::is_reservable);
  static_assert(container_traits<bytes_type>::is_contiguous);

public:
  constexpr socket(
      io_context *context = nullptr,
      const native_handle_type handle = details::invalid_socket) noexcept
      : context_(context), handle_(handle), remote_endpoint_(std::nullopt) {}
  constexpr socket(
      io_context &context,
      const native_handle_type handle = details::invalid_socket) noexcept
      : context_(&context), handle_(handle), remote_endpoint_(std::nullopt) {}
  inline socket(io_context *context, const ip::family family)
      : context_(context),
        handle_(details::socket(family, protocol_type::socket_type())),
        remote_endpoint_(std::nullopt) {}
  inline socket(io_context &context, const ip::family family)
      : context_(&context),
        handle_(details::socket(family, protocol_type::socket_type())),
        remote_endpoint_(std::nullopt) {}
  socket(const socket &) = delete;
  socket &operator=(const socket &) = delete;
  socket(socket &&that) noexcept
      : context_(std::exchange(that.context_, nullptr)),
        handle_(std::exchange(that.handle_, details::invalid_socket)),
        remote_endpoint_(std::move(that.remote_endpoint_)) {}

  socket &operator=(socket &&that) noexcept {
    if (this != &that) {
      close();
      context_ = std::exchange(that.context_, nullptr);
      handle_ = std::exchange(that.handle_, details::invalid_socket);
      remote_endpoint_ = std::move(that.remote_endpoint_);
    }
    return *this;
  }
  inline ~socket() noexcept { close(); }

  inline static StatusOr<socket> v4(io_context *context = nullptr) {
    if (auto self = socket(context, ip::family::v4); self.is_valid()) [[likely]]
      return {std::move(self)};
    else [[unlikely]]
      return {details::make_ctor_error()};
  }
  inline static StatusOr<socket> v6(io_context *context = nullptr) {
    if (auto self = socket(context, ip::family::v6); self.is_valid()) [[likely]]
      return {std::move(self)};
    else [[unlikely]]
      return {details::make_ctor_error()};
  }
  auto to_string(const FormatPolicy policy) const { return Format(handle_); }

public:
  constexpr auto native_handle() const noexcept { return handle_; }
  constexpr auto context() const noexcept { return context_; }
  constexpr auto is_valid() const noexcept {
    return handle_ != details::invalid_socket;
  }
  Status bind(const endpoint_type &address) {
    if (details::bind(handle_, address.data(), address.size()) != -1) [[likely]]
      return {};
    else [[unlikely]]
      return details::make_bind_error();
  }
  Status listen(const int backlog = 0) {
    if (details::listen(handle_, backlog) != -1) [[likely]]
      return {};
    else [[unlikely]]
      return details::make_listen_error();
  }
  AC_SOCKET_FUNCTIONAL_METHOD(listen);
  Status connect(const endpoint_type &address) {
    if (details::connect(handle_, address.data(), address.size()) != -1)
        [[likely]]
      return {};
    else [[unlikely]]
      return details::make_connect_error();
  }
  Status send_bytes(bytes_type &&bytes) {
    if (details::send(handle_, std::move(bytes)) != -1) [[likely]]
      return {};
    else
      return details::make_send_error();
  }
  Status close() const {
    if (is_valid()) [[likely]]
      if (details::closesocket(handle_) != -1) [[likely]]
        return {};
      else [[unlikely]]
        return details::make_close_error();
    else [[unlikely]]
      return {};
  }
  StatusOr<bytes_type> recv(const size_t max_size = 0x0400) const {
    if (!remote_endpoint_)
      return UnavailableError(
          "Remote endpoint is not set. make sure you called accept() and it succeeded.");

    bytes_type buffer;
    bool success = true;
    buffer.resize_and_overwrite(
        max_size,
        [this, &success](char *buf, const size_t buf_size) noexcept -> size_t {
          // NOLINTNEXTLINE
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
  AC_SOCKET_FUNCTIONAL_METHOD(recv);
  AC_NODISCARD_REASON(
      "the `accept` method returns a new socket that you should use to communicate with the client.")
  inline StatusOr<socket> accept() {
    details::socket_len_type len = sizeof(details::socket_storage_type);
    details::socket_storage_type storage;
    if (auto h = details::accept(
            handle_, reinterpret_cast<details::sockaddr_t *>(&storage), &len);
        h == -1)
      return details::make_accept_error();
    else if (len == sizeof(details::sockaddr_in4_t))
      return {{context_, h, reinterpret_cast<endpoint_type &&>(storage)}};
    else if (len == sizeof(details::sockaddr_in6_t))
      return {{context_, h, reinterpret_cast<endpoint_type &&>(storage)}};
    else
      return UnavailableError(
          "Unknown or unsupported address family of the accepted socket.");
  }

private:
  socket(io_context *context,
         const native_handle_type handle,
         endpoint_type &&remote_endpoint) noexcept
      : context_(context), handle_(handle),
        remote_endpoint_(std::move(remote_endpoint)) {}

private:
  io_context *context_;
  native_handle_type handle_;
  std::optional<endpoint_type> remote_endpoint_;
};
} // namespace auxilia::net
