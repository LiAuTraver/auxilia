#pragma once

#ifdef _WIN32

#  define WIN32_LEAN_AND_MEAN
#  define NOMINMAX
#  include <WinSock2.h>
#  include <mswsock.h>
#  include <ws2ipdef.h>
#  include <io.h>
#  include <ws2tcpip.h>
#  include <winerror.h>
#  pragma comment(lib, "Ws2_32.lib")

#  include "auxilia/base/macros.hpp"

#  ifdef __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif
namespace auxilia::net::details::inline windows {

using raw_socket_t = ::SOCKET;
using socket_len_type = ::socklen_t;
using socket_storage_type = ::sockaddr_storage;
static constexpr auto invalid_socket = INVALID_SOCKET;
static constexpr auto socket_error = SOCKET_ERROR;

using sockaddr_t = ::sockaddr;
using in4_addr_t = ::in_addr;
using in4_mreq_type = ::ip_mreq;
using sockaddr_in4_t = ::sockaddr_in;

using in6_addr_t = ::in6_addr;
using in6_mreq_type = ::ipv6_mreq;
using sockaddr_in6_t = ::sockaddr_in6;
using sockaddr_storage_t = ::sockaddr_storage;
using addrinfo_t = ::addrinfo;
} // namespace auxilia::net::details::inline windows
namespace auxilia::net::details::inline windows {
AC_FORCEINLINE inline raw_socket_t socket(const ip::family family,
                                          const socket_kind socket_type,
                                          const int protocol = 0) {
  return ::WSASocketW(std::to_underlying(family),
                      std::to_underlying(socket_type),
                      protocol,
                      nullptr, // too complex...
                      0,       // no group
                      WSA_FLAG_OVERLAPPED);
}

AC_FORCEINLINE inline Status listen(const raw_socket_t s,
                                    const int backlog = 0) {
  if (::listen(s, backlog) != -1) [[likely]]
    return {};
  else
    return details::make_listen_error();
}

AC_FORCEINLINE inline auto recv(const raw_socket_t s,
                                char *const buf,
                                const size_t len,
                                [[maybe_unused]] const int = 0,
                                sockaddr_t *from = nullptr,
                                socket_len_type *fromlen = nullptr) {
  auto wsabuf = WSABUF{.len = static_cast<ULONG>(len), .buf = buf};
  DWORD bytes_received = 0;
  DWORD flags_out = 0;
  auto result = ::WSARecvFrom(s,
                              &wsabuf,
                              1, // buffer count
                              &bytes_received,
                              &flags_out,
                              from,
                              fromlen,
                              nullptr, // no overlapped structure
                              nullptr  // no completion routine
  );
  if (result == socket_error)
    if (::WSAGetLastError() == WSAEWOULDBLOCK)
      // For non-blocking sockets, WSARecvFrom returns WSAEWOULDBLOCK if no data
      // is available.
      // treat this as a special case and return 0 bytes received.
      return (::WSASetLastError(ERROR_SUCCESS), 0);
    else
      return socket_error;
  else
    return static_cast<int>(bytes_received);
}
AC_FORCEINLINE inline auto send(const raw_socket_t s,
                                std::string &&str,
                                const int flags = 0,
                                const sockaddr_t *to = nullptr,
                                const socket_len_type tolen = 0) {
  auto wsabuf = WSABUF{.len = static_cast<ULONG>(str.size()),
                       .buf = const_cast<char *>(str.c_str())};
  DWORD bytes_sent = 0;
  auto result = ::WSASendTo(
      s, &wsabuf, 1, &bytes_sent, flags, to, tolen, nullptr, nullptr);
  if (result == socket_error)
    return ::WSAGetLastError() == WSAEWOULDBLOCK
               ? (::WSASetLastError(ERROR_SUCCESS), 0)
               : socket_error;
  else
    return static_cast<int>(bytes_sent);
}
AC_FORCEINLINE inline Status closesocket(const raw_socket_t s) {
  if (::closesocket(s) != -1) [[likely]]
    return {};
  else
    return make_close_error();
}

AC_FORCEINLINE inline auto
accept(const raw_socket_t s, sockaddr_t *addr, socket_len_type *addrlen) {
  return ::WSAAccept(s, addr, addrlen, nullptr, 0);
}

AC_FORCEINLINE inline auto connect(const raw_socket_t s,
                                   const sockaddr_t *addr,
                                   const socket_len_type addrlen) {
  return ::WSAConnect(s, addr, addrlen, nullptr, nullptr, nullptr, nullptr);
}
AC_FORCEINLINE inline auto bind(const raw_socket_t s,
                                const sockaddr_t *addr,
                                const socket_len_type addrlen) {
  return ::bind(s, addr, addrlen);
}
} // namespace auxilia::net::details::inline windows

#  include "error.hpp" // IWYU pragma: export
#  include "iocp.hpp"  // IWYU pragma: export
#endif
