#pragma once
#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  define NOMINMAX
#  include <WinSock2.h>
#  include <ws2ipdef.h>
#  include <io.h>
#  include <ws2tcpip.h>
#  include <winerror.h>
// Windows
#  pragma comment(lib, "Ws2_32.lib")
#elif defined(__linux__)
#  include <netinet/in.h>
#  include <sys/socket.h>
#  include <sys/types.h>
#  include <unistd.h>
#else
#  error unsupported
#endif

#include <type_traits>
#include <algorithm>
#include <string>
#include <utility>

#include "auxilia/base/macros.hpp"

namespace auxilia::net::ip {
enum class family { v4 = AF_INET, v6 = AF_INET6 };
}
namespace auxilia::net {
enum class socket_type { stream = SOCK_STREAM, datagram = SOCK_DGRAM };
}
namespace auxilia::net::details::inline os {
#ifdef _WIN32

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
#elif defined(__linux__)
using raw_socket_t = int;
using socket_len_type = socklen_t;
using socket_storage_type = sockaddr_storage;
static constexpr auto invalid_socket = -1;
static constexpr auto socket_error = -1;
using sockaddr_t = ::sockaddr;
using sockaddr_in4_t = ::sockaddr_in;
using sockaddr_in6_t = ::sockaddr_in6;
using sockaddr_storage_t = ::sockaddr_storage;
using in4_addr_t = ::in_addr;
using in4_mreq_type = ::ip_mreq;
using in6_addr_t = ::in6_addr;
using in6_mreq_type = ::ipv6_mreq;
// using addrinfo_t = ::addrinfo;
#else
#  error unsupported
#endif
using port_type = unsigned short;
} // namespace auxilia::net::details::inline os
namespace auxilia::net::details::inline os {
AC_FORCEINLINE inline raw_socket_t socket(const ip::family family,
                                          const socket_type socket_type,
                                          const int protocol = 0) {
  return ::socket(
      std::to_underlying(family), std::to_underlying(socket_type), protocol);
}

AC_FORCEINLINE inline auto listen(const raw_socket_t s, const int backlog = 0) {
  return ::listen(s, backlog);
}
AC_FORCEINLINE inline auto recv(const raw_socket_t s,
                                char *const buf,
                                const size_t len,
                                const int flags = 0,
                                sockaddr_t *from = nullptr,
                                socket_len_type *fromlen = nullptr) {
  return ::recvfrom(s, buf, len, flags, from, fromlen); // NOLINT
}
AC_FORCEINLINE inline auto send(const raw_socket_t s,
                                std::string &&str,
                                const int flags = 0,
                                const sockaddr_t *to = nullptr,
                                const socket_len_type tolen = 0) {
  return ::sendto(s, str.c_str(), str.size(), flags, to, tolen); // NOLINT
}
AC_FORCEINLINE inline auto closesocket(const raw_socket_t s) {
#ifdef _WIN32
  return ::closesocket(s);
#else
  return ::close(s);
#endif
}

using ::accept;
using ::bind;
using ::connect;

using ::recvfrom;
using ::sendto;

// using ::read;  // C lib func
// using ::write; // C lib func

using ::select;
// using ::poll;

// using ::close;

// using ::getaddrinfo;
// using ::setaddrinfo;
// using ::gethostbyaddr;
// using ::gethostbyname;

using ::getsockopt;
using ::setsockopt;

} // namespace auxilia::net::details::inline os
#include "auxilia/status/Status.hpp"
#include "auxilia/status/StatusOr.hpp"
namespace auxilia::net::details {
/// `htons` and `htonl`, but in a *constexpr* way.
template <typename Unsigned>
static inline constexpr Unsigned host2net(const Unsigned host) noexcept {
  static_assert(std::is_unsigned_v<Unsigned>);
  if constexpr (std::endian::native == std::endian::little)
    return std::byteswap(host);
  else
    return host;
}
/// `ntohs` and `ntohl`, in a *constexpr* way.
template <typename Unsigned>
static inline constexpr Unsigned net2host(const Unsigned net) noexcept {
  static_assert(std::is_unsigned_v<Unsigned>);
  if constexpr (std::endian::native == std::endian::little)
    return std::byteswap(net);
  else
    return net;
}

[[gnu::cold]] static inline Status make_ctor_error();
[[gnu::cold]] static inline Status make_bind_error();
[[gnu::cold]] static inline Status make_listen_error();
[[gnu::cold]] static inline Status make_connect_error();
[[gnu::cold]] static inline Status make_send_error();
[[gnu::cold]] static inline Status make_close_error();
[[gnu::cold]] static inline Status make_accept_error();
[[gnu::cold]] static inline Status make_recv_error();
} // namespace auxilia::net::details
#ifdef _WIN32
namespace auxilia::net::details {
[[gnu::cold]] static inline Status wsa_error() {
  AC_DEFER { ::WSASetLastError(ERROR_SUCCESS); };

  std::string message;

  constexpr auto max_size = 0x0200ULL;

  message.resize_and_overwrite(max_size, [](char *buf, size_t size) -> size_t {
    return ::FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                               FORMAT_MESSAGE_IGNORE_INSERTS,
                           nullptr,
                           ::WSAGetLastError(),
                           MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                           buf,
                           static_cast<DWORD>(size),
                           nullptr);
  });

  if (message.empty()) {
    if (::GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
      ::SetLastError(ERROR_SUCCESS);
      message = "insufficient memory for error message";
    } else
      message = "Error code unrecognized";
  }

  return UnknownError(std::move(message));
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_ctor_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_bind_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_listen_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_connect_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_send_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_close_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_accept_error() {
  return wsa_error();
}
[[gnu::cold]] AC_FORCEINLINE static inline Status make_recv_error() {
  if (::WSAGetLastError() == ERROR_SUCCESS)
    return {};
  else
    return wsa_error();
}
} // namespace auxilia::net::details
#else
namespace auxilia::net::details {
/// @version 6.17
/// `man socket`
[[gnu::cold]] static inline Status make_ctor_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EACCES:
    return PermissionDeniedError(
        "Permission to create a socket of the specified type and/or protocol is denied.");
  case EAFNOSUPPORT:
    return UnavailableError(
        "The implementation does not support the specified address family.");
  case EINVAL:
    return InvalidArgumentError(
        "Unknown protocol, or protocol family not available, "
        "or the socket type is invalid.");
  case EMFILE:
    return ResourceExhaustedError(
        "The per-process or system-wide limit on the number of "
        "open file descriptors has been reached.");
  case ENOBUFS:
    [[fallthrough]];
  case ENOMEM:
    return ResourceExhaustedError(
        "Insufficient memory is available.  "
        "The socket cannot be created until sufficient resources are freed.");
  case EPROTONOSUPPORT:
    return UnavailableError(
        "The protocol type or the specified protocol is not supported within this domain.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
/// @version 6.17
/// `man bind`
[[gnu::cold]] static inline Status make_bind_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EACCES:
    return PermissionDeniedError(
        "The address is protected, and the user is not the superuser.");
  case EADDRINUSE:
    return AlreadyExistsError(
        "The given address is already in use, "
        "or the port number was specified as zero in the socket address structure, "
        "but, upon attempting to bind to an ephemeral port, it was determined that "
        "all port numbers in the ephemeral port range are currently in use.  "
        "See the discussion of /proc/sys/net/ipv4/ip_local_port_range ip(7).");
  case EBADF:
    return InvalidArgumentError("sockfd is not a valid file descriptor.");
  case EINVAL:
    return InvalidArgumentError(
        "The socket is already bound to an address, or addrlen is wrong, "
        "or addr is not a valid address for this socket's domain.");
  case ENOTSOCK:
    return InvalidArgumentError(
        "The file descriptor sockfd does not refer to a socket.");
  case EADDRNOTAVAIL:
    return InvalidArgumentError("A nonexistent interface was requested or "
                                "the requested address was not local.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
/// @version 6.17
/// `man listen`
[[gnu::cold]] static inline Status make_listen_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EADDRINUSE:
    return AlreadyExistsError(
        "The given address is already in use, "
        "or the port number was specified as zero in the socket address structure, "
        "but, upon attempting to bind to an ephemeral port, it was determined that "
        "all port numbers in the ephemeral port range are currently in use.  "
        "See the discussion of /proc/sys/net/ipv4/ip_local_port_range ip(7).");
  case EBADF:
    return InvalidArgumentError("sockfd is not a valid file descriptor.");
  case ENOTSOCK:
    return InvalidArgumentError(
        "The file descriptor sockfd does not refer to a socket.");
  case EOPNOTSUPP:
    return InvalidArgumentError(
        "The socket is not of a type that supports the listen() operation.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
/// @version 6.17
/// `man connect`
[[gnu::cold]] static inline Status make_connect_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EPERM:
    [[fallthrough]];
  case EACCES:
    return PermissionDeniedError(
        "The user tried to connect to a broadcast address without "
        "having the socket broadcast flag enabled "
        "or the connection request failed because of a local firewall rule.\n"
        "It can also be returned if an SELinux policy denied a connection "
        "(for example, if there is a policy saying that "
        "an HTTP proxy can only connect to ports associated with HTTP servers, "
        "and the proxy tries to connect to a different port)\n"
        "For UNIX domain sockets, which are identified by pathname: "
        "Write permission is denied on the socket file, "
        "or search permission is denied for one of the directories in the path prefix. "
        "(See also path_resolution(7).)");
  case EADDRINUSE:
    return AlreadyExistsError("Local address is already in use.");
  case EADDRNOTAVAIL:
    return InvalidArgumentError(
        "(Internet  domain  sockets) The socket referred to by sockfd had not previously been bound to an address and, "
        "upon attempting to bind it to an ephemeral port, "
        "it was determined that all port numbers in the ephemeral port range are currently in use. "
        "See the discussion of /proc/sys/net/ipv4/ip_local_port_range in ip(7).");
  case EAFNOSUPPORT:
    return UnavailableError(
        "The passed address didn't have the correct address family in its sa_family field.");
  case EAGAIN:
    return UnavailableError(
        "For nonblocking UNIX domain sockets, the socket is nonblocking, "
        "and the connection cannot be completed immediately.  \n"
        "For other socket families, there are insufficient entries in the routing cache.");
  case EALREADY:
    return UnavailableError(
        "The socket is nonblocking and a previous connection attempt has not yet been completed.");
  case EBADF:
    return InvalidArgumentError("sockfd is not a valid file descriptor.");
  case ECONNREFUSED:
    return UnavailableError(
        "A connect() on a stream socket found no one listening on the remote address.");
  case EFAULT:
    return InvalidArgumentError(
        "The socket structure address is outside the user's address space.");
  case EINPROGRESS:
    return UnavailableError(
        "The socket is nonblocking and the connection cannot be completed immediately. "
        "It is possible to select(2) or poll(2) for completion by selecting the socket for writing. "
        "After select(2) indicates writability, "
        "use getsockopt(2) to read the SO_ERROR option at level SOL_SOCKET to determine "
        "whether connect() completed successfully (SO_ERROR is zero) "
        "or unsuccessfully (SO_ERROR is one of the usual error codes listed here, explaining the reason for the failure).");
  case EINTR:
    return UnavailableError(
        "The system call was interrupted by a signal that was caught; see signal(7).");
  case EISCONN:
    return AlreadyExistsError("The socket is already connected.");
  case ENETUNREACH:
    return UnavailableError("Network is unreachable.");
  case ENOTSOCK:
    return InvalidArgumentError(
        "The file descriptor sockfd does not refer to a socket.");
  case EPROTOTYPE:
    return InvalidArgumentError(
        "The socket type does not support the requested communications protocol. This error can occur, for example, on an attempt to connect a UNIX domain datagram socket to a stream socket.");
  case ETIMEDOUT:
    return UnavailableError(
        "Timeout while attempting connection. The server may be too busy to accept new connections. Note that for IP sockets the timeout may be very long when syncookies are enabled on the server.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
[[gnu::cold]] static inline Status make_send_error() { AC_TODO_(); }
[[gnu::cold]] static inline Status make_close_error() { AC_TODO_(); }
[[gnu::cold]] static inline Status make_accept_error() { AC_TODO_(); }
[[gnu::cold]] static inline Status make_recv_error() { AC_TODO_(); }
} // namespace auxilia::net::details
#endif
