#pragma once
#ifdef __linux__

#  include <netinet/in.h>
#  include <sys/socket.h>
#  include <sys/types.h>
#  include <unistd.h>

#  include "auxilia/status/Status.hpp"
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
