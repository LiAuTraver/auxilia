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
        "Insufficient memory is available. "
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
        "all port numbers in the ephemeral port range are currently in use. "
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
        "all port numbers in the ephemeral port range are currently in use. "
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
        "(Internet domain sockets) The socket referred to by sockfd had not previously been bound to an address and, "
        "upon attempting to bind it to an ephemeral port, "
        "it was determined that all port numbers in the ephemeral port range are currently in use. "
        "See the discussion of /proc/sys/net/ipv4/ip_local_port_range in ip(7).");
  case EAFNOSUPPORT:
    return UnavailableError(
        "The passed address didn't have the correct address family in its sa_family field.");
  case EAGAIN:
    return UnavailableError(
        "For nonblocking UNIX domain sockets, the socket is nonblocking, "
        "and the connection cannot be completed immediately. \n"
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
[[gnu::cold]] static inline Status make_send_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EACCES:
    return PermissionDeniedError(
        "(For UNIX domain sockets, which are identified by pathname) Write permission is denied on the destination socket file, "
        "or search permission is denied for one of the directories the path prefix. (See path_resolution(7).)\n"
        "(For UDP sockets) An attempt was made to send to a network/broadcast address as though it was a unicast address.");
  case EAGAIN:
#  if EAGAIN != EWOULDBLOCK
    [[fallthrough]];
  case EWOULDBLOCK:
#  endif
    return UnavailableError(
        "The socket is marked nonblocking and the requested operation would block. "
        "POSIX.1-2001 allows either error to be returned for this case, and does not require these constants to have the same value, "
        "so a portable application should check for both possibilities. \n"
        "(Internet domain datagram sockets) The socket referred to by sockfd had not previously been bound to an address and, "
        "upon attempting to bind it to an ephemeral port, it was determined that all port numbers in the ephemeral port range are currently in use.  "
        "See the discussion of /proc/sys/net/ipv4/ip_local_port_range in ip(7).\n");
  case EALREADY:
    return UnavailableError("Another Fast Open is in progress.");
  case EBADF:
    return InvalidArgumentError("sockfd is not a valid file descriptor.");
  case ECONNRESET:
    return UnavailableError("Connection reset by peer.");
  case EDESTADDRREQ:
    return FailedPreconditionError(
        "The socket is not connection-mode, and no peer address is set.");
  case EFAULT:
    return InvalidArgumentError(
        "An invalid user space address was specified for an argument.");
  case EINTR:
    return UnavailableError(
        "A signal occurred before any data was transmitted; see signal(7).");
  case EINVAL:
    return InvalidArgumentError("Invalid argument passed.");
  case EISCONN:
    return AlreadyExistsError(
        "The connection-mode socket was connected already but a recipient was specified. "
        "(Now either this error is returned, or the recipient specification is ignored.)");
  case EMSGSIZE:
    return InvalidArgumentError(
        "The socket type requires that message be sent atomically, "
        "and the size of the message to be sent made this impossible.");
  case ENOBUFS:
    return ResourceExhaustedError(
        "The output queue for a network interface was full. "
        "This generally indicates that the interface has stopped sending, but may be caused by transient congestion. "
        "(Normally, this does not occur in Linux. Packets are just silently dropped when a device queue overflows.)");
  case ENOMEM:
    return ResourceExhaustedError("No memory available.");
  case ENOTCONN:
    return FailedPreconditionError(
        "The socket is not connected, and no target has been given.");
  case ENOTSOCK:
    return InvalidArgumentError(
        "The file descriptor sockfd does not refer to a socket.");
  case EOPNOTSUPP:
    return InvalidArgumentError(
        "Some bit in the flags argument is inappropriate for the socket type.");
  case EPIPE:
    return UnavailableError(
        "The local end has been shut down on a connection oriented socket. "
        "In this case, the process will also receive a SIGPIPE unless MSG_NOSIGNAL is set.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
[[gnu::cold]] static inline Status make_close_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EBADF:
    return InvalidArgumentError("sockfd is not a valid file descriptor.");
  case EINTR:
    return UnavailableError(
        "The close() call was interrupted by a signal; see signal(7)");
  case EIO:
    return UnavailableError("An I/O error occurred.");
  case ENOSPC:
    [[fallthrough]];
  case EDQUOT:
    return ResourceExhaustedError(
        "On NFS, these errors are not normally reported against the first write which exceeds the available storage space, "
        "but instead against a subsequent write(2), fsync(2), or close().");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}

[[gnu::cold]] static inline Status make_accept_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EAGAIN:
#  if EAGAIN != EWOULDBLOCK
    [[fallthrough]];
  case EWOULDBLOCK:
#  endif
    return UnavailableError(
        "The socket is marked nonblocking and no connections are present to be accepted.  POSIX.1-2001 and POSIX.1-2008 allow either error to be returned for this case, and do not require these constants to have the same value, so a portable application should check for both possibilities.");
  case EBADF:
    return InvalidArgumentError("sockfd is not an open file descriptor.");
  case ECONNABORTED:
    return UnavailableError("A connection has been aborted.");
  case EFAULT:
    return InvalidArgumentError(
        "The addr argument is not in a writable part of the user address space.");
  case EINTR:
    return UnavailableError(
        "The system call was interrupted by a signal that was caught before a valid connection arrived; see signal(7).");
  case EINVAL:
    return InvalidArgumentError(
        "Socket is not listening for connections, or addrlen is invalid ( e.g., is negative). \n"
        "(accept4()) invalid value in flags.");
  case EMFILE:
    return ResourceExhaustedError(
        "The per-process limit on the number of open file descriptors has been reached.");
  case ENFILE:
    return ResourceExhaustedError(
        "The system-wide limit on the total number of open files has been reached.");
  case ENOBUFS:
    [[fallthrough]];
  case ENOMEM:
    return ResourceExhaustedError(
        "Not enough free memory. This often means that the memory allocation is limited by the socket buffer limits, not by the system memory.");
  case ENOTSOCK:
    return InvalidArgumentError(
        "The file descriptor sockfd does not refer to a socket.");
  case EOPNOTSUPP:
    return InvalidArgumentError(
        "The referenced socket is not of type SOCK_STREAM.");
  case EPERM:
    return PermissionDeniedError("Firewall rules forbid connection.");
  case EPROTO:
    return UnavailableError("Protocol error.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
[[gnu::cold]] static inline Status make_recv_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EAGAIN:
#  if EAGAIN != EWOULDBLOCK
    [[fallthrough]];
  case EWOULDBLOCK:
#  endif
    return UnavailableError(
        "The socket is marked nonblocking and the accept() operation would block, or a receive timeout had been set and the timeout expired before a connection was made.  "
        "POSIX.1 allows either error to be returned for this case, and does not require these constants to have the same value, so a portable application should check for both possibilities.");
  case EBADF:
    return InvalidArgumentError("sockfd is not a valid file descriptor.");
  case ECONNABORTED:
    return UnavailableError(
        "A remote host refused to allow the network connection (typically because it is not running the requested service).");
  case EFAULT:
    return InvalidArgumentError(
        "The receive buffer pointer(s) point outside the process's address space.");
  case EINTR:
    return UnavailableError(
        "The receive was interrupted by delivery of a signal before any data was available; see signal(7).");
  case EINVAL:
    return InvalidArgumentError("Invalid argument passed.");
  case ENOMEM:
    return ResourceExhaustedError("Could not allocate memory for recvmsg().");
  case ENOTCONN:
    return FailedPreconditionError(
        "The socket is associated with a connection-oriented protocol and has not been connected (see connect(2) and accept(2)).");
  case ENOTSOCK:
    return InvalidArgumentError(
        "The file descriptor sockfd does not refer to a socket.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
namespace epoll {

[[gnu::cold]] static inline Status make_create_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EINVAL:
    // i only used epoll_create1. epoll_create is 'size is not positive'.
    return InvalidArgumentError("Invalid value specified in flags.");
  case EMFILE:
    return ResourceExhaustedError(
        "The per-process limit on the number of open file descriptors has been reached.");
  case ENFILE:
    return ResourceExhaustedError(
        "The system-wide limit on the total number of open files has been reached.");
  case ENOMEM:
    return ResourceExhaustedError(
        "There was insufficient memory to create the kernel object.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
[[gnu::cold]] static inline Status make_eventfd_create_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EINVAL:
    return InvalidArgumentError("An unsupported value was specified in flags.");
  case EMFILE:
    return ResourceExhaustedError(
        "The per-process limit on the number of open file descriptors has been reached.");
  case ENFILE:
    return ResourceExhaustedError(
        "The system-wide limit on the total number of open files has been reached.");
  case ENODEV:
    return UnavailableError(
        "Could not mount (internal) anonymous inode device.");
  case ENOMEM:
    return ResourceExhaustedError(
        "There was insufficient memory to create a new eventfd file descriptor.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
[[gnu::cold]] static inline Status make_ctl_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EBADF:
    return InvalidArgumentError("epfd or fd is not a valid file descriptor.");
  case EEXIST:
    return AlreadyExistsError(
        "op was EPOLL_CTL_ADD, and the supplied file descriptor fd is already registered with this epoll instance.");
  case EINVAL:
    return InvalidArgumentError(
        "epfd is not an epoll file descriptor, or fd is the same as epfd, or the requested operation op is not supported by this interface;\n"
        "or an invalid event type was specified along with EPOLLEXCLUSIVE;\n"
        "or op was EPOLL_CTL_MOD and events included EPOLLEXCLUSIVE;\n"
        "or op was EPOLL_CTL_MOD and the EPOLLEXCLUSIVE flag has previously been applied to this epfd, fd pair;\n"
        "or EPOLLEXCLUSIVE was specified in event and fd refers to an epoll instance.");
  case ELOOP:
    return UnavailableError(
        "fd refers to an epoll instance and this EPOLL_CTL_ADD operation would result in a circular loop of epoll instances monitoring one another or a nesting depth of epoll instances greater than 5.");
  case ENOENT:
    return NotFoundError(
        "op was EPOLL_CTL_MOD or EPOLL_CTL_DEL, and fd is not registered with this epoll instance.");
  case ENOMEM:
    return ResourceExhaustedError(
        "There was insufficient memory to handle the requested op control operation.");
  case ENOSPC:
    return ResourceExhaustedError(
        "The limit imposed by /proc/sys/fs/epoll/max_user_watches was encountered while trying to register (EPOLL_CTL_ADD) a new file descriptor on an epoll instance.  See epoll(7) for further details.");
  case EPERM:
    return PermissionDeniedError(
        "The target file fd does not support epoll.  This error can occur if fd refers to, for example, a regular file or a directory.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
[[gnu::cold]] static inline Status make_wait_error() {
  AC_PRECONDITION(errno != 0)
  AC_DEFER { errno = 0; };
  switch (errno) {
  case EBADF:
    return InvalidArgumentError("epfd is not a valid file descriptor.");
  case EFAULT:
    return InvalidArgumentError(
        "The memory area pointed to by events is not accessible with write permissions.");
  case EINTR:
    return UnavailableError(
        "The call was interrupted by a signal handler before either (1) any of the requested events occurred or (2) the timeout expired; see signal(7).");
  case EINVAL:
    return InvalidArgumentError(
        "epfd is not an epoll file descriptor, or n is less than or equal to zero.");
  default:
    break;
  }
  return UnknownError(::strerror(errno));
}
} // namespace epoll
} // namespace auxilia::net::details
#endif
