#pragma once
#ifdef __linux__
#  include <netinet/in.h>
#  include <sys/socket.h>
#  include <sys/types.h>
#  include <unistd.h>

#  ifdef __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif
namespace auxilia::net::details::inline os {
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
} // namespace auxilia::net::details::inline os

#  include "error.hpp" // IWYU pragma: export

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
  return ::close(s);
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
#endif
