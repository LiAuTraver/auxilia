#pragma once

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX

#include <WinSock2.h>
#include <ws2ipdef.h>
#include <io.h>

// Windows
namespace auxilia::net::details::inline os {
using raw_socket_t = ::SOCKET;

using sockaddr_t = ::sockaddr;
using in4_addr_t = ::in_addr;
using in4_mreq_type = ::ip_mreq;
using sockaddr_in4_t = ::sockaddr_in;

using in6_addr_t = ::in6_addr;
using in6_mreq_type = ::ipv6_mreq;
using sockaddr_in6_t = ::sockaddr_in6;
using sockaddr_storage_t = ::sockaddr_storage;
using addrinfo_t = ::addrinfo;

using ::socket;

using ::accept;
using ::bind;
using ::connect;
using ::listen;

using ::recv;
using ::send;

using ::recvfrom;
using ::sendto;

using ::read;  // C lib func
using ::write; // C lib func

using ::select;
// using ::poll;

using ::close;

// using ::getaddrinfo;
// using ::setaddrinfo;
using ::gethostbyaddr;
using ::gethostbyname;

using ::getsockopt;
using ::setsockopt;
} // namespace auxilia::net::details::inline os
