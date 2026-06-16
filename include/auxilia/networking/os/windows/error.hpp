#pragma once
#if _WIN32
#  define WIN32_LEAN_AND_MEAN
#  define NOMINMAX
#  include <WinSock2.h>
#  include <ws2ipdef.h>
#  include <io.h>
#  include <ws2tcpip.h>
#  include <winerror.h>
#  pragma comment(lib, "Ws2_32.lib")

#  include "auxilia/status/Status.hpp"

namespace auxilia::net::details {
[[gnu::cold]] static inline Status win_error() {
  AC_DEFER { ::SetLastError(ERROR_SUCCESS); };

  std::string message;
  constexpr auto max_size = 0x0400ULL;

  message.resize_and_overwrite(
      max_size, [](char *buf, const size_t size) -> size_t {
        return ::FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                                   FORMAT_MESSAGE_IGNORE_INSERTS,
                               nullptr,
                               ::GetLastError(),
                               MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                               buf,
                               static_cast<DWORD>(size),
                               nullptr);
      });
  if (message.empty()) {
    if (::GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
      // ::SetLastError(ERROR_SUCCESS);
      message = "insufficient memory for error message";
    } else
      message = "Error code unrecognized";
  }
  return UnknownError(std::move(message));
}
[[gnu::cold]] static inline Status wsa_error() {
  AC_DEFER { ::WSASetLastError(ERROR_SUCCESS); };

  std::string message;

  constexpr auto max_size = 0x0200ULL;

  message.resize_and_overwrite(
      max_size, [](char *buf, const size_t size) -> size_t {
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
      // ::SetLastError(ERROR_SUCCESS);
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
  return wsa_error();
}
} // namespace auxilia::net::details
#endif
