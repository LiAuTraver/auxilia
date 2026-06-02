#pragma once

#ifdef _WIN32

#  if __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif

namespace auxilia::net::details::iocp {
struct AC_EMPTY_BASES AC_NOVTABLE operation {
  using complete_fn = void (*)(operation *, DWORD, DWORD) noexcept;
  constexpr explicit operation(const complete_fn complete = nullptr) noexcept
      : overlapped({}), complete(complete) {}
  OVERLAPPED overlapped;
  complete_fn complete;
};

struct AC_EMPTY_BASES AC_NOVTABLE completion {
  operation *op;
  DWORD bytes;
  DWORD error;
};
} // namespace auxilia::net::details::iocp

#endif
