#pragma once

#include "os/basic.hpp" // IWYU pragma: export

#ifdef _WIN32
#  include "os/windows/windows.hpp" // IWYU pragma: export
#elif defined(__linux__)
#  include "os/linux/linux.hpp" // IWYU pragma: export
#else
#  error unsupported
#endif
