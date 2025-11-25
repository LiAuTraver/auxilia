#pragma once

#include <atomic>
#include <cstddef>
#include <mutex>
#include <cstdint>
#include <limits>
#include <array>

#include "accat/auxilia/base/config.hpp"

namespace accat::auxilia::id::details {
inline auto &_current_id() {
  static std::atomic_uint32_t value{0};
  return value;
}

constexpr size_t id_pool_size = std::numeric_limits<unsigned char>::max() + 1;
inline auto &_active_ids() {
  static std::array<std::atomic_bool, id_pool_size> ids{};
  return ids;
}

} // namespace accat::auxilia::id::details

EXPORT_AUXILIA
namespace accat::auxilia::id {
inline auto get() {
  uint32_t id;
  bool expected;
  do {
    id = details::_current_id().fetch_add(1) % details::id_pool_size;
    expected = false;
  } while (!details::_active_ids()[id].compare_exchange_weak(
      expected, true, std::memory_order::release, std::memory_order::relaxed));

  return id;
}

inline auto is_active(const uint32_t id) {
  if (id >= details::id_pool_size)
    return false;
  return details::_active_ids()[id].load(std::memory_order::acquire);
}

inline auto release(const uint32_t id) {
  if (id >= details::id_pool_size)
    return;
  details::_active_ids()[id].store(false, std::memory_order::release);
}
} // namespace accat::auxilia::id
