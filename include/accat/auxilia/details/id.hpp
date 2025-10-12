#pragma once

#include "./config.hpp"

namespace accat::auxilia::id::details {
inline auto &_current_id() {
  static std::atomic_uint32_t value{0};
  return value;
}

inline auto &_active_ids() {
  static std::unordered_set<uint32_t> ids;
  return ids;
}

inline auto &_id_mutex() {
  static std::mutex m;
  return m;
}
static inline auto _do_insert(const uint32_t id) {
  std::lock_guard<std::mutex> lock(_id_mutex());
  return _active_ids().insert(id);
}
} // namespace accat::auxilia::id::details
EXPORT_AUXILIA
namespace accat::auxilia::id {
inline auto get() {
  uint32_t id;

  do {
    id = details::_current_id().fetch_add(1);
  } while (!details::_do_insert(id).second &&
           details::_active_ids().size() !=
               (std::numeric_limits<uint32_t>::max)());

  return id;
}

inline auto is_active(const uint32_t id) {
  std::lock_guard<std::mutex> lock(details::_id_mutex());
  return details::_active_ids().contains(id);
}

inline auto release(const uint32_t id) {
  std::lock_guard<std::mutex> lock(details::_id_mutex());
  return details::_active_ids().erase(id);
}
} // namespace accat::auxilia::id
