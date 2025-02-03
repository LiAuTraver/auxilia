#pragma once

#include "./config.hpp"

namespace accat::auxilia::id::details {
// inline variables have a single definition shared across translation units,
//    so we won’t generate duplicate IDs.
inline std::atomic_uint32_t _current_id{0};
inline std::unordered_set<uint32_t> _active_ids;
inline std::mutex _id_mutex;
static inline auto _do_insert(const uint32_t id) {
  std::lock_guard<std::mutex> lock(_id_mutex);
  return _active_ids.insert(id);
}
} // namespace accat::auxilia::id::details
EXPORT_AUXILIA
namespace accat::auxilia::id {
inline auto get() {
  uint32_t id;

  do {
    id = details::_current_id.fetch_add(1);
  } while (!details::_do_insert(id).second &&
           details::_active_ids.size() !=
               (std::numeric_limits<uint32_t>::max)());

  return id;
}

inline auto is_active(const uint32_t id) {
  std::lock_guard<std::mutex> lock(details::_id_mutex);
  return details::_active_ids.contains(id);
}

inline auto release(const uint32_t id) {
  std::lock_guard<std::mutex> lock(details::_id_mutex);
  return details::_active_ids.erase(id);
}
} // namespace accat::auxilia::id
