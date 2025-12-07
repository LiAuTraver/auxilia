#pragma once

#include "accat/auxilia/base/config.hpp"

namespace accat::auxilia {
template <typename Ty>
inline constexpr auto is_container_v = requires(Ty t) {
  typename Ty::size_type;
  typename Ty::value_type;
  typename Ty::iterator;
  typename Ty::const_iterator;
  { t.begin() } -> std::same_as<typename Ty::iterator>;
  { t.end() } -> std::same_as<typename Ty::iterator>;
  { t.cbegin() } -> std::same_as<typename Ty::const_iterator>;
  { t.cend() } -> std::same_as<typename Ty::const_iterator>;
};

template <typename Ty>
struct is_container : std::bool_constant<is_container_v<Ty>> {};

template <typename Container> struct container_traits {

  using size_type = typename Container::size_type;
  using value_type = typename Container::value_type;
  using iterator = typename Container::iterator;
  using const_iterator = typename Container::const_iterator;

  static constexpr bool is_contiguous = requires(Container c) {
    { c.data() } -> std::same_as<value_type *>;
  };

  static constexpr bool is_reversible = requires(Container c) {
    { c.rbegin() } -> std::same_as<typename Container::reverse_iterator>;
    { c.rend() } -> std::same_as<typename Container::reverse_iterator>;
  };

  static constexpr bool is_allocator_aware =
      requires(Container c) { typename Container::allocator_type; };

  static constexpr bool is_sequence = requires(Container c, size_type n) {
    { c.insert(c.end(), std::declval<value_type>()) } -> std::same_as<iterator>;
    { c.erase(c.begin()) } -> std::same_as<iterator>;
    { c.push_back(std::declval<value_type>()) } -> std::same_as<void>;
    { c.pop_back() } -> std::same_as<void>;
  };

  static constexpr bool is_associative = requires(Container c, value_type v) {
    { c.insert(v) } -> std::same_as<std::pair<iterator, bool>>;
    { c.erase(v) } -> std::same_as<size_type>;
    { c.find(v) } -> std::same_as<iterator>;
  };

  static constexpr bool is_unordered = requires(Container c, value_type v) {
    { c.insert(v) } -> std::same_as<std::pair<iterator, bool>>;
    { c.erase(v) } -> std::same_as<size_type>;
    { c.find(v) } -> std::same_as<iterator>;
  };

  // ^^^ named requirements / not named vvv

  static constexpr bool is_reservable = requires(Container c, size_type n) {
    { c.reserve(n) } -> std::same_as<void>;
  };
};
} // namespace accat::auxilia
