#pragma once

#include "Automaton.hpp"

#include <accat/auxilia/defines.hpp>
#include <accat/auxilia/auxfwd.hpp>

namespace accat::cp {
class NFA;
}

EXPORT_AUXILIA
namespace accat::cp {
class DFA : details::_automaton_base {
  using MyBase = details::_automaton_base;
  friend MyBase;

  using IndexSetTy = std::unordered_set<size_t>;
  using IndexSetHasher = decltype([](const IndexSetTy &s) {
    auto h = 0ull;
    for (const auto v : s)
      h ^= std::hash<size_t>{}(v) + auxilia::hash_magic_number_64bit +
           (h << 6) + (h >> 2);
    return h;
  });
  using Key2IdTy = std::unordered_map<IndexSetTy, size_t, IndexSetHasher>;

  std::unordered_map<size_t, IndexSetTy> mapping;

public:
  using MyBase::test;
  using MyBase::to_dot;
  using MyBase::to_string;

  DFA() noexcept = default;
  DFA(const DFA &) = delete;
  DFA(DFA &&) noexcept = default;
  DFA &operator=(const DFA &) = delete;
  DFA &operator=(DFA &&) noexcept = default;

private:
  auto _dot_transition_details() const;
  auto _to_dot_impl(auxilia::FormatPolicy policy) const -> std::string;
  void process_start_state(const NFA &nfa, Key2IdTy &key_to_id);
  void process_transitions(const NFA &nfa, Key2IdTy &key_to_id);
  void finalize(const NFA &nfa);
  void construct_from_nfa(const NFA &nfa);

public:
  static auxilia::StatusOr<DFA> FromNFA(const NFA &nfa);
  static auxilia::StatusOr<DFA> FromRegex(std::string_view sv);
  template <const accat::auxilia::basic_chars_storage Option = "Hopcroft">
  DFA &minify() {
    if (!empty())
      _do_minify<Option.arr>();
    return *this;
  }

private:
  using PartitionTy = std::unordered_set<size_t>;
  using PartitionsTy = std::vector<PartitionTy>;

  static auto initial_partition(const StatesTy &states) -> PartitionsTy;
  void rebuild_from_partitions(const PartitionsTy &partitions);
  /// Hopcroft algorithm (not Hopcroft-Karp algorithm)
  /// @ref https://en.wikipedia.org/wiki/DFA_minimization
  void hopcroft(PartitionsTy &partitions) const;

  IndexSetTy get_transition_signature(const PartitionsTy &partitions,
                                      const State &s) const;

  // splits a given partition (`part`) into smaller groups
  //        based on the transitions of states in the partition.
  // states with identical transition behavior
  //      (signature) will remain in the same group.
  auto split(const PartitionsTy &partitions, const PartitionTy &part) const;
  void moore(PartitionsTy &partitions) const;

  template <const auto &Char> struct unknown_algorithm;
  template <const auto &Option> void _do_minify() {
    AC_DEBUG_ONLY(const auto old_state_count = this->states.size();)

    using auxilia::as_chars;

    auto partitions = initial_partition(this->states);

    if constexpr (as_chars(Option) == "Hopcroft")
      hopcroft(partitions);
    else if constexpr (as_chars(Option) == "Moore")
      moore(partitions);
    else
      unknown_algorithm<Option>{};

    rebuild_from_partitions(partitions);

    AC_DEBUG_ONLY(
        const auto new_state_count = this->states.size();
        AC_DEBUG_LOGGING(info,
                         "DFA Minification: reduced states from {} to {}",
                         old_state_count,
                         new_state_count);)
  }
};
} // namespace accat::cp
