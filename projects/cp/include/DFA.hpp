#pragma once

#include "Automaton.hpp"

#include <auxilia/defines.hpp>
#include <auxilia/auxfwd.hpp>

namespace accat::cp {
class NFA;
}
namespace accat::cp::details {
struct hopcroft_helper;
struct moore_helper;
} // namespace accat::cp::details

EXPORT_AUXILIA
namespace accat::cp {
/// @brief Deterministic Finite Automaton
///
/// A DFA can be represented by a 5-tuple (Q, Σ, δ, q0, F) where:
/// - Q: finite set of states:
///     @link details::AutomatonBase::states @endlink
/// - Σ: finite set of input symbols:
///     @link details::AutomatonBase::input_alphabet @endlink
/// - δ: Q × Σ → Q is the transition function, represented by
///     @link details::AutomatonBase::State::edges @endlink
/// - q0 ∈ Q is the start state:
///     @link details::AutomatonBase::start_id @endlink
/// - F ⊆ Q is the set of accept states:
///     @link details::AutomatonBase::accept_ids @endlink
class DFA : details::AutomatonBase {
  using MyBase = details::AutomatonBase;
  friend MyBase;
  friend details::hopcroft_helper;
  friend details::moore_helper;

  using IndexSetTy = std::unordered_set<size_t>;
  using IndexSetHasher =
      decltype([](const IndexSetTy &s) AC_STATIC_CALL_OPERATOR {
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
  auto _to_dot_impl(auxilia::FormatPolicy policy) const -> std::string;
  inline auto _dot_transition_details() const;
  inline void process_start_state(const NFA &nfa, Key2IdTy &key_to_id);
  inline void process_transitions(const NFA &nfa, Key2IdTy &key_to_id);
  inline void finalize(const NFA &nfa);
  inline void construct_from_nfa(const NFA &nfa);

public:
  static auxilia::StatusOr<DFA> FromNFA(const NFA &nfa);
  static auxilia::StatusOr<DFA> FromRegex(std::string_view sv);
  template <const auxilia::basic_chars_storage Option = "Hopcroft">
  DFA &minify() {
    dbg_only(const auto old_state_count = this->states.size();)

    if (!empty())
      _do_minify<Option.arr>();

    dbg_only(const auto new_state_count = this->states.size();)
    dbg(info,
        "DFA Minification: reduced states from {} to {}",
        old_state_count,
        new_state_count);

    return *this;
  }

private:
  using PartitionTy = std::unordered_set<size_t>;
  using PartitionsTy = std::vector<PartitionTy>;

  static auto initial_partition(const StatesTy &) -> PartitionsTy;
  void rebuild_from_partitions(const PartitionsTy &);
  /// Hopcroft algorithm (not Hopcroft-Karp algorithm)
  /// @ref https://en.wikipedia.org/wiki/DFA_minimization
  void hopcroft(PartitionsTy &) const;
  inline void moore(PartitionsTy &) const;

  template <const auto &Char> struct unknown_algorithm;
  template <const auto &Option> inline void _do_minify() {
    auto partitions = initial_partition(this->states);

    if constexpr (auxilia::as_chars(Option) == "Hopcroft")
      hopcroft(partitions);
    else if constexpr (auxilia::as_chars(Option) == "Moore")
      moore(partitions);
    else
      unknown_algorithm<Option>{};

    rebuild_from_partitions(partitions);
  }
};
} // namespace accat::cp
