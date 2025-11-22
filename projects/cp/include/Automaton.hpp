#pragma once

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <accat/auxilia/auxilia.hpp>

using namespace accat::auxilia;
namespace accat::cp::details {
class AutomatonMixin : Printable {
protected:
  AutomatonMixin() noexcept = default;
  AutomatonMixin(AutomatonMixin &&) noexcept = default;
  AutomatonMixin &operator=(AutomatonMixin &&) noexcept = default;

  // we don't add const here for move-ctor/assignment,
  // but it's logically immutable once it is assigned.
  // also: const member field serves no purpose.
  struct Transition {
    /* const */ size_t target_id = npos;
    /* const */ char symbol = '\0';
    [[nodiscard]] bool is_epsilon() const noexcept { return symbol == '\0'; }
    string_type to_string(FormatPolicy policy = FormatPolicy::kDefault) const;
  };

  // partial NFA, used during construction
  struct Fragment {
    /* const */ size_t start = npos;
    /* const */ size_t end = npos;
  };
  AC_STATIC_ASSERT(std::conjunction_v<std::is_aggregate<Transition>,
                                      std::is_aggregate<Fragment>,
                                      std::is_trivially_copyable<Transition>,
                                      std::is_trivially_copyable<Fragment>>);
  struct State : Printable {
    using edges_t = std::unordered_set<
        Transition,
        decltype([](const Transition &t) {
          const auto h1 = std::hash<size_t>{}(t.target_id);
          const auto h2 = std::hash<char>{}(t.symbol);
          return h1 ^ (h2 + hash_magic_number_32bit + (h1 << 6) + (h1 >> 2));
        }),
        decltype([](const Transition &lhs, const Transition &rhs) {
          return lhs.target_id == rhs.target_id && lhs.symbol == rhs.symbol;
        })>;

    enum struct [[clang::flag_enum]] Type : unsigned short {
      kIntermediate = 0b00, // state neither start nor accept
      kStart = 0b01,
      kAccept = 0b10,
      kStartAndAccept = kStart | kAccept // 0b11
    } type = Type::kIntermediate;
    const char *type_string() const;
    edges_t edges;
    State() noexcept = default;
    State(const State &) = delete;
    State(State &&) noexcept = default;
    State &operator=(const State &) = delete;
    State &operator=(State &&) noexcept = default;

    explicit State(const Type type) noexcept : type(type) {}
    string_type to_string(FormatPolicy policy = FormatPolicy::kDefault) const;
  };
  std::string input_alphabet;
  std::unordered_map<size_t, State> states;
  // conceptually we can have multiple start states,
  // but they are just elements in the same epsilon closure.
  size_t start_id = npos;
  // in Thompson's construction, only one accept state
  // size_t accept_id = npos;
  std::unordered_set<size_t> accept_ids;

  using StatesTy = std::unordered_map<size_t, State>;

  static string_type _dot_states(const StatesTy &states);

  size_t new_state(this auto &&self,
                   State::Type type = State::Type::kIntermediate) {
    auto id = self.states.size();
    self.states.emplace(id, type);
    return id;
  }
  void
  add_transition(const size_t from, const size_t to, const char symbol = '\0') {
    AC_PRECONDITION(from < states.size() && to < states.size(), "out of range")
    states[from].edges.emplace(to, symbol);
  }
  void closure(std::unordered_set<size_t> &state_set, char ch) const;
  void epsilon_closure(std::unordered_set<size_t> &state_set) const {
    closure(state_set, '\0');
  }
  auto epsilon_closure(const size_t state_id) const {
    std::unordered_set state_set = {state_id};
    epsilon_closure(state_set);
    return state_set;
  }
  [[nodiscard]] bool empty() const noexcept {
    return states.empty() && start_id == npos && accept_ids.empty();
  }

  Fragment from_char(char c);
  Fragment concat(Fragment &&lhs, Fragment &&rhs);
  Fragment union_operation(Fragment &&a, Fragment &&b);
  Fragment kleene_star(Fragment &&f);
  [[nodiscard]] auto _dot_transitions() const -> std::string;
  bool test(std::string_view input);
  auto to_dot(this const auto &self,
              FormatPolicy policy = FormatPolicy::kDefault) -> std::string;
  [[nodiscard]] auto
  to_string(FormatPolicy policy = FormatPolicy::kDefault) const -> std::string;
};
auto AutomatonMixin::to_dot(this const auto &self, const FormatPolicy policy)
    -> std::string {
  if (self.empty())
    return raw(R"(
digraph Automaton {
  // empty
}
                )");

  return self._to_dot_impl(policy);
}
} // namespace accat::cp::details

namespace accat::cp {
EXPORT_AUXILIA
class NFA : details::AutomatonMixin {
  using MyBase = details::AutomatonMixin;
  friend MyBase;
  friend class DFA;

public:
  using MyBase::test;
  using MyBase::to_dot;
  using MyBase::to_string;

private:
  static std::string preprocess_regex(std::string_view regex);

  // Shunting-Yard Algorithm, ref:
  // https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail
  // I choose not to use recursive descent here,
  // for I'm used it too often
  static StatusOr<std::string> to_postfix(std::string_view regex);
  auto _to_dot_impl(FormatPolicy) const -> std::string;
  void init_input_alphabet(std::string_view sv);
  StatusOr<Fragment> build_graph(std::string_view postfix);
  void finalize(Fragment &&frag);
  auto construxt_from_regex(std::string_view sv);

public:
  // McNaughton-Yamada-Thompson algorithm
  static StatusOr<NFA> FromRegex(std::string_view sv);
};
EXPORT_AUXILIA
class DFA : details::AutomatonMixin {
  using MyBase = details::AutomatonMixin;
  friend MyBase;

  using IndexSetTy = std::unordered_set<size_t>;
  using IndexSetHasher = decltype([](const IndexSetTy &s) {
    auto h = 0ull;
    for (const auto v : s)
      h ^= std::hash<size_t>{}(v) + hash_magic_number_64bit + (h << 6) +
           (h >> 2);
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
  auto _to_dot_impl(FormatPolicy policy) const -> std::string;
  void process_start_state(const NFA &nfa, Key2IdTy &key_to_id);
  void process_transitions(const NFA &nfa, Key2IdTy &key_to_id);
  void finalize(const NFA &nfa);
  void construct_from_nfa(const NFA &nfa);

public:
  static StatusOr<DFA> FromNFA(const NFA &nfa);
  static StatusOr<DFA> FromRegex(const std::string_view sv) {
    return NFA::FromRegex(sv).and_then(
        [](auto &&nfa) { return DFA::FromNFA(nfa); });
  }
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
