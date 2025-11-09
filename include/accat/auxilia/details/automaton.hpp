#pragma once

#include <algorithm>
#include <cstddef>
#include <functional>
#include <ranges>
#include <stack>
#include <type_traits>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <string>
#include <string_view>

#include "./StatusOr.hpp"

#include "./chars.hpp"
namespace accat::auxilia::details {
class AutomatonMixin : Printable {
protected:
  AutomatonMixin() noexcept = default;
  AutomatonMixin(const AutomatonMixin &) = delete;
  AutomatonMixin(AutomatonMixin &&) noexcept = default;
  AutomatonMixin &operator=(const AutomatonMixin &) = delete;
  AutomatonMixin &operator=(AutomatonMixin &&) noexcept = default;

  static constexpr auto npos = static_cast<size_t>(-1);
  static constexpr auto operators = as_chars("|*.+?()");
  static constexpr auto epsilon = "ε";
  static constexpr auto unformatted_header = raw(R"(
digraph {} {{
  rankdir=LR;
  node [shape=circle, fontsize=12];
  node [shape=point]; start;
  start -> {};
)");

  // we don't add const here for move-ctor/assignment,
  // but it's logically immutable once it is assigned.
  // also: const member field serves no purpose.
  struct Transition {
    /* const */ size_t target_id = npos;
    /* const */ char symbol = '\0';
    constexpr auto literal() const noexcept {
      return is_epsilon() ? epsilon : widen(symbol);
    }
    [[nodiscard]] bool is_epsilon() const noexcept { return symbol == '\0'; }
    auto to_string(const FormatPolicy policy = FormatPolicy::kDefault) const {
      if (policy == FormatPolicy::kDefault)
        return format("--{}->{}", literal(), target_id);
      else if (policy == FormatPolicy::kBrief)
        return format("{}: {}", literal(), target_id);
      else if (policy == FormatPolicy::kDetailed)
        return format(
            "Transition{{target_id={}, symbol={}}}", target_id, literal());
      AC_UNREACHABLE()
    }
  };

  // partial NFA, used during construction
  struct Fragment {
    /* const */ size_t start = npos;
    /* const */ size_t end = npos;
  };
  static_assert(std::conjunction_v<std::is_aggregate<Transition>,
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
    auto type_string() const {
      switch (type) {
      case Type::kIntermediate:
        return "";
      case Type::kStart:
        return "Start";
      case Type::kAccept:
        return "Accept";
      case Type::kStartAndAccept:
        return "Start & Accept";
      default:
        AC_UNREACHABLE("Unknown State Type: {}", (type))
      }
    }
    edges_t edges;
    State() noexcept = default;
    State(const State &) = delete;
    State(State &&) = default;
    State &operator=(const State &) = delete;
    State &operator=(State &&) = default;

    explicit State(const Type type) noexcept : type(type) {}
    auto to_string(const FormatPolicy policy = FormatPolicy::kDefault) const {
      std::string result;

      if (policy == FormatPolicy::kDetailed) {
        result += format("{{type={}, edges=[", type_string());
        for (const auto &e : edges)
          result += format("{} ,", e.to_string(policy));
        result += "]}";
        return result;
      }

      if (type == Type::kIntermediate)
        result += format("{}\n", type_string());

      if (policy == FormatPolicy::kDefault)
        for (const auto &e : edges)
          result += format("{}\n", e.to_string(policy));
      else if (policy == FormatPolicy::kBrief)
        result += format("[{} edges]", edges.size());

      return result;
    }
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

  static constexpr bool is_operator(const char c) {
    return operators.find(c) != npos;
  }
  static constexpr bool is_regex_operator(const char c) {
    return c != '(' && c != ')' && is_operator(c);
  }
  static constexpr size_t precedence(const char op) {
    switch (op) {
    case '*':
    case '+':
    case '?':
      return 3;
    case '.':
      return 2; // explicit concat
    case '|':
      return 1;
    default:
      return 0;
    }
  }
  static auto _dot_states(const StatesTy &states) {
    std::string dot;
    // mark start and accept distinctly
    for (const auto &[id, s] : states) {
      const auto isAccept = (s.type == State::Type::kAccept or
                             s.type == State::Type::kStartAndAccept);

      // doublecircle for accept states, single for others
      dot += format(R"~~~({0}[label="{0}\n{1}", shape={2}circle];)~~~"
                    " \n",
                    id,
                    s.type_string(),
                    isAccept ? "double" : "");
    }
    return dot;
  }

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
  void closure(std::unordered_set<size_t> &state_set, const char ch) const {

    // FIXME: poor use of vec
    std::vector stack(state_set.begin(), state_set.end());
    while (!stack.empty()) {
      size_t current = stack.back();
      stack.pop_back();
      for (const auto &edge : states.at(current).edges) {
        if (edge.symbol == ch && state_set.insert(edge.target_id).second) {
          // second is true if insertion took place
          stack.emplace_back(edge.target_id);
        }
      }
    }
  }
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

  Fragment from_char(const char c) {
    const auto s = new_state(State::Type::kIntermediate);
    const auto a = new_state(State::Type::kIntermediate);
    add_transition(s, a, c);
    return {s, a};
  }

  Fragment concat(Fragment &&lhs, Fragment &&rhs) {
    add_transition(lhs.end, rhs.start, '\0');
    return {lhs.start, rhs.end};
  }

  Fragment union_operation(Fragment &&a, Fragment &&b) {
    const auto s = new_state(State::Type::kIntermediate);
    const auto acc = new_state(State::Type::kIntermediate);

    add_transition(s, a.start, '\0');
    add_transition(s, b.start, '\0');

    add_transition(a.end, acc, '\0');
    add_transition(b.end, acc, '\0');
    return {s, acc};
  }

  Fragment kleene_star(Fragment &&f) {
    const auto s = new_state(State::Type::kIntermediate);
    const auto acc = new_state(State::Type::kIntermediate);

    add_transition(s, f.start, '\0');
    add_transition(s, acc, '\0');

    add_transition(f.end, f.start, '\0');
    add_transition(f.end, acc, '\0');
    return {s, acc};
  }
  [[nodiscard]] auto _dot_transitions() const -> std::string {
    using literals::operator""_raw;
    std::string dot;
    for (const auto &[id, s] : states) {
      for (const auto &e : s.edges) {
        dot += format(R"(  {} -> {} [label="{}"];)"
                      "\n",
                      id,
                      e.target_id,
                      e.literal());
      }
    }
    return dot;
  }
  bool test(std::string_view input) {
    if (empty())
      return input.empty();

    if (!input_alphabet.empty() &&
        std::ranges::any_of(input, std::not_fn([this](const char c) {
                              return (input_alphabet.contains(c) &&
                                      !is_operator(c));
                            })))
      return false;

    auto current_states = epsilon_closure(start_id);

    for (const char c : input) {
      decltype(current_states) next_states;
      for (const auto state_id : current_states)
        for (const auto &edge : states[state_id].edges)
          if (!edge.is_epsilon() && edge.symbol == c)
            next_states.insert(edge.target_id);

      if (next_states.empty())
        return false;
      epsilon_closure(next_states);
      current_states = std::move(next_states);
    }

    return std::ranges::any_of(accept_ids, [&](const size_t accept_id) {
      return current_states.contains(accept_id);
    });
  }
  auto to_dot(this const auto &self,
              const FormatPolicy policy = FormatPolicy::kDefault)
      -> std::string {
    if (self.empty())
      return raw(R"(
digraph Automaton {
  // empty
}
                )");

    return self._to_dot_impl(policy);
  }
  [[nodiscard]] auto
  to_string(const FormatPolicy policy = FormatPolicy::kDefault) const
      -> std::string {
    if (empty())
      return "Automaton <empty>";

#ifdef _WIN32
    SetConsoleOutputCP(65001);
#endif

    std::string result = "Automaton: [\n";
    for (const auto &[id, s] : states)
      result += format("State {}: {}\n", id, s.to_string(policy));

    result.replace(result.end() - 1, result.end(), "]");
    result += "\n";
    return result;
  }
};
} // namespace accat::auxilia::details

namespace accat::auxilia {
class NFA : details::AutomatonMixin {
  using MyBase = details::AutomatonMixin;
  friend MyBase;
  friend class DFA;

public:
  using MyBase::test;
  using MyBase::to_dot;
  using MyBase::to_string;

private:
  static std::string preprocess_regex(const std::string_view regex) {
    std::string result;
    // insert `.` for concatenation between
    //        char or `)` `*` `+` `?` followed by char or `(`
    // (here char means non-op char ^^^)
    result.resize_and_overwrite(
        regex.size() * 2,
        [&](char *const out, [[maybe_unused]] const size_t capacity) {
          constexpr auto is_in = [](const char c,
                                    const std::string_view chars) {
            return chars.find(c) != npos;
          };
          auto j = 0ull;
          for (auto i = 0ull; i < regex.size(); ++i) {
            const char c = regex[i];
            out[j++] = c;

            if (i + 1 < regex.size() && !is_in(c, "|(") &&
                !is_in(regex[i + 1], "|)*+?"))
              out[j++] = '.';
          }
          return j; // final length
        });
    return result;
  }

  // Shunting-Yard Algorithm, ref:
  // https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail
  // I choose not to use recursive descent here,
  // for I'm used it too often
  static StatusOr<std::string> to_postfix(const std::string_view regex) {
    std::string postfix;
    std::string op_stack;
    auto lvl = 0u; // workaround, detect parentheses level
    for (const auto c : regex) {
      if (c == '(') {
        op_stack.push_back(c);
        ++lvl;
        continue;
      }
      if (c == ')') {
        while (!op_stack.empty() && op_stack.back() != '(') {
          postfix += op_stack.back();
          op_stack.pop_back();
        }
        if (op_stack.empty()) {
          return InvalidArgumentError(
              "Unfinished parentheses in regex: missing '('");
        }
        if (op_stack.back() != '(') {
          return InvalidArgumentError(
              "Mismatched parentheses in regex: expected '(', got '{}'",
              op_stack.back());
        }
        op_stack.pop_back(); // pop '('
        --lvl;
        continue;
      }
      if (is_regex_operator(c)) {
        while (!op_stack.empty() && op_stack.back() != '(' &&
               precedence(op_stack.back()) >= precedence(c)) {
          postfix += op_stack.back();
          op_stack.pop_back();
        }
        op_stack.push_back(c);
        continue;
      }

      // normal character
      postfix += c;
    }
    if (lvl != 0) {
      return InvalidArgumentError(
          "Unfinished parentheses in regex: missing ')'");
    }

    // while (!op_stack.empty()) {
    //   postfix += op_stack.back();
    //   op_stack.pop_back();
    // }
    // ^^^ native way
    // vvv failed and non-conforming
    // postfix += std::ranges::reverse(op_stack)._Unwrapped();

    // worked, and fancy :P vvv
    std::ranges::reverse_copy(op_stack, std::back_inserter(postfix));

    return {postfix};
  }
  auto _to_dot_impl(const FormatPolicy) const -> std::string {
    auto dot = format(unformatted_header, "NFA", start_id);

    dot += _dot_states(states);

    dot += "\n";
    dot += _dot_transitions();
    dot += "}\n";
    return dot;
  }

  void init_input_alphabet(const std::string_view sv) {
    input_alphabet =
        sv | std::views::filter([](const char c) { return !is_operator(c); }) |
        std::ranges::to<std::string>();
    std::ranges::sort(input_alphabet);

    auto r = std::ranges::unique(input_alphabet);
    input_alphabet.erase(r.begin(), r.end());
  }
  StatusOr<Fragment> build_graph(const std::string_view postfix) {
    std::stack<Fragment> stack;
    auto top_and_pop_stack = [&stack]() {
      AC_RUNTIME_ASSERT(!stack.empty(), "Stack underflow")
      const auto val = stack.top();
      stack.pop();
      return val;
    };
    for (char c : postfix) {
      if (!is_operator(c)) {
        // regular character
        stack.emplace(from_char(c));
        continue;
      }
      if (c == '|') {
        if (stack.size() < 2) {
          return InvalidArgumentError(
              "Invalid regex at position {}: not enough operands for |",
              postfix.find(c));
        }
        auto b = top_and_pop_stack();
        auto a = top_and_pop_stack();
        stack.emplace(union_operation(std::move(a), std::move(b)));
        continue;
      }
      if (c == '.') {
        if (stack.size() < 2) {
          return InvalidArgumentError("Invalid regex at position {}: not "
                                      "enough operands for concatenation",
                                      postfix.find(c));
        }
        auto b = top_and_pop_stack();
        auto a = top_and_pop_stack();
        stack.emplace(concat(std::move(a), std::move(b)));
        continue;
      }
      if (c == '*') {
        if (stack.empty()) {
          return InvalidArgumentError(
              "Invalid regex at position {}: not enough operands for *",
              postfix.find(c));
        }
        auto f = top_and_pop_stack();
        stack.emplace(kleene_star(std::move(f)));
        continue;
      }
      if (c == '+') {
        // a+ = aa*
        if (stack.empty()) {
          return InvalidArgumentError(
              "Invalid regex at position {}: not enough operands for +",
              postfix.find(c));
        }
        auto f = top_and_pop_stack();
        auto f_copy = // reuse same states
            Fragment{.start = f.start, .end = f.end};
        auto star = kleene_star(std::move(f_copy));
        stack.emplace(concat(std::move(f), std::move(star)));
        continue;
      }
      if (c == '?') {
        // a? = (a|ε)
        if (stack.empty()) {
          return InvalidArgumentError(
              "Invalid regex at position {}: not enough operands for ?",
              postfix.find(c));
        }
        auto [fStart, fEnd] = top_and_pop_stack();
        auto s = new_state(State::Type::kIntermediate);
        auto acc = new_state(State::Type::kIntermediate);
        add_transition(s, fStart, '\0');
        add_transition(s, acc, '\0');
        add_transition(fEnd, acc, '\0');
        stack.emplace(s, acc);
        continue;
      }
      // stack.emplace(from_char(c)); // fallback
      AC_UNREACHABLE("Unimplemented operator in regex: '{}'", c);
      return UnimplementedError("Unhandled character in postfix regex: '{}'",
                                c);
    }
    if (stack.size() != 1) {
      AC_UNREACHABLE(
          "Internal Error: expression not fully reduced, stack size {}",
          stack.size());
      return InternalError("Internal Error: expression not fully reduced");
    }
    return {top_and_pop_stack()};
  }
  void finalize(Fragment &&frag) {
    const auto &[sid, eid] = frag;
    start_id = sid;
    accept_ids.emplace(eid);
    states[start_id].type = State::Type::kStart;
    // only one accept state in this algo
    states[eid].type = State::Type::kAccept;
  }
  auto construxt_from_regex(const std::string_view sv) {
    init_input_alphabet(sv);

    const auto preprocessed = preprocess_regex(sv);
    auto maybe_postfix = to_postfix(preprocessed);
    if (!maybe_postfix)
      return maybe_postfix.as_status();

    auto maybe_frag = build_graph(*maybe_postfix);
    if (!maybe_frag)
      return maybe_frag.as_status();

    finalize(std::move(*maybe_frag));

    return OkStatus();
  }

public:
  // McNaughton-Yamada-Thompson algorithm
  static StatusOr<NFA> FromRegex(const std::string_view sv) {
    if (sv.empty())
      return {};

    NFA nfa;

    auto s = nfa.construxt_from_regex(sv);

    if (!s)
      return s;
    return {std::move(nfa)};
  }
};
class DFA : details::AutomatonMixin {
  using IndexSetTy = std::unordered_set<size_t>;
  using MyBase = details::AutomatonMixin;
  friend MyBase;

  std::unordered_map<size_t, IndexSetTy> mapping;

  using IndexSetHasher = decltype([](const IndexSetTy &s) {
    auto h = 0ull;
    for (const auto v : s)
      h ^= std::hash<size_t>{}(v) + hash_magic_number_64bit + (h << 6) +
           (h >> 2);
    return h;
  });

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
  auto _dot_transition_details() const {
    std::string dot;
    for (const auto &[dfa_id, nfa_states] : mapping) {
      std::string nfa_states_str;
      for (const auto nfa_state_id : nfa_states) {
        nfa_states_str += format("{}, ", nfa_state_id);
      }
      if (!nfa_states_str.empty()) {
        nfa_states_str.erase(nfa_states_str.size() - 2); // remove last ", "
      }
      dot += format("{0} [label=\"{0}\n"
                    "({1})\"];  \n",
                    dfa_id,
                    nfa_states_str);
    }
    return dot;
  }
  auto _to_dot_impl(const FormatPolicy policy) const -> std::string {
    auto dot = format(unformatted_header, "DFA", start_id);
    dot += _dot_states(states);

    // not only output, but also add notation for the NFA states represented
    if (policy == FormatPolicy::kDetailed)
      dot += _dot_transition_details();

    dot += "\n";
    dot += _dot_transitions();
    dot += "}\n";
    return dot;
  }

  void process_start_state(const NFA &nfa, auto &&key_to_id) {
    auto start_subset = nfa.epsilon_closure(nfa.start_id);

    start_id = 0;
    states.emplace(0, State::Type::kStart);
    key_to_id.emplace(start_subset, 0);

    mapping.emplace(0, std::move(start_subset));
  }

  void process_transitions(const NFA &nfa, auto &key_to_id) {
    for (size_t cur = 0; cur < mapping.size(); ++cur) {
      for (const char a : input_alphabet) {
        std::unordered_set<size_t> move_set;
        for (const auto s : mapping[cur]) {
          auto it = nfa.states.find(s);
          AC_RUNTIME_ASSERT(
              it != nfa.states.end(),
              "Internal Error: NFA state {} not found during DFA construction",
              s)
          for (const auto &e : it->second.edges) {
            if (/* !e.is_epsilon() && */ e.symbol == a)
              move_set.insert(e.target_id);
          }
        }
        if (move_set.empty())
          continue;

        nfa.epsilon_closure(move_set);
        size_t to_id;
        if (auto [it, inserted] = key_to_id.emplace(move_set, npos); inserted) {
          to_id = new_state(State::Type::kIntermediate);
          it->second = to_id;
          mapping.emplace(to_id, std::move(move_set));
        } else {
          to_id = it->second;
        }
        add_transition(cur, to_id, a);
      }
    }
  }

  void finalize(const NFA &nfa) {
    for (auto dfa_id = 0ull; dfa_id < mapping.size(); ++dfa_id) {
      if (std::ranges::none_of(nfa.accept_ids, [&](const size_t nfa_accept) {
            return mapping[dfa_id].contains(nfa_accept);
          }))
        continue;

      accept_ids.emplace(dfa_id);
      states[dfa_id].type =
          (dfa_id == start_id)
              ? State::Type::kStartAndAccept // both start and accept
              : State::Type::kAccept;
    }
  }

  void construct_from_nfa(const NFA &nfa) {
    using Key2IdTy = std::unordered_map<IndexSetTy, size_t, IndexSetHasher>;

    Key2IdTy key_to_id;

    process_start_state(nfa, key_to_id);
    process_transitions(nfa, key_to_id);
    finalize(nfa);

    AC_DEBUG_ONLY(
        // check determinism(each edge symbol unique per state)
        for (const auto &s : states | std::views::values) {
          std::unordered_set<char> seen;
          for (const auto &[to_id, symbol] : s.edges) {
            AC_RUNTIME_ASSERT(
                !seen.contains(symbol),
                "Non-deterministic DFA found! Should not happen.");
            seen.insert(symbol);
          }
        })
  }

public:
  static StatusOr<DFA> FromNFA(const NFA &nfa) {
    DFA dfa;
    dfa.input_alphabet = nfa.input_alphabet;

    if (nfa.empty())
      return {std::move(dfa)};

    dfa.construct_from_nfa(nfa);

    return {std::move(dfa)};
  }

private:
  using PartitionTy = std::unordered_set<size_t>;
  using PartitionsTy = std::vector<PartitionTy>;

  static auto initial_partition(const StatesTy &states) {
    PartitionsTy partitions;
    PartitionTy accept_states, non_accept_states;

    for (const auto &[sid, state] : states) {
      if (state.type == State::Type::kAccept)
        accept_states.emplace(sid);
      else
        non_accept_states.emplace(sid);
    }
    AC_RUNTIME_ASSERT(!accept_states.empty(), "");
    partitions.emplace_back(std::move(accept_states));

    if (!non_accept_states.empty())
      partitions.emplace_back(std::move(non_accept_states));

    return partitions;
  }

  auto rebuild_from_partitions(const PartitionsTy &partitions) {
    // mapping: old state -> partition it belongs to
    std::unordered_map<size_t, size_t> oldstate_to_partition;
    for (auto i = 0ull; i < partitions.size(); ++i) {
      for (const auto sid : partitions[i]) {
        oldstate_to_partition[sid] = i;
      }
    }

    StatesTy new_states;
    std::unordered_set<size_t> new_accept_ids;
    auto new_start_id = npos;

    for (size_t pid = 0; pid < partitions.size(); ++pid) {
      const auto &part = partitions[pid];
      // randomly choose one
      const auto chosen_id = *part.begin();
      const auto &chosen_state = states[chosen_id];

      const auto isKeyContained = [&part](auto &&keyval) constexpr {
        return part.contains(keyval);
      };

      State newState; // id is pid in new states

      AC_DEBUG_ONLY(auto oneshot = false;)
      if (isKeyContained(start_id)) {
        AC_DEBUG_ONLY(AC_RUNTIME_ASSERT(oneshot == false,
                                        "should only have one start state")
                          oneshot = true);
        new_start_id = pid;
        newState.type = chosen_state.type;
      }

      if (std::ranges::any_of(accept_ids, isKeyContained)) {
        new_accept_ids.emplace(pid);
        if (pid == new_start_id) {
          newState.type = State::Type::kStartAndAccept;
        } else {
          newState.type = State::Type::kAccept;
        }
      } else if (pid == new_start_id) {
        newState.type = State::Type::kStart;
      }

      std::unordered_map<char, size_t> symbol_to_target;
      for (const auto edge : chosen_state.edges) {
        // target partition id is the new target state id.
        const auto target_partition_id = oldstate_to_partition[edge.target_id];
        newState.edges.emplace(target_partition_id, edge.symbol);
      }

      new_states.emplace(pid, std::move(newState));
    }

    states = std::move(new_states);
    start_id = new_start_id;
    accept_ids = std::move(new_accept_ids);
  }
  auto split(const PartitionsTy &partitions, const PartitionTy &part) {
    std::unordered_map<IndexSetTy, PartitionTy, IndexSetHasher> groups;

    for (const auto sid : part) {
      IndexSetTy dest_set;
      dest_set.reserve(input_alphabet.size());

      for (const auto symbol : input_alphabet) {
        // from state sid, a `symbol` move
        auto target_id = npos;

        // state: states.find(sid)->second;
        for (const auto edge : states.find(sid)->second.edges) {
          if (edge.symbol == symbol) {
            target_id = edge.target_id;
            break;
          }
        }

        if (target_id == npos)
          continue; // didn't have the transition `symbol`

        auto target_partition_idx = npos;

        for (auto pid = 0ull; pid < partitions.size(); ++pid) {
          if (partitions[pid].contains(target_id)) {
            target_partition_idx = pid;
            break;
          }
        }

        AC_RUNTIME_ASSERT(target_partition_idx != npos, "should not happen")

        dest_set.insert(target_partition_idx);
      }

      // Add this state to the group with this signature
      groups[dest_set].insert(sid);
    }
    return groups;
  }
  auto _do_minify() {

    auto partitions = initial_partition(this->states);

    bool changed;
    do {
      changed = false;
      decltype(partitions) new_partitions;

      for (const auto &part : partitions) {
        if (part.size() == 1) {
          // can't split an atom set
          new_partitions.emplace_back(part);
          continue;
        }

        // mapping: states -> partition
        auto groups = split(partitions, part);

        // a part was splited, so it's changed
        if (groups.size() > 1)
          changed = true;

        // this part -> several parts or unchanged
        new_partitions.append_range(std::move(groups) | std::views::values);
      }

      partitions = std::move(new_partitions);
    } while (changed);

    rebuild_from_partitions(partitions);
  }

public:
  // Hopcroft algorithm (not Hopcroft–Karp algorithm)
  DFA &minify() {
    if (!empty())
      _do_minify();
    return *this;
  }
};
} // namespace accat::auxilia
