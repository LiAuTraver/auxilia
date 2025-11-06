#pragma once

#include <algorithm>
#include <functional>
#include <ranges>
#include <stack>
#include <type_traits>
#include <unordered_set>
#include <unordered_map>
#include <vector>

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
  fake_start [shape=none, label=""];
  fake_start -> {};
)");

  // we don't add const here for move-ctor/assignment,
  // but it's logically immutable once it is assigned.
  // also: const member field serves no purpose.
  struct Transition {
    /* const */ size_t target_id = npos;
    /* const */ char symbol = '\0';
    [[nodiscard]] bool is_epsilon() const noexcept { return symbol == '\0'; }
  };

  // partial NFA, used during construction
  struct Fragment {
    /* const */ size_t start = npos;
    /* const */ size_t end = npos;
  };
  static_assert(std::conjunction_v<std::is_aggregate<Transition>,
                                   std::is_aggregate<Fragment>>);
  struct State {
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
    enum struct Type : unsigned short {
      kNone = 0, // intermediate state
      kStart = 1,
      kAccept = 2,
      kEmpty = kStart | kAccept
    } type = Type::kNone;
    edges_t edges;
    State() noexcept = default;
    explicit State(const Type type) noexcept : type(type) {}
  };
  string input_alphabet;
  std::unordered_map<size_t, State> states;
  // conceptually we can have multiple start states,
  // but they are just elements in the same epsilon closure.
  size_t start_id = npos;
  // in Thompson's construction, only one accept state
  // size_t accept_id = npos;
  std::vector<size_t> accept_ids;

  static constexpr bool is_operator(const char c) {
    return operators.find(c) != string::npos;
  }

  size_t new_state(this auto &&self, State::Type type = State::Type::kNone) {
    auto id = self.states.size();
    self.states.emplace(id, type);
    return id;
  }
  void
  add_transition(const size_t from, const size_t to, const char symbol = '\0') {
    AC_PRECONDITION(from < states.size() && to < states.size(), "out of range")
    states[from].edges.emplace(to, symbol);
  }

  void epsilon_closure(std::unordered_set<size_t> &state_set) const {
    // FIXME: poor use of vec
    std::vector stack(state_set.begin(), state_set.end());
    while (!stack.empty()) {
      size_t current = stack.back();
      stack.pop_back();
      for (const auto &edge : states.at(current).edges) {
        if (edge.is_epsilon() && state_set.insert(edge.target_id).second) {
          // second is true if insertion took place
          stack.push_back(edge.target_id);
        }
      }
    }
  }
  [[nodiscard]] bool empty() const noexcept {
    return states.empty() && start_id == npos && accept_ids.empty();
  }

  Fragment from_char(const char c) {
    const auto s = new_state(State::Type::kNone);
    const auto a = new_state(State::Type::kNone);
    add_transition(s, a, c);
    return {s, a};
  }

  Fragment concat(Fragment &&lhs, Fragment &&rhs) {
    add_transition(lhs.end, rhs.start, '\0');
    return {lhs.start, rhs.end};
  }

  Fragment union_operation(Fragment &&a, Fragment &&b) {
    const auto s = new_state(State::Type::kNone);
    const auto acc = new_state(State::Type::kNone);

    add_transition(s, a.start, '\0');
    add_transition(s, b.start, '\0');

    add_transition(a.end, acc, '\0');
    add_transition(b.end, acc, '\0');
    return {s, acc};
  }

  Fragment kleene_star(Fragment &&f) {
    const auto s = new_state(State::Type::kNone);
    const auto acc = new_state(State::Type::kNone);

    add_transition(s, f.start, '\0');
    add_transition(s, acc, '\0');

    add_transition(f.end, f.start, '\0');
    add_transition(f.end, acc, '\0');
    return {s, acc};
  }
  [[nodiscard]] auto dot_transitions() const -> std::string {
    using literals::operator""_raw;
    std::string dot;
    for (const auto &[id, s] : states) {
      for (const auto &e : s.edges) {
        const char widen[] = {e.symbol, '\0'};
        dot += format(R"(  {} -> {} [label="{}"];)"_raw
                      "\n",
                      id,
                      e.target_id,
                      e.is_epsilon() ? epsilon : widen);
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
                            }))) {
      return false;
    }

    std::unordered_set current_states = {start_id};
    epsilon_closure(current_states);

    for (const char c : input) {
      std::unordered_set<size_t> next_states;
      for (size_t state_id : current_states) {
        for (const auto &edge : states[state_id].edges) {
          if (!edge.is_epsilon() && edge.symbol == c) {
            next_states.insert(edge.target_id);
          }
        }
      }
      if (next_states.empty())
        return false;
      epsilon_closure(next_states);
      current_states = std::move(next_states);
    }

    return std::ranges::any_of(accept_ids, [&](const size_t accept_id) {
      return current_states.contains(accept_id);
    });
  }
  auto to_dot(this const auto &self) -> string {
    if (self.empty())
      return raw(R"(
digraph Automaton {
  // empty
}
                )");

    return self.to_dot_impl();
  }
  [[nodiscard]] auto to_string(FormatPolicy = FormatPolicy::kDefault) const
      -> string {
    if (empty())
      return "<empty>";

#ifdef _WIN32
    SetConsoleOutputCP(65001);
#endif

    string result;
    for (const auto &[id, s] : states) {
      result += format("State {}", id);
      if (s.type == State::Type::kStart)
        result += " (start)";
      if (s.type == State::Type::kAccept)
        result += " (accept)";
      result += ":\n";
      for (const auto &e : s.edges) {
        if (e.is_epsilon())
          result += format("  --{}--> {}\n", epsilon, e.target_id);
        else
          result += format("  --{}--> {}\n", e.symbol, e.target_id);
      }
    }
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
          constexpr auto is_in = [](const char c, const string_view chars) {
            return chars.find(c) != string_view::npos;
          };
          auto j = 0ull;
          for (auto i = 0ull; i < regex.size(); ++i) {
            const char c = regex[i];
            out[j++] = c;

            if (i + 1 < regex.size()) {
              if (!is_in(c, "|(") && !is_in(regex[i + 1], "|)*+?")) {
                out[j++] = '.';
              }
            }
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
    // constexpr is for syntax highlighting, not useful here.
    constexpr auto is_regex_operator = [](const char c) {
      return c == '|' || c == '*' || c == '.' || c == '+' || c == '?';
    };
    // ditto
    constexpr auto precedence = [](const char op) {
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
    };
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
  auto to_dot_impl() const -> std::string {
    using literals::operator""_raw;

    auto dot = format(unformatted_header, "NFA", start_id);

    // mark start and accept distinctly
    for (const auto &[id, s] : states) {
      // doublecircle for accept states, single for others
      dot += format(
          R"({}[label="{}{}", shape={}];)"
          " \n",
          id,
          id,
          ((s.type == State::Type::kStart)
               ? R"(\n(start))"
               : ((s.type == State::Type::kAccept) ? R"(\n(accept))" : "")),
          (s.type == State::Type::kAccept) ? "doublecircle" : "circle");
    }

    dot += "\n";
    dot += dot_transitions();
    dot += "}\n";
    return dot;
  }

  void init_input_alphabet(const std::string_view sv) {
    input_alphabet =
        sv | std::views::filter([](const char c) { return !is_operator(c); }) |
        std::ranges::to<string>();
    std::ranges::sort(input_alphabet);

    auto r = std::ranges::unique(input_alphabet);
    input_alphabet.erase(r.begin(), r.end());
  }
  StatusOr<Fragment> build_graph(const string_view postfix) {
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
        auto s = new_state(State::Type::kNone);
        auto acc = new_state(State::Type::kNone);
        add_transition(s, fStart, '\0');
        add_transition(s, acc, '\0');
        add_transition(fEnd, acc, '\0');
        stack.emplace(s, acc);
        continue;
      }
      // stack.emplace(from_char(c)); // fallback
      DebugUnreachable("Unimplemented operator in regex: '{}'", c);
      return UnimplementedError("Unhandled character in postfix regex: '{}'",
                                c);
    }
    if (stack.size() != 1) {
      DebugUnreachable(
          "Internal Error: expression not fully reduced, stack size {}",
          stack.size());
      return InternalError("Internal Error: expression not fully reduced");
    }
    return {top_and_pop_stack()};
  }
  void finalize(Fragment &&frag) {
    const auto &[sid, eid] = frag;
    start_id = sid;
    accept_ids.emplace_back(eid);
    states[start_id].type = State::Type::kStart;
    // only one accept state in this algo
    states[eid].type = State::Type::kAccept;
  }

public:
  // McNaughton-Yamada-Thompson algorithm
  static StatusOr<NFA> FromRegex(const string_view sv) {
    if (sv.empty()) {
      return {};
    }
    NFA nfa;
    nfa.init_input_alphabet(sv);

    const auto preprocessed = preprocess_regex(sv);
    auto maybe_postfix = to_postfix(preprocessed);
    if (!maybe_postfix)
      return {maybe_postfix.as_status()};

    auto maybe_frag = nfa.build_graph(*maybe_postfix);
    if (!maybe_frag)
      return {maybe_frag.as_status()};

    nfa.finalize(std::move(*maybe_frag));

    return {std::move(nfa)};
  }
};
class DFA : details::AutomatonMixin {
  using SubSetTy = std::unordered_set<size_t>;
  using MyBase = details::AutomatonMixin;
  friend MyBase;

  std::unordered_map<size_t, SubSetTy> mapping;

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
  auto to_dot_impl() const -> std::string {
    // not only output, but also add notation for the NFA states represented
    using literals::operator""_raw;
    auto dot = format(unformatted_header, "DFA", start_id);
    // mark start and accept distinctly

    for (const auto &[id, s] : states) {
      dot += format(
          R"({0}[label="{0}{1}", shape={2}];)"
          " \n",
          id,
          ((s.type == State::Type::kStart)
               ? R"(\n(start))"
               : ((s.type == State::Type::kAccept) ? R"(\n(accept))" : "")),
          (s.type == State::Type::kAccept) ? "doublecircle" : "circle");
    }

    // add NFA states info, just numbers
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

    dot += "\n";
    dot += dot_transitions();
    dot += "}\n";
    return dot;
  }

  void process_start_state(const NFA &nfa, auto &&key_to_id) {
    std::unordered_set start_subset{nfa.start_id};
    nfa.epsilon_closure(start_subset);

    start_id = 0;
    states.emplace(0, State::Type::kStart);
    key_to_id.emplace(start_subset, 0);

    mapping.emplace(0, std::move(start_subset));
  }

  void process_transitions(const NFA &nfa, auto &&key_to_id) {
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
            if (!e.is_epsilon() && e.symbol == a) {
              move_set.insert(e.target_id);
            }
          }
        }
        if (move_set.empty())
          continue;

        nfa.epsilon_closure(move_set);
        size_t to_id;
        if (auto [it, inserted] = key_to_id.emplace(move_set, npos); inserted) {
          to_id = new_state(State::Type::kNone);
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

      accept_ids.push_back(dfa_id);
      states[dfa_id].type = (dfa_id == start_id)
                                ? State::Type::kEmpty // both start and accept
                                : State::Type::kAccept;
    }
  }

public:
  static StatusOr<DFA> FromNFA(const NFA &nfa) {
    DFA dfa;
    dfa.input_alphabet = nfa.input_alphabet;

    if (nfa.empty()) {
      return {std::move(dfa)};
    }

    using SubSetHashTy = decltype([](const SubSetTy &s) {
      auto h = 0ull;
      for (const auto v : s)
        h ^= std::hash<size_t>{}(v) + hash_magic_number_64bit + (h << 6) +
             (h >> 2);
      return h;
    });
    using Key2IdTy = std::unordered_map<SubSetTy, size_t, SubSetHashTy>;

    Key2IdTy key_to_id;

    dfa.process_start_state(nfa, key_to_id);
    dfa.process_transitions(nfa, std::move(key_to_id));
    dfa.finalize(nfa);

    AC_DEBUG_ONLY(
        // check determinism(each edge symbol unique per state)
        for (const auto &s : dfa.states | std::views::values) {
          std::unordered_set<char> seen;
          for (const auto &[to_id, symbol] : s.edges) {
            AC_RUNTIME_ASSERT(
                !seen.contains(symbol),
                "Non-deterministic DFA found! Should not happen.");
            seen.insert(symbol);
          }
        })

    return {std::move(dfa)};
  }
};
} // namespace accat::auxilia
