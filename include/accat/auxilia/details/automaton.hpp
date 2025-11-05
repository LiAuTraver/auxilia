#pragma once

#include <algorithm>
#include <cstddef>
#include <functional>
#include <ranges>
#include <stack>
#include <unordered_set>
#include <unordered_map>
#include <vector>

#include "./StatusOr.hpp"

#include "./chars.hpp"
namespace accat::auxilia::details {
class AutomatonBase : Printable {
public:
  static constexpr auto npos = static_cast<size_t>(-1);
  static constexpr auto operators = as_chars("|*.+?()");
  static constexpr auto epsilon = "ε";

  static constexpr bool is_operator(const char c) {
    return operators.find(c) != string::npos;
  }

public:
  // should be private, workaround for trait is_nothrow_constructible
  AutomatonBase() noexcept = default;
  AutomatonBase(const AutomatonBase &) = delete;
  AutomatonBase &operator=(const AutomatonBase &) = delete;
  AutomatonBase(AutomatonBase &&) noexcept = default;
  AutomatonBase &operator=(AutomatonBase &&) noexcept = default;
  ~AutomatonBase() noexcept = default;

protected:
  struct Transition {
    size_t target_id = npos;
    char symbol = '\0';
    bool is_epsilon() const { return symbol == '\0'; }
  };
  struct State {
    using edges_t = std::unordered_set<
        Transition,
        decltype([](const Transition &t) {
          auto h1 = std::hash<size_t>{}(t.target_id);
          auto h2 = std::hash<char>{}(t.symbol);
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
    State(const Type type) noexcept : type(type) {}
  };

  // partial NFA, used during construction
  struct Fragment {
    size_t start = npos;
    size_t end = npos;
  };
  string input_alphabet;
  std::unordered_map<size_t, State> states;
  size_t start_id = npos;
  // in Thompson's construction, only one accept state
  // size_t accept_id = npos;
  std::vector<size_t> accept_ids;

protected:
  size_t new_state(State::Type type = State::Type::kNone) {
    size_t id = states.size();
    states.emplace(id, type);
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
  bool empty() const noexcept {
    return states.empty() && start_id == npos && accept_ids.empty();
  }

  Fragment from_char(const char c) {
    size_t s = new_state(State::Type::kNone);
    size_t a = new_state(State::Type::kNone);
    add_transition(s, a, c);
    return {s, a};
  }

  Fragment concat(Fragment &&lhs, Fragment &&rhs) {
    add_transition(lhs.end, rhs.start, '\0');
    return {lhs.start, rhs.end};
  }

  Fragment union_operation(Fragment &&a, Fragment &&b) {
    size_t s = new_state(State::Type::kNone);
    size_t acc = new_state(State::Type::kNone);

    add_transition(s, a.start, '\0');
    add_transition(s, b.start, '\0');

    add_transition(a.end, acc, '\0');
    add_transition(b.end, acc, '\0');
    return {s, acc};
  }

  Fragment kleene_star(Fragment &&f) {
    auto s = new_state(State::Type::kNone);
    auto acc = new_state(State::Type::kNone);

    add_transition(s, f.start, '\0');
    add_transition(s, acc, '\0');

    add_transition(f.end, f.start, '\0');
    add_transition(f.end, acc, '\0');
    return {s, acc};
  }

public:
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

    return std::ranges::any_of(accept_ids, [&](size_t accept_id) {
      return current_states.contains(accept_id);
    });
  }
  auto to_dot() const -> std::string {
    using literals::operator""_raw;
    if (empty())
      return R"(
digraph NFA {
  // empty
}
                )"_raw;

    static constexpr auto header = R"(
digraph NFA {{
  rankdir=LR;
  node [shape=circle, fontsize=12];
  fake_start [shape=none, label=""];
  fake_start -> {};
                                                        )"_raw;

    auto dot = format(header, start_id);

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
    // transitions
    for (const auto &[id, s] : states) {
      for (const auto &e : s.edges) {
        char widen[2] = {e.symbol, '\0'};
        dot += format(R"(  {} -> {} [label="{}"];)"_raw
                      "\n",
                      id,
                      e.target_id,
                      e.is_epsilon() ? epsilon : widen);
      }
    }

    dot += "}\n";
    return dot;
  }
  auto to_string(FormatPolicy = FormatPolicy::kDefault) const -> string {
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
class NFA : public details::AutomatonBase {

private:
  static std::string preprocess_regex(const std::string_view regex) {
    std::string result;
    // insert `.` for concatenation between
    //        char or `)` `*` `+` `?` followed by char or `(`
    // (here char means non-op char ^^^)
    result.resize_and_overwrite(
        regex.size() * 2, [&](char *out, size_t capacity) {
          constexpr auto is_in = [](const char c, const string_view chars) {
            return chars.find(c) != string_view::npos;
          };
          size_t j = 0;
          for (size_t i = 0; i < regex.size(); ++i) {
            char c = regex[i];
            out[j++] = c;

            if (i + 1 < regex.size()) {
              if (char next = regex[i + 1];
                  !is_in(c, "|(") && !is_in(next, "|)*+?")) {
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

public:
  // McNaughton-Yamada-Thompson algorithm
  static StatusOr<NFA> FromRegex(std::string_view sv) {
    if (sv.empty()) {
      return {};
    }
    NFA nfa;
    nfa.input_alphabet =
        sv | std::views::filter([](const char c) { return !is_operator(c); }) |
        std::ranges::to<string>();
    std::ranges::sort(nfa.input_alphabet);

    auto r = std::ranges::unique(nfa.input_alphabet);
    nfa.input_alphabet.erase(r.begin(), r.end());

    auto preprocessed = preprocess_regex(sv);
    auto maybe_postfix = to_postfix(preprocessed);
    if (!maybe_postfix) {
      return {maybe_postfix.as_status()};
    }
    auto postfix = *std::move(maybe_postfix);

    std::stack<Fragment> stack;
    auto top_and_pop_stack = [&stack]() {
      AC_RUNTIME_ASSERT(!stack.empty(), "Stack underflow")
      auto val = stack.top();
      stack.pop();
      return val;
    };

    for (char c : postfix) {
      if (!nfa.is_operator(c)) {
        // regular character
        stack.emplace(nfa.from_char(c));
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
        stack.emplace(nfa.union_operation(std::move(a), std::move(b)));
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
        stack.emplace(nfa.concat(std::move(a), std::move(b)));
        continue;
      }
      if (c == '*') {
        if (stack.empty()) {
          return InvalidArgumentError(
              "Invalid regex at position {}: not enough operands for *",
              postfix.find(c));
        }
        auto f = top_and_pop_stack();
        stack.emplace(nfa.kleene_star(std::move(f)));
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
        auto star = nfa.kleene_star(std::move(f_copy));
        stack.emplace(nfa.concat(std::move(f), std::move(star)));
        continue;
      }
      if (c == '?') {
        // a? = (a|ε)
        if (stack.empty()) {
          return InvalidArgumentError(
              "Invalid regex at position {}: not enough operands for ?",
              postfix.find(c));
        }
        auto f = top_and_pop_stack();
        auto s = nfa.new_state(State::Type::kNone);
        auto acc = nfa.new_state(State::Type::kNone);
        nfa.add_transition(s, f.start, '\0');
        nfa.add_transition(s, acc, '\0');
        nfa.add_transition(f.end, acc, '\0');
        stack.emplace(s, acc);
        continue;
      }
      // stack.emplace(nfa.from_char(c)); // fallback
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

    // finalize the graph
    const auto &[start_id, end_id] = stack.top();
    nfa.start_id = start_id;
    nfa.accept_ids.emplace_back(end_id);
    nfa.states[nfa.start_id].type = State::Type::kStart;
    // only one accept state in this algo
    nfa.states[nfa.accept_ids.back()].type = State::Type::kAccept;

    return {std::move(nfa)};
  }
  friend class DFA;
};
class DFA : public details::AutomatonBase {

public:
  static StatusOr<DFA> FromNFA(const NFA &nfa) {
    DFA dfa;
    dfa.input_alphabet = nfa.input_alphabet;

    if (nfa.empty()) {
      return {std::move(dfa)};
    }

    using SubSetTy = std::unordered_set<size_t>;
    using SubSetHashTy = decltype([](const SubSetTy &s) {
      auto h = 0ull;
      for (const auto v : s)
        h ^= std::hash<size_t>{}(v) + hash_magic_number_64bit + (h << 6) +
             (h >> 2);
      return h;
    });
    using Key2IdTy = std::unordered_map<SubSetTy, size_t, SubSetHashTy>;

    // subset -> DFA state id, and id -> subset
    Key2IdTy key_to_id;
    std::vector<SubSetTy> id_to_subset;

    std::unordered_set start_subset{nfa.start_id};
    nfa.epsilon_closure(start_subset);

    dfa.start_id = 0;
    dfa.states.emplace(0, State::Type::kStart);
    key_to_id.emplace(start_subset, 0);

    id_to_subset.emplace_back(std::move(start_subset));

    // BFS
    for (size_t cur = 0; cur < id_to_subset.size(); ++cur) {

      for (char a : dfa.input_alphabet) {
        std::unordered_set<size_t> move_set;
        for (auto s : id_to_subset[cur]) {
          auto it = nfa.states.find(s);
          if (it == nfa.states.end())
            continue;
          for (const auto &e : it->second.edges) {
            if (!e.is_epsilon() && e.symbol == a) {
              move_set.insert(e.target_id);
            }
          }
        }
        if (move_set.empty())
          continue;

        nfa.epsilon_closure(move_set);
        // Insert the subset key directly. If this is a new subset, create a new
        // DFA state.
        size_t to_id;
        if (auto [it, inserted] = key_to_id.emplace(move_set, npos); inserted) {
          to_id = dfa.new_state(State::Type::kNone);
          it->second = to_id;
          id_to_subset.push_back(std::move(move_set));
        } else {
          to_id = it->second;
        }
        dfa.add_transition(cur, to_id, a);
      }
    }

    for (auto dfa_id = 0ull; dfa_id < id_to_subset.size(); ++dfa_id) {
      const auto &nfa_subset = id_to_subset[dfa_id];

      // check if this subset contains any NFA accept state
      bool is_accept =
          std::ranges::any_of(nfa.accept_ids, [&](size_t nfa_accept) {
            return nfa_subset.contains(nfa_accept);
          });

      if (!is_accept)
        continue;

      dfa.accept_ids.push_back(dfa_id);
      dfa.states[dfa_id].type =
          (dfa_id == dfa.start_id)
              ? State::Type::kEmpty // both start and accept
              : State::Type::kAccept;
    }

    return {std::move(dfa)};
  }
  friend class NFA;
};
} // namespace accat::auxilia
