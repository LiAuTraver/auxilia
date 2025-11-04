#pragma once

#include <stack>

#include "./StatusOr.hpp"

namespace accat::auxilia {
class NFA : Printable {
public:
  using size_type = size_t;
  using symbol_type = char;

  static constexpr auto npos = static_cast<size_type>(-1);
  static constexpr string_view operators = "|*.+?()";

  // should be private, workaround for trait is_nothrow_constructible
  NFA() noexcept = default;
  NFA(const NFA &) noexcept = delete;
  NFA &operator=(const NFA &) noexcept = delete;
  NFA(NFA &&) noexcept = default;
  NFA &operator=(NFA &&) noexcept = default;
  ~NFA() noexcept = default;


private:
  static consteval symbol_type widen(const char c) {
    return static_cast<symbol_type>(c);
  }
  static constexpr bool is_operator(const symbol_type c) {
    return operators.find(c) != string_view::npos;
  }
  struct State;
  struct Transition;
  struct Fragment;

  struct State {
    enum struct Type : unsigned short {
      kNone = 0, // intermediate state
      kStart = 1,
      kAccept = 2,
      kEmpty = kStart | kAccept // unused in current code
    } type = Type::kNone;
    using enum Type;
    size_type id = npos;
    std::vector<Transition> edges;
    State() noexcept = default;
    State(const Type type, const size_type id) noexcept : type(type), id(id) {}
  };
  struct Transition {
    size_type target_id = npos;
    symbol_type symbol = widen('\0');
    bool is_epsilon() const { return symbol == widen('\0'); }
    Transition() noexcept = default;
    Transition(const size_type target_id, const symbol_type symbol) noexcept
        : target_id(target_id), symbol(symbol) {}
  };

  // partial NFA, used during construction
  struct Fragment {
    size_type start = npos;
    size_type end = npos;
  };
  // empty input alphabet means all possible input symbols
  string input_alphabet;
  std::vector<State> states;
  size_type start_id = npos;
  // in Thompson's construction, only one accept state
  size_type accept_id = npos;

private:
  size_type new_state(State::Type type = State::kNone) {
    size_type id = states.size();
    states.emplace_back(type, id);
    return id;
  }
  void add_transition(const size_type from,
                      const size_type to,
                      const symbol_type symbol = '\0') {
    AC_PRECONDITION(from < states.size() && to < states.size(), "out of range")
    states[from].edges.emplace_back(to, symbol);
  }

  void epsilon_closure(std::unordered_set<size_type> &state_set) const {
    // FIXME: poor use of vec
    std::vector stack(state_set.begin(), state_set.end());
    while (!stack.empty()) {
      size_type current = stack.back();
      stack.pop_back();
      for (const auto &edge : states[current].edges) {
        if (edge.is_epsilon() && state_set.insert(edge.target_id).second) {
          // second is true if insertion took place
          stack.push_back(edge.target_id);
        }
      }
    }
  }

private:
  Fragment from_char(const symbol_type c) {
    size_type s = new_state(State::kNone);
    size_type a = new_state(State::kNone);
    add_transition(s, a, c);
    return {s, a};
  }

  Fragment concat(Fragment &&lhs, Fragment &&rhs) {
    add_transition(lhs.end, rhs.start, '\0');
    return {lhs.start, rhs.end};
  }

  Fragment union_operation(Fragment &&a, Fragment &&b) {
    size_type s = new_state(State::kNone);
    size_type acc = new_state(State::kNone);

    add_transition(s, a.start, '\0');
    add_transition(s, b.start, '\0');

    add_transition(a.end, acc, '\0');
    add_transition(b.end, acc, '\0');
    return {s, acc};
  }

  Fragment kleene_star(Fragment &&f) {
    size_type s = new_state(State::kNone);
    size_type acc = new_state(State::kNone);

    add_transition(s, f.start, '\0');
    add_transition(s, acc, '\0');

    add_transition(f.end, f.start, '\0');
    add_transition(f.end, acc, '\0');
    return {s, acc};
  }
  void finalize(Fragment &&frag) {
    AC_PRECONDITION(start_id == std::numeric_limits<size_type>::max() &&
                        accept_id == std::numeric_limits<size_type>::max(),
                    "NFA already finalized")
    AC_PRECONDITION(frag.start != npos && frag.end != npos,
                    "Invalid fragment to finalize;"
                    "if it's from empty regex, handle separately")
    start_id = frag.start;
    accept_id = frag.end;
    states[start_id].type = State::Type::kStart;
    states[accept_id].type = State::Type::kAccept;
  }

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
              char next = regex[i + 1];
              if (!is_in(c, "|(") && !is_in(next, "|)*+?")) {
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
    std::string postfix;
    std::string op_stack;
    constexpr auto is_operator = [](const symbol_type c) {
      return c == '|' || c == '*' || c == '.' || c == '+' || c == '?';
    };
    constexpr auto precedence = [](const symbol_type op) {
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
      if (is_operator(c)) {
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
  bool empty() const noexcept {
    return states.empty() && start_id == npos && accept_id == npos;
  }

public:
  auto to_string(FormatPolicy = FormatPolicy::kDefault) const -> string {
    if (empty())
      return "<empty>";

#ifdef _WIN32
    SetConsoleOutputCP(65001);
#endif

    string result;
    for (const auto &s : states) {
      result += format("State {}", s.id);
      if (s.type == State::Type::kStart)
        result += " (start)";
      if (s.type == State::Type::kAccept)
        result += " (accept)";
      result += ":\n";
      for (const auto &e : s.edges) {
        if (e.is_epsilon())
          result += format("  --ε--> {}\n", e.target_id);
        else
          result += format("  --{}--> {}\n", e.symbol, e.target_id);
      }
    }
    return result;
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

    std::unordered_set<size_type> current_states = {start_id};
    epsilon_closure(current_states);

    for (const char c : input) {
      std::unordered_set<size_type> next_states;
      for (size_type state_id : current_states) {
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

    return current_states.contains(accept_id);
  }
  // McNaughton-Yamada-Thompson algorithm
  static StatusOr<NFA> FromRegex(std::string_view sv) {
    NFA nfa;
    if (sv.empty()) {
      return {std::move(nfa)};
    }
    nfa.input_alphabet =
        sv | std::views::filter([](const char c) { return !is_operator(c); }) |
        std::ranges::to<string>();

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
        size_type s = nfa.new_state(State::kNone);
        size_type acc = nfa.new_state(State::kNone);
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

    nfa.finalize(std::move(stack.top()));

    return {std::move(nfa)};
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
    for (const auto &s : states) {
      // doublecircle for accept states, single for others
      dot +=
          format(R"({}[label="{}{}", shape={}];)"_raw
                 " \n",
                 s.id,
                 s.id,
                 (s.type == State::Type::kStart    ? R"(\n(start))"_raw
                  : s.type == State::Type::kAccept ? R"(\n(accept))"_raw
                                                   : ""),
                 (s.type == State::Type::kAccept) ? "doublecircle" : "circle");
    }

    dot += "\n";

    // transitions
    for (const auto &s : states) {
      for (const auto &e : s.edges) {
        dot += format(R"(  {} -> {} [label="{}"];)"_raw
                      "\n",
                      s.id,
                      e.target_id,
                      e.is_epsilon() ? "ε" : std::string(1, e.symbol));
      }
    }

    dot += "}\n";
    return dot;
  }
};
} // namespace accat::auxilia
