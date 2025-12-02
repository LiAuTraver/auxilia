#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <ostream>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
#include <optional>

#include <accat/auxilia/defines.hpp>

#include "Lexing.hpp"
#include "Grammar.hpp"
#include "accat/auxilia/status/StatusOr.hpp"

using namespace accat::auxilia;
using namespace accat::cp;

constexpr auto flight = R"(
E -> E+T | T;
T -> T*F | F;
F -> (E) | id;
)";

constexpr auto factors = R"(
A -> A B C | A C B | a;
B -> B A C | B C A | b;
C -> C A B | C B A | c;
)";
constexpr auto flight2 = R"(
E -> E+T | T;
T -> T*F | F;
F -> E+F | id;
)";

constexpr auto simple = R"(
S -> ( X | E sq) | F );
X -> E ) | F sq);
E -> A;
F -> A;
A -> ε;
)";

extern const char *const sysY;
extern const char *const sysY_BNF;
AC_SPDLOG_INITIALIZATION("demo", debug)

int main() {
  set_console_output_cp_utf8();

  auto grammar = Grammar::FromStr(sysY_BNF);
  if (!grammar) {
    Println(stderr, "Error: {}", grammar.message());
    exit(1);
  }
  auto good = grammar->eliminate_left_recursion();
  if (!good) {
    Println(stderr, "Error: {}", good.message());
    exit(1);
  }
  grammar->apply_left_factorization();

  Out << grammar << "\n\n\n";

  Println("TERMINALS: \n{}", grammar->terminals());
  Println("NON-TERMINALS: \n{}", grammar->non_terminals_view());

  grammar->compute_first_set();
  grammar->compute_follow_set();

  auto &pieces = grammar->non_terminals();
  Println("FIRST: {}",
          pieces |
              std::ranges::views::transform(&Grammar::NonTerminal::first_set));
  Println("FOLLOW: {}",
          pieces |
              std::ranges::views::transform(&Grammar::NonTerminal::follow_set));
  Println("SELECT: {}",
          pieces |
              std::ranges::views::transform(&Grammar::NonTerminal::select_set));

  Out << "\n\n"
      << (grammar->isLL1() ? "this grammar is LL1."
                           : "this grammar is not LL1.")
      << "\n\n";

  auto res = grammar->parse(") ( ) sq) ( sq)");
  Out << (res ? "Parsing successful." : res.message());

  return 0;
}
// TODO: cannot handle `A ( B | C )`, it should be `A B | A C`.
#include "sysY.grammar.inl"

// Test EBNF constructs
constexpr auto ebnf_optional = R"(
A -> [ B ] C;
B -> b;
C -> c;
)";

constexpr auto ebnf_repetition = R"(
A -> { B } C;
B -> b;
C -> c;
)";

constexpr auto ebnf_complex = R"(
Expr -> Term { + Term };
Term -> Factor { * Factor };
Factor -> ( Expr ) | id;
)";

constexpr auto ebnf_epsilon_recursion = R"(
A -> ε | A B;
B -> b;
)";

void ebnf_test() {
  // Test EBNF support first
  Println("=== Testing EBNF Epsilon Recursion ===");
  auto ebnf_test = Grammar::FromStr(ebnf_epsilon_recursion);
  if (!ebnf_test) {
    Println(stderr, "EBNF Test Error: {}", ebnf_test.message());
  } else {
    Println("EBNF grammar parsed successfully!");
    Println("Before transformation:\n{}", ebnf_test);

    auto good = ebnf_test->eliminate_left_recursion();
    if (!good) {
      Println(stderr, "Error: {}", good.message());
    } else {
    }
  }

  Println("\n=== Testing EBNF Optional Construct [B] ===");
  auto ebnf_optional_test = Grammar::Process(ebnf_optional);
  if (!ebnf_optional_test) {
    Println(
        stderr, "EBNF Optional Test Error: {}", ebnf_optional_test.message());
  } else {
    Println("EBNF optional grammar processed successfully!");
    Println("Result:\n{}", ebnf_optional_test);
  }

  Println("\n=== Testing EBNF Repetition Construct {B} ===");
  auto ebnf_rep_test = Grammar::Process(ebnf_repetition);
  if (!ebnf_rep_test) {
    Println(stderr, "EBNF Repetition Test Error: {}", ebnf_rep_test.message());
  } else {
    Println("EBNF repetition grammar processed successfully!");
    Println("Result:\n{}", ebnf_rep_test);
  }

  Println("\n=== Testing EBNF Complex Example ===");
  auto ebnf_complex_test = Grammar::Process(ebnf_complex);
  if (!ebnf_complex_test) {
    Println(stderr, "EBNF Complex Test Error: {}", ebnf_complex_test.message());
  } else {
    Println("EBNF complex grammar processed successfully!");
    Println("Result:\n{}", ebnf_complex_test);
    Println("Is LL(1): {}", ebnf_complex_test->isLL1());
  }
}
