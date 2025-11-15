#include <gtest/gtest.h>

#include "./test.env.inl.hpp"

#include <accat/auxilia/details/EBNF.hpp>
#include <iostream>
#include <string>
#include <utility>

constexpr auto arithmetic = R"~~(
E -> E+T | T
T -> T*F | F
F -> (E) | id
)~~";

constexpr auto arithmetic_expected = R"~~(
E -> T E'
T -> F T'
F -> ( E ) | id
E' -> + T E' | ε
T' -> * F T' | ε
)~~";

constexpr auto complex_expr = R"~~(
Expr -> Expr + Term | Expr - Term | Term
Term -> Term * Factor | Term / Factor | Factor
Factor -> (Expr) | num | id
)~~";

constexpr auto complex_expr_expected = R"~~(
Expr -> Term Expr'
Term -> Factor Term'
Factor -> ( Expr ) | num | id
Expr' -> + Term Expr' | - Term Expr' | ε
Term' -> * Factor Term' | / Factor Term' | ε
)~~";

constexpr auto list_grammar = R"~~(
List -> List , Element | Element
Element -> a | b | c
)~~";

constexpr auto list_grammar_expected = R"~~(
List -> Element List'
Element -> a | b | c
List' -> , Element List' | ε
)~~";

constexpr auto mixed_ops = R"~~(
E -> E == T | E != T | T
T -> T < F | T > F | F
F -> (E) | id | num
)~~";

constexpr auto mixed_ops_expected = R"~~(
E -> T E'
T -> F T'
F -> ( E ) | id | num
E' -> == T E' | != T E' | ε
T' -> < F T' | > F T' | ε
)~~";

constexpr auto multiple_recursion = R"~~(
S -> S a | S b | c | d
)~~";

constexpr auto multiple_recursion_expected = R"~~(
S -> c S' | d S'
S' -> a S' | b S' | ε
)~~";

using namespace accat::auxilia;
auto getStr(auto &&str) -> std::string {
  auto tokens = Lexer(str).lexAll_or_error();
  if (!tokens)
    return tokens.error();

  auto grammar = Grammar::parse(*std::move(tokens));
  if (!grammar)
    return grammar.raw_message();

  auto ok = grammar->eliminate_left_recursion();
  if (!ok)
    return ok.raw_message();

  return grammar->to_string();
}
#include <accat/auxilia/details/views.hpp>
using ranges::views::trim;
TEST(EBNF, lr) {
  set_console_output_cp_utf8();

  EXPECT_EQ(trim(getStr(arithmetic)), trim(arithmetic_expected));
  EXPECT_EQ(trim(getStr(complex_expr)), trim(complex_expr_expected));
  EXPECT_EQ(trim(getStr(list_grammar)), trim(list_grammar_expected));
  EXPECT_EQ(trim(getStr(mixed_ops)), trim(mixed_ops_expected));
  EXPECT_EQ(trim(getStr(multiple_recursion)),
            trim(multiple_recursion_expected));
}
