#include <gtest/gtest.h>

#include "./test.env.inl.hpp"

#include <accat/auxilia/details/Grammar.hpp>
#include <algorithm>
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
// also test different line here!
constexpr auto mixed_ops = R"~~(
E -> 
E == T | 
E != T | 
T
T
-> T < F 
| T > F 
| F
F -> 
(E) 
| 
id 
| 
num
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

  if (auto ok = grammar->eliminate_left_recursion(); !ok)
    return ok.raw_message();

  return grammar->to_string();
}
#include <accat/auxilia/details/views.hpp>
using ranges::views::trim;
TEST(Grammar, LeftRecursive) {
  set_console_output_cp_utf8();

  EXPECT_EQ(trim(getStr(arithmetic)), trim(arithmetic_expected));
  EXPECT_EQ(trim(getStr(complex_expr)), trim(complex_expr_expected));
  EXPECT_EQ(trim(getStr(list_grammar)), trim(list_grammar_expected));
  EXPECT_EQ(trim(getStr(mixed_ops)), trim(mixed_ops_expected));
  EXPECT_EQ(trim(getStr(multiple_recursion)),
            trim(multiple_recursion_expected));
}
constexpr auto multipleLeftArrow = R"~~(
A -> A -> B | B
)~~";
constexpr auto multipleLeftArrow_expected = R"~~(
line 2: Unexpected LeftArrow in rhs.
)~~";
TEST(Grammar, LeftRecursiveErrorHandling) {
  set_console_output_cp_utf8();

  EXPECT_EQ(trim(getStr(multipleLeftArrow)), trim(multipleLeftArrow_expected));
}
constexpr auto nested_left_recursion = R"~~(
S -> A | B
A -> A a B | A b C | d
B -> B c A | B d B | e  
C -> C f A | C g B | h
)~~";
constexpr auto nested_left_recursion_expected = R"~~(
S -> A | B
A -> d A'
B -> e B'
C -> h C'
A' -> a B A' | b C A' | ε
B' -> c A B' | d B B' | ε
C' -> f A C' | g B C' | ε
)~~";

constexpr auto multi_level_recursion = R"~~(
Expr -> Expr + Term | Expr - Term | Term
Term -> Term * Factor | Term / Factor | Factor  
Factor -> Factor Pow Primary | Primary
Pow -> ^
Primary -> ( Expr ) | id | num
)~~";
constexpr auto multi_level_recursion_expected = R"~~(
Expr -> Term Expr'
Term -> Factor Term'
Factor -> Primary Factor'
Pow -> ^
Primary -> ( Expr ) | id | num
Expr' -> + Term Expr' | - Term Expr' | ε
Term' -> * Factor Term' | / Factor Term' | ε
Factor' -> Pow Primary Factor' | ε
)~~";

constexpr auto crazy = R"~~(
A -> A B C | A C B | a
B -> B A C | B C A | b  
C -> C A B | C B A | c
)~~";
constexpr auto crazy_doomed = R"~~(
A -> a A'
B -> b B'
C -> c C'
A' -> B C A' | C B A' | ε
B' -> A C B' | C A B' | ε
C' -> A B C' | B A C' | ε
)~~";
TEST(Grammar, ComplexLeftRecursive) {
  set_console_output_cp_utf8();

  EXPECT_EQ(trim(getStr(nested_left_recursion)),
            trim(nested_left_recursion_expected));
  EXPECT_EQ(trim(getStr(multi_level_recursion)),
            trim(multi_level_recursion_expected));
  // failed: substitues too eagerly
  // A' -> b B' C A' | c C' B A' | ε
  EXPECT_EQ(trim(getStr(crazy)), trim(crazy_doomed));
}

constexpr auto simple = R"~~(
A -> b c d | b c e | b f
)~~";
constexpr auto simple_expected = R"~~(
A -> b A@@
A@ -> d | e
A@@ -> f | c A@
)~~";
constexpr auto multi = R"~~(
A -> b c D 
   | b c E 
   | b F
B -> x Y z | x Z
C -> p Q | p R | s T
)~~";
constexpr auto multi_expected = R"~~(
A -> b A@@
B -> x B@
C -> s T | p C@
A@ -> D | E
B@ -> Y z | Z
C@ -> Q | R
A@@ -> F | c A@
)~~";
constexpr auto complex = R"~~(
Expression -> Term + Expression | Term - Expression | Term
Term -> Factor * Term | Factor / Term | Factor
Factor -> ( Expression ) | Number | Identifier
)~~";
constexpr auto complex_expected = R"~~(
Expression -> Term Expression@
Term -> Factor Term@
Factor -> ( Expression ) | Number | Identifier
Expression@ -> ε | + Expression | - Expression
Term@ -> ε | * Term | / Term
)~~";
constexpr auto cond = R"~~(
Statement -> if ( Condition ) Statement | if ( Condition ) Statement else Statement
Condition -> ID == NUM | ID != NUM
)~~";
constexpr auto cond_expected = R"~~(
Statement -> if ( Condition ) Statement Statement@
Condition -> ID Condition@
Statement@ -> ε | else Statement
Condition@ -> == NUM | != NUM
)~~";
auto getLL1Str(auto &&str) {
  auto tokens = Lexer(str).lexAll_or_error();

  return tokens ? Grammar::ContextFree(*std::move(tokens))
                      .to_string(FormatPolicy::kBrief)
                : tokens.error();
}
TEST(Grammar, LeftFactoring) {
  set_console_output_cp_utf8();

  EXPECT_EQ(trim(getLL1Str(simple)), trim(simple_expected));
  EXPECT_EQ(trim(getLL1Str(multi)), trim(multi_expected));
  EXPECT_EQ(trim(getLL1Str(complex)), trim(complex_expected));
  EXPECT_EQ(trim(getLL1Str(cond)), trim(cond_expected));
}
constexpr auto simple_nullable = R"~~(
S -> A B
A -> a | ε
B -> b
)~~";
constexpr auto simple_nullable_first_expected = R"~~(
[{"a", "b"}, {"a", "ε"}, {"b"}]
)~~";
auto getFirstSet(auto &&str) {
  auto tokens = Lexer(str).lexAll_or_error();
  if (!tokens) {
    return tokens.error();
  }
  auto grammar = Grammar::ContextFree(*std::move(tokens));
  auto pieces = grammar->non_terminals();
  return Format("{}",
                std::move(pieces) | std::ranges::views::transform(
                                        &Grammar::NonTerminal::first_set));
}
TEST(Grammar, FirstSet) {
  EXPECT_EQ(trim(getFirstSet(simple_nullable)),
            trim(simple_nullable_first_expected));
}

constexpr auto simple_nullable_follow_expected = R"~~(
[{"$"}, {"b"}, {"$"}]
)~~";
auto getFollowSet(auto &&str) {
  auto tokens = Lexer(str).lexAll_or_error();
  if (!tokens) {
    return tokens.error();
  }
  auto grammar = Grammar::ContextFree(*std::move(tokens));
  auto pieces = grammar->non_terminals();
  return Format("{}",
                std::move(pieces) | std::ranges::views::transform(
                                        &Grammar::NonTerminal::follow_set));
}
TEST(Grammar, FollowSet) {
  EXPECT_EQ(trim(getFollowSet(simple_nullable)),
            trim(simple_nullable_follow_expected));
}
