#include <gtest/gtest.h>

#include "./test.env.inl.hpp"

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iostream>
#include <ostream>
#include <ranges>
#include <string>
#include <utility>

#include "Grammar.hpp"

using namespace accat::auxilia;
using namespace accat::cp;

using ranges::views::trim;

namespace rv = std::ranges::views;
using NonTerminal = Grammar::NonTerminal;

#define EXPECT_TRIMMED_STR_EQ(str1, str2) EXPECT_EQ(trim(str1), trim(str2))
struct Results {
  const char *const str;
  const char *const answer;
};

constexpr auto arithmetic = Results{
    .str = R"(
E -> E+T | T
T -> T*F | F
F -> (E) | id
)",
    .answer = R"(
E -> T E'
T -> F T'
F -> ( E ) | id
E' -> + T E' | ε
T' -> * F T' | ε
)",
};

constexpr auto complex_expr = Results{
    .str = R"(
Expr -> Expr + Term | Expr - Term | Term
Term -> Term * Factor | Term / Factor | Factor
Factor -> (Expr) | num | id
)",
    .answer = R"(
Expr -> Term Expr'
Term -> Factor Term'
Factor -> ( Expr ) | num | id
Expr' -> + Term Expr' | - Term Expr' | ε
Term' -> * Factor Term' | / Factor Term' | ε
)",
};

constexpr auto list_grammar = Results{
    .str = R"(
List -> List , Element | Element
Element -> a | b | c
)",
    .answer = R"(
List -> Element List'
Element -> a | b | c
List' -> , Element List' | ε
)",
};

constexpr auto mixed_ops = Results{
    .str = R"(
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
)",
    .answer = R"(
E -> T E'
T -> F T'
F -> ( E ) | id | num
E' -> == T E' | != T E' | ε
T' -> < F T' | > F T' | ε
)",
};

constexpr auto multiple_recursion = Results{
    .str = R"(
S -> S a | S b | c | d
)",
    .answer = R"(
S -> c S' | d S'
S' -> a S' | b S' | ε
)",
};

using namespace accat::auxilia;
auto getStr(auto &&str) -> std::string {

  auto grammar = Grammar::Process(str);
  if (!grammar)
    return grammar.raw_message();

  return grammar->to_string();
}
TEST(Grammar, LeftRecursion) {
  set_console_output_cp_utf8();

  EXPECT_TRIMMED_STR_EQ(getStr(arithmetic.str), arithmetic.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(complex_expr.str), complex_expr.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(list_grammar.str), list_grammar.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(mixed_ops.str), mixed_ops.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(multiple_recursion.str),
                        multiple_recursion.answer);
}
constexpr auto multipleLeftArrow = R"(
A -> A -> B | B
)";
constexpr auto multipleLeftArrow_expected = R"(
line 2: Unexpected LeftArrow in rhs.
)";
TEST(Grammar, LeftRecursionErrorHandling) {
  set_console_output_cp_utf8();

  EXPECT_TRIMMED_STR_EQ(getStr(multipleLeftArrow), multipleLeftArrow_expected);
}
constexpr auto nested_left_recursion = Results{
    .str = R"(
S -> A | B
A -> A a B | A b C | d
B -> B c A | B d B | e  
C -> C f A | C g B | h
)",
    .answer = R"(
S -> A | B
A -> d A'
B -> e B'
C -> h C'
A' -> a B A' | b C A' | ε
B' -> c A B' | d B B' | ε
C' -> f A C' | g B C' | ε
)",
};

constexpr auto multi_level_recursion = Results{
    .str = R"(
Expr -> Expr + Term | Expr - Term | Term
Term -> Term * Factor | Term / Factor | Factor  
Factor -> Factor Pow Primary | Primary
Pow -> ^
Primary -> ( Expr ) | id | num
)",
    .answer = R"(
Expr -> Term Expr'
Term -> Factor Term'
Factor -> Primary Factor'
Pow -> ^
Primary -> ( Expr ) | id | num
Expr' -> + Term Expr' | - Term Expr' | ε
Term' -> * Factor Term' | / Factor Term' | ε
Factor' -> Pow Primary Factor' | ε
)",
};

constexpr auto crazy = Results{
    .str = R"(
A -> A B C | A C B | a
B -> B A C | B C A | b  
C -> C A B | C B A | c
)",
    .answer = R"(
A -> a A'
B -> b B'
C -> c C'
A' -> B C A' | C B A' | ε
B' -> A C B' | C A B' | ε
C' -> A B C' | B A C' | ε
)",
};

TEST(Grammar, ComplexLeftRecursion) {
  set_console_output_cp_utf8();

  EXPECT_TRIMMED_STR_EQ(getStr(nested_left_recursion.str),
                        nested_left_recursion.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(multi_level_recursion.str),
                        multi_level_recursion.answer);
  // failed: substitutes too eagerly
  // A' -> b B' C A' | c C' B A' | ε
  EXPECT_TRIMMED_STR_EQ(getStr(crazy.str), crazy.answer);
}

constexpr auto simple = Results{
    .str = R"(
A -> b c d | b c e | b f
)",
    .answer = R"(
A -> b A@@
A@ -> d | e
A@@ -> f | c A@
)",
};

constexpr auto multi = Results{
    .str = R"(
A -> b c D 
   | b c E 
   | b F
B -> x Y z | x Z
C -> p Q | p R | s T
)",
    .answer = R"(
A -> b A@@
B -> x B@
C -> s T | p C@
A@ -> D | E
B@ -> Y z | Z
C@ -> Q | R
A@@ -> F | c A@
)",
};

constexpr auto complex = Results{
    .str = R"(
Expression -> Term + Expression | Term - Expression | Term
Term -> Factor * Term | Factor / Term | Factor
Factor -> ( Expression ) | Number | Identifier
)",
    .answer = R"(
Expression -> Term Expression@
Term -> Factor Term@
Factor -> ( Expression ) | Number | Identifier
Expression@ -> ε | + Expression | - Expression
Term@ -> ε | * Term | / Term
)",
};

constexpr auto cond = Results{
    .str = R"(
Statement -> if ( Condition ) Statement | if ( Condition ) Statement else Statement
Condition -> ID == NUM | ID != NUM
)",
    .answer = R"(
Statement -> if ( Condition ) Statement Statement@
Condition -> ID Condition@
Statement@ -> ε | else Statement
Condition@ -> == NUM | != NUM
)",
};

TEST(Grammar, LeftFactoring) {
  set_console_output_cp_utf8();

  EXPECT_TRIMMED_STR_EQ(getStr(simple.str), simple.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(multi.str), multi.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(complex.str), complex.answer);
  EXPECT_TRIMMED_STR_EQ(getStr(cond.str), cond.answer);
}
struct Answer {
  const char *const str;
  const char *const first_set;
  const char *const follow_set;
  const char *const nullability;
};
constexpr auto simple2 = Answer{
    .str = R"(
S -> A B
A -> a | ε
B -> b
)",
    .first_set = R"(
[{"a", "b"}, {"a", "ε"}, {"b"}]
)",
    .follow_set = R"(
[{"$"}, {"b"}, {"$"}]
)",
    .nullability = R"([false, true, false])",
};
constexpr auto trivial = Answer{
    .str = R"(
S -> id
   | V assign E
V -> id
E -> V
   | num
)",
    .first_set = R"(
[{"id"}, {"id"}, {"id", "num"}]
)",
    .follow_set = R"(
[{"$"}, {"assign"}, {"$"}]
)",
    .nullability = R"([false, false, false])",
};

auto getFirstSet(auto &&str) {
  auto grammar = Grammar::Process(str);
  auto pieces = grammar->non_terminals();
  return Format("{}",
                std::move(pieces) |
                    rv::transform(&Grammar::NonTerminal::first_set));
}
TEST(Grammar, FirstSet) {
  EXPECT_TRIMMED_STR_EQ(getFirstSet(simple2.str), simple2.first_set);

  EXPECT_TRIMMED_STR_EQ(getFirstSet(trivial.str), trivial.first_set);
}
auto getFollowSet(auto &&str) {
  auto grammar = Grammar::Process(str);
  auto pieces = grammar->non_terminals();
  return Format("{}",
                std::move(pieces) |
                    rv::transform(&Grammar::NonTerminal::follow_set));
}
TEST(Grammar, FollowSet) {
  EXPECT_TRIMMED_STR_EQ(getFollowSet(simple2.str), simple2.follow_set);

  EXPECT_TRIMMED_STR_EQ(getFollowSet(trivial.str), trivial.follow_set);
}
constexpr auto ll0_lr0_0 = Answer{
    .str = R"(
S -> b A i B
A -> ε
B -> r C
C -> d
)",
    .first_set = R"([{"b"}, {"ε"}, {"r"}, {"d"}])",
    .follow_set = R"([{"$"}, {"i"}, {"$"}, {"$"}])",
    .nullability = R"([false, true, false, false])",
};

TEST(Grammar, LL0_LR0) {
  auto grammar = *Grammar::Process(ll0_lr0_0.str);
  EXPECT_TRUE(grammar.isLL1());
  auto pieces = grammar.non_terminals();
  EXPECT_TRIMMED_STR_EQ(
      ll0_lr0_0.first_set,
      Format(pieces | rv::transform(&NonTerminal::first_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll0_lr0_0.follow_set,
      Format(pieces | rv::transform(&NonTerminal::follow_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll0_lr0_0.nullability,
      Format(pieces |
             rv::transform(std::bind_back(&NonTerminal::nullable, &grammar))));
}
constexpr auto ll1_lr0_0 = Answer{
    .str = R"(
A -> B
| x C
| y A
B -> C B
C -> r
)",
    .first_set = R"([{"r", "x", "y"}, {"r"}, {"r"}])",
    .follow_set = R"([{"$"}, {"$"}, {"$", "r"}])",
    .nullability = R"([false, false, false])",
};

TEST(Grammar, LL1_LR0) {
  auto grammar = *Grammar::Process(ll1_lr0_0.str);
  EXPECT_TRUE(grammar.isLL1());
  auto pieces = grammar.non_terminals();
  EXPECT_TRIMMED_STR_EQ(
      ll1_lr0_0.first_set,
      Format(pieces | rv::transform(&NonTerminal::first_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_lr0_0.follow_set,
      Format(pieces | rv::transform(&NonTerminal::follow_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_lr0_0.nullability,
      Format(pieces |
             rv::transform(std::bind_back(&NonTerminal::nullable, &grammar))));
}
constexpr auto ll1_slr1_0 = Answer{
    .str = R"(
A -> B c
| d n A B fo
B -> r
| ε
)",
    .first_set = R"([{"r", "c", "d"}, {"r", "ε"}])",
    .follow_set = R"([{"$", "r", "fo"}, {"c", "fo"}])",
    .nullability = R"([false, true])",
};

TEST(Grammar, LL1_SLR1) {
  auto grammar = *Grammar::Process(ll1_slr1_0.str);
  EXPECT_TRUE(grammar.isLL1());
  auto pieces = grammar.non_terminals();
  EXPECT_TRIMMED_STR_EQ(
      ll1_slr1_0.first_set,
      Format(pieces | rv::transform(&NonTerminal::first_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_slr1_0.follow_set,
      Format(pieces | rv::transform(&NonTerminal::follow_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_slr1_0.nullability,
      Format(pieces |
             rv::transform(std::bind_back(&NonTerminal::nullable, &grammar))));
}

constexpr auto ll1_lalr1_0 = Answer{
    .str = R"(
S -> id Sp
Sp -> V assign E
| ε
V -> ε
E -> id V
| num
)",
    .first_set = R"([{"id"}, {"assign", "ε"}, {"ε"}, {"id", "num"}])",
    .follow_set = R"([{"$"}, {"$"}, {"assign", "$"}, {"$"}])",
    .nullability = R"([false, true, true, false])",
};

TEST(Grammar, LL1_LALR1) {
  auto grammar = *Grammar::Process(ll1_lalr1_0.str);
  EXPECT_TRUE(grammar.isLL1());
  auto pieces = grammar.non_terminals();
  EXPECT_TRIMMED_STR_EQ(
      ll1_lalr1_0.first_set,
      Format(pieces | rv::transform(&NonTerminal::first_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_lalr1_0.follow_set,
      Format(pieces | rv::transform(&NonTerminal::follow_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_lalr1_0.nullability,
      Format(pieces |
             rv::transform(std::bind_back(&NonTerminal::nullable, &grammar))));
}
constexpr auto ll1_lr1_0 = Answer{
    .str = R"(
S -> a A
| b B
A -> C a
| D b
B -> C b
| D a
C -> E
D -> E
E -> ε
)",
    .first_set = R"([{"a", "b"}, {"a", "b"}, {"b", "a"}, {"ε"}, {"ε"}, {"ε"}])",
    .follow_set =
        R"([{"$"}, {"$"}, {"$"}, {"a", "b"}, {"b", "a"}, {"a", "b"}])",
    .nullability = R"([false, false, false, true, true, true])",
};

TEST(Grammar, LL1_LR1) {
  auto grammar = *Grammar::Process(ll1_lr1_0.str);
  EXPECT_TRUE(grammar.isLL1());
  auto pieces = grammar.non_terminals();
  EXPECT_TRIMMED_STR_EQ(
      ll1_lr1_0.first_set,
      Format(pieces | rv::transform(&NonTerminal::first_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_lr1_0.follow_set,
      Format(pieces | rv::transform(&NonTerminal::follow_set)));

  EXPECT_TRIMMED_STR_EQ(
      ll1_lr1_0.nullability,
      Format(pieces |
             rv::transform(std::bind_back(&NonTerminal::nullable, &grammar))));
}
