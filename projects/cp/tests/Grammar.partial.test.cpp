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
E -> E+T | T;
T -> T*F | F;
F -> (E) | id;
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
Expr -> Expr + Term | Expr - Term | Term;
Term -> Term * Factor | Term / Factor | Factor;
Factor -> (Expr) | num | id;
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
List -> List , Element | Element;
Element -> a | b | c;
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
T;
T
-> T < F 
| T > F 
| F;
F -> 
(E) 
| 
id 
| 
num;
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
S -> S a | S b | c | d;
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
A -> A -> B | B;
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
S -> A | B;
A -> A a B | A b C | d;
B -> B c A | B d B | e;
C -> C f A | C g B | h;
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
Expr -> Expr + Term | Expr - Term | Term;
Term -> Term * Factor | Term / Factor | Factor;
Factor -> Factor Pow Primary | Primary;
Pow -> ^;
Primary -> ( Expr ) | id | num;
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
A -> A B C | A C B | a;
B -> B A C | B C A | b;
C -> C A B | C B A | c;
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
A -> b c d | b c e | b f;
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
   | b F;
B -> x Y z | x Z;
C -> p Q | p R | s T;
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
Expression -> Term + Expression | Term - Expression | Term;
Term -> Factor * Term | Factor / Term | Factor;
Factor -> ( Expression ) | Number | Identifier;
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
Statement -> if ( Condition ) Statement | if ( Condition ) Statement else Statement;
Condition -> ID == NUM | ID != NUM;
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

constexpr auto ebnf_optional = Results{
    .str = R"(
A -> [ B ] C;
B -> b;
C -> c;
)",
    .answer = R"(
A -> A_Opt C
B -> b
C -> c
A_Opt -> B | ε    
)",
};

constexpr auto ebnf_repetition = Results{
    .str = R"(
A -> { B } C;
B -> b;
C -> c;
)",
    .answer = R"(
A -> A_Rep C
B -> b
C -> c
A_Rep -> ε | A_Rep B
)",
};

constexpr auto ebnf_complex = Results{
    .str = R"(
Expr -> Term { + Term };
Term -> Factor { * Factor };
Factor -> ( Expr ) | id;
)",
    .answer = R"(
Expr -> Term Expr_Rep
Term -> Factor Term_Rep
Factor -> ( Expr ) | id
Expr_Rep -> ε | Expr_Rep + Term
Term_Rep -> ε | Term_Rep * Factor
)",
};

// this will be handled in the left recursion elimination phase not expansion
constexpr auto ebnf_epsilon_recursion = Results{
    .str = R"(
A -> ε | A B;
B -> b;
)",
    .answer = R"(
A -> ε | A B
B -> b
)",
};
auto getStrPostEBNF(auto &&str) -> std::string {

  auto grammar = Grammar::FromStr(str);
  if (!grammar)
    return grammar.raw_message();

  return grammar->to_string();
}
TEST(Grammar, BasicEBNF) {
  set_console_output_cp_utf8();

  EXPECT_TRIMMED_STR_EQ(getStrPostEBNF(ebnf_optional.str),
                        ebnf_optional.answer);
  EXPECT_TRIMMED_STR_EQ(getStrPostEBNF(ebnf_repetition.str),
                        ebnf_repetition.answer);
  EXPECT_TRIMMED_STR_EQ(getStrPostEBNF(ebnf_complex.str), ebnf_complex.answer);
  EXPECT_TRIMMED_STR_EQ(getStrPostEBNF(ebnf_epsilon_recursion.str),
                        ebnf_epsilon_recursion.answer);
}
