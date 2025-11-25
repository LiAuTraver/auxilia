#include <gtest/gtest.h>
#include <array>
#include <ranges>
#include <utility>

#include "Grammar.hpp"

#include "accat/auxilia/ranges/views.hpp"
#include "accat/auxilia/status/StatusOr.hpp"

using namespace accat::auxilia;
using namespace accat::cp;
using NonTerminal = Grammar::NonTerminal;
using ranges::views::trim;
namespace rv = std::ranges::views;

#define EXPECT_TRIMMED_STR_EQ(str1, str2) EXPECT_EQ(trim(str1), trim(str2))

struct Pair {
  const char *const input = nullptr;
  const char *const output = nullptr;
  static constexpr auto EmptyStr = "";
  static constexpr auto NotNull(const Pair &p) noexcept {
    return p.input != nullptr && p.output != nullptr;
  }
};
struct Answer {
  const char *const rules;
  const char *const first_set;
  const char *const follow_set;
  const char *const nullability;
  const std::array<Pair, 10> tests;
};

constexpr auto simple = Answer{
    .rules = R"(
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
    .rules = R"(
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
  return Format("{}", pieces | rv::transform(&NonTerminal::first_set));
}
TEST(Grammar, FirstSet) {
  EXPECT_TRIMMED_STR_EQ(getFirstSet(simple.rules), simple.first_set);

  EXPECT_TRIMMED_STR_EQ(getFirstSet(trivial.rules), trivial.first_set);
}
auto getFollowSet(auto &&str) {
  auto grammar = Grammar::Process(str);
  auto pieces = grammar->non_terminals();
  return Format("{}", pieces | rv::transform(&NonTerminal::follow_set));
}
TEST(Grammar, FollowSet) {
  EXPECT_TRIMMED_STR_EQ(getFollowSet(simple.rules), simple.follow_set);

  EXPECT_TRIMMED_STR_EQ(getFollowSet(trivial.rules), trivial.follow_set);
}
constexpr auto ll0_lr0_0 = Answer{
    .rules = R"(
S -> b A i B
A -> ε
B -> r C
C -> d
)",
    .first_set = R"([{"b"}, {"ε"}, {"r"}, {"d"}])",
    .follow_set = R"([{"$"}, {"i"}, {"$"}, {"$"}])",
    .nullability = R"([false, true, false, false])",
    .tests =
        {
            {
                {.input = "b i r d", .output = Pair::EmptyStr},
                {.input = "fail", .output = "Unrecognized identifier 'fail'"},
            },
        },
};

TEST(Grammar, LL0_LR0) {
  auto grammar = *Grammar::Process(ll0_lr0_0.rules);
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

  for (auto &&sample : ll0_lr0_0.tests | rv::take_while(Pair::NotNull)) {
    EXPECT_EQ(sample.output, grammar.parse(sample.input).message());
  }
}
constexpr auto ll1_lr0_0 = Answer{
    .rules = R"(
A -> B
| x C
| y A
B -> C B
C -> r
)",
    .first_set = R"([{"r", "x", "y"}, {"r"}, {"r"}])",
    .follow_set = R"([{"$"}, {"$"}, {"$", "r"}])",
    .nullability = R"([false, false, false])",
    .tests =
        {
            {
                {
                    .input = "y y y y y y y y y y y y y y y x r",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = "y y y y y y y y y y y y y y y y y y y x r",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = "x x",
                    .output = "No production found for non-terminal symbol 'C' "
                              "with terminal lookahead symbol 'x'",
                },
                {
                    .input = "x r",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = "*",
                    .output = "Unrecognized identifier '*'",
                },
            },
        },
};

TEST(Grammar, LL1_LR0) {
  auto grammar = *Grammar::Process(ll1_lr0_0.rules);
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

  for (auto &&sample : ll1_lr0_0.tests | rv::take_while(Pair::NotNull)) {
    EXPECT_EQ(sample.output, grammar.parse(sample.input).message());
  }
}
constexpr auto ll1_slr1_0 = Answer{
    .rules = R"(
A -> B c
| d n A B fo
B -> r
| ε
)",
    .first_set = R"([{"r", "c", "d"}, {"r", "ε"}])",
    .follow_set = R"([{"$", "r", "fo"}, {"c", "fo"}])",
    .nullability = R"([false, true])",
    .tests =
        {
            {
                {
                    .input = "d n d n d n r c r fo r fo r fo",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = Pair::EmptyStr,
                    .output = "Unexpected end of input, expected 'A'",
                },
            },
        },
};

TEST(Grammar, LL1_SLR1) {
  auto grammar = *Grammar::Process(ll1_slr1_0.rules);
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

  for (auto &&[input, output] :
       ll1_slr1_0.tests | rv::take_while(Pair::NotNull)) {
    EXPECT_EQ(output, grammar.parse(input).message());
  }
}

constexpr auto ll1_lalr1_0 =
    Answer{.rules = R"(
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
           .tests = {
               {
                   {
                       .input = "id",
                       .output = Pair::EmptyStr,
                   },
                   {
                       .input = "id assign id",
                       .output = Pair::EmptyStr,
                   },
                   {
                       .input = "id assign num",
                       .output = Pair::EmptyStr,
                   },
               },
           }};

TEST(Grammar, LL1_LALR1) {
  auto grammar = *Grammar::Process(ll1_lalr1_0.rules);
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

  for (auto &&[input, output] :
       ll1_lalr1_0.tests | rv::take_while(Pair::NotNull)) {
    EXPECT_EQ(output, grammar.parse(input).message());
  }
}
constexpr auto ll1_lr1_0 = Answer{
    .rules = R"(
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
    .tests =
        {
            {
                {
                    .input = "b b",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = "a b",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = "b a",
                    .output = Pair::EmptyStr,
                },
                {
                    .input = "a a",
                    .output = Pair::EmptyStr,
                },
            },
        },
};

TEST(Grammar, LL1_LR1) {
  auto grammar = *Grammar::Process(ll1_lr1_0.rules);
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
  for (auto &&[input, output] :
       ll1_lr1_0.tests | rv::take_while(Pair::NotNull)) {
    EXPECT_EQ(output, grammar.parse(input).message());
  }
}
