#include "./test.env.inl.hpp"

#include <accat/auxilia/details/automaton.hpp>

using namespace accat::auxilia;

TEST(NFA, EmptyRegex) {
  auto nfa = *NFA::FromRegex("");
  EXPECT_FALSE(nfa.test("a"));
  EXPECT_TRUE(nfa.test(""));
  EXPECT_EQ(nfa.to_string(), "Automaton <empty>");
}

TEST(NFA, SingleCharacter) {
  auto nfa = *NFA::FromRegex("a");
  EXPECT_TRUE(nfa.test("a"));
  EXPECT_FALSE(nfa.test("b"));
  EXPECT_FALSE(nfa.test(""));
  EXPECT_FALSE(nfa.test("aa"));
}

TEST(NFA, Concatenation) {
  auto nfa = *NFA::FromRegex("abc");
  EXPECT_TRUE(nfa.test("abc"));
  EXPECT_FALSE(nfa.test("ab"));
  EXPECT_FALSE(nfa.test("abcd"));
  EXPECT_FALSE(nfa.test(""));
}

TEST(NFA, Union) {
  auto nfa = *NFA::FromRegex("a|b");
  EXPECT_TRUE(nfa.test("a"));
  EXPECT_TRUE(nfa.test("b"));
  EXPECT_FALSE(nfa.test("c"));
  EXPECT_FALSE(nfa.test("ab"));
  EXPECT_FALSE(nfa.test(""));
}

TEST(NFA, KleeneStar) {
  auto nfa = *NFA::FromRegex("a*");
  EXPECT_TRUE(nfa.test(""));
  EXPECT_TRUE(nfa.test("a"));
  EXPECT_TRUE(nfa.test("aa"));
  EXPECT_TRUE(nfa.test("aaa"));
  EXPECT_FALSE(nfa.test("b"));
  EXPECT_FALSE(nfa.test("ab"));
}

TEST(NFA, PlusOperator) {
  auto nfa = *NFA::FromRegex("a+");
  EXPECT_FALSE(nfa.test(""));
  EXPECT_TRUE(nfa.test("a"));
  EXPECT_TRUE(nfa.test("aa"));
  EXPECT_TRUE(nfa.test("aaa"));
  EXPECT_FALSE(nfa.test("b"));
}

TEST(NFA, Optional) {
  auto nfa = *NFA::FromRegex("ab?c");
  EXPECT_TRUE(nfa.test("ac"));
  EXPECT_TRUE(nfa.test("abc"));
  EXPECT_FALSE(nfa.test("abbc"));
  EXPECT_FALSE(nfa.test("a"));
}

TEST(NFA, ComplexPattern) {
  auto nfa = *NFA::FromRegex("b(a|b)*abb");
  EXPECT_TRUE(nfa.test("babb"));
  EXPECT_TRUE(nfa.test("baabb"));
  EXPECT_TRUE(nfa.test("bbabb"));
  EXPECT_TRUE(nfa.test("bababb"));
  EXPECT_FALSE(nfa.test("abb"));
  EXPECT_FALSE(nfa.test("aabb"));
  EXPECT_FALSE(nfa.test("ab"));
  EXPECT_FALSE(nfa.test("a"));
  EXPECT_FALSE(nfa.test("b"));
  EXPECT_FALSE(nfa.test(""));
}

TEST(NFA, NestedGroups) {
  auto nfa = *NFA::FromRegex("(a|b)(c|d)");
  EXPECT_TRUE(nfa.test("ac"));
  EXPECT_TRUE(nfa.test("ad"));
  EXPECT_TRUE(nfa.test("bc"));
  EXPECT_TRUE(nfa.test("bd"));
  EXPECT_FALSE(nfa.test("ab"));
  EXPECT_FALSE(nfa.test("cd"));
}

TEST(NFA, StarWithGroups) {
  auto nfa = *NFA::FromRegex("(ab)*");
  EXPECT_TRUE(nfa.test(""));
  EXPECT_TRUE(nfa.test("ab"));
  EXPECT_TRUE(nfa.test("abab"));
  EXPECT_TRUE(nfa.test("ababab"));
  EXPECT_FALSE(nfa.test("a"));
  EXPECT_FALSE(nfa.test("aba"));
}

TEST(NFA, AlphanumericPattern) {
  auto nfa = *NFA::FromRegex("a1b2");
  EXPECT_TRUE(nfa.test("a1b2"));
  EXPECT_FALSE(nfa.test("a1b"));
  EXPECT_FALSE(nfa.test("ab12"));
}

TEST(NFA, MultipleOperators) {
  auto nfa = *NFA::FromRegex("a+b*c?");
  EXPECT_TRUE(nfa.test("a"));
  EXPECT_TRUE(nfa.test("ab"));
  EXPECT_TRUE(nfa.test("abc"));
  EXPECT_TRUE(nfa.test("aabbc"));
  EXPECT_TRUE(nfa.test("aaac"));
  EXPECT_FALSE(nfa.test(""));
  EXPECT_FALSE(nfa.test("b"));
}

TEST(NFA, ComplexAlternation) {
  auto nfa1 = *NFA::FromRegex("cat|dog");
  EXPECT_TRUE(nfa1.test("cat"));
  EXPECT_TRUE(nfa1.test("dog"));

  EXPECT_FALSE(nfa1.test("catdog"));
  EXPECT_FALSE(nfa1.test("bird"));

  auto nfa2 = *NFA::FromRegex("ca(t|d)og");
  EXPECT_TRUE(nfa2.test("catog"));
  EXPECT_TRUE(nfa2.test("cadog"));
}

// basic sanity check
TEST(NFA, ToStringOutput) {
  auto nfa = *NFA::FromRegex("ab");
  auto str = nfa.to_string();
  EXPECT_FALSE(str.empty());
  EXPECT_NE(str.find("State"), std::string::npos);
}

TEST(NFA, SingleStarPattern) {
  auto nfa = *NFA::FromRegex("x*");
  EXPECT_TRUE(nfa.test(""));
  EXPECT_TRUE(nfa.test("x"));
  EXPECT_TRUE(nfa.test("xxxxxxxxxx"));
}

TEST(NFA, ErrorHandling) {
  auto result = NFA::FromRegex("(ab");
  EXPECT_FALSE(result.ok());
  EXPECT_EQ(result.code(), Status::Code::kInvalidArgument);
  EXPECT_EQ(result.message(), "Unfinished parentheses in regex: missing ')'");

  result = NFA::FromRegex("a|b)");
  EXPECT_FALSE(result.ok());

  result = NFA::FromRegex("a||b");
  EXPECT_FALSE(result.ok());
  EXPECT_EQ(result.code(), Status::Code::kInvalidArgument);
  EXPECT_EQ(result.message(),
            "Invalid regex at position 1: not enough operands for |");
}

TEST(DFA, SimplePattern) {
  auto dfa = *DFA::FromRegex("ab");
  EXPECT_TRUE(dfa.test("ab"));
  EXPECT_FALSE(dfa.test("a"));
  EXPECT_FALSE(dfa.test("b"));
  EXPECT_FALSE(dfa.test("abc"));
  EXPECT_FALSE(dfa.test(""));
}

TEST(DFA, OpPattern) {
  auto dfa = *DFA::FromRegex("a*b?");
  EXPECT_TRUE(dfa.test(""));
  EXPECT_TRUE(dfa.test("a"));
  EXPECT_TRUE(dfa.test("aa"));
  EXPECT_TRUE(dfa.test("b"));
  EXPECT_TRUE(dfa.test("ab"));
  EXPECT_TRUE(dfa.test("aab"));
  EXPECT_FALSE(dfa.test("ba"));
}

TEST(DFA, ComplexPattern) {
  auto dfa = *DFA::FromRegex("b(a|b)*abb");
  EXPECT_TRUE(dfa.test("babb"));
  EXPECT_TRUE(dfa.test("baabb"));
  EXPECT_TRUE(dfa.test("bbabb"));
  EXPECT_TRUE(dfa.test("bababb"));
  EXPECT_FALSE(dfa.test("abb"));
  EXPECT_FALSE(dfa.test("aabb"));
  EXPECT_FALSE(dfa.test("ab"));
  EXPECT_FALSE(dfa.test("a"));
  EXPECT_FALSE(dfa.test("b"));
  EXPECT_FALSE(dfa.test(""));
}

TEST(DFA, ToDotOutput) {
  auto dfa = *DFA::FromRegex("ab");
  auto dot = dfa.to_dot();
  EXPECT_FALSE(dot.empty());
  EXPECT_NE(dot.find("digraph"), std::string::npos);
  EXPECT_NE(dot.find("->"), std::string::npos);
}

TEST(DFA, AlphanumericPattern) {
  auto dfa = *DFA::FromRegex("a1b2");
  EXPECT_TRUE(dfa.test("a1b2"));
  EXPECT_FALSE(dfa.test("a1b"));
  EXPECT_FALSE(dfa.test("ab12"));
}

TEST(DFA, NestedGroups) {
  auto dfa = *DFA::FromRegex("(a|b)(c|d)");
  EXPECT_TRUE(dfa.test("ac"));
  EXPECT_TRUE(dfa.test("ad"));
  EXPECT_TRUE(dfa.test("bc"));
  EXPECT_TRUE(dfa.test("bd"));
  EXPECT_FALSE(dfa.test("ab"));
  EXPECT_FALSE(dfa.test("cd"));
}
#include <fstream>
TEST(DFA, Minification) {
  auto dfa = *DFA::FromRegex("a*b*a");
  EXPECT_TRUE(dfa.test("aa"));
  EXPECT_TRUE(dfa.test("aba"));
  EXPECT_FALSE(dfa.test("aaabaaa"));
  EXPECT_FALSE(dfa.test("b"));
  EXPECT_FALSE(dfa.test("ab"));

  auto before_minify_dot = dfa.to_dot();
  dfa.minify();
  auto after_minify_dot = dfa.to_dot();

  EXPECT_LE(after_minify_dot.size(), before_minify_dot.size());
  EXPECT_TRUE(dfa.test("aa"));
  EXPECT_TRUE(dfa.test("aba"));
  EXPECT_FALSE(dfa.test("aaabaaa"));
  EXPECT_FALSE(dfa.test("b"));
  EXPECT_FALSE(dfa.test("ab"));

  dfa = *DFA::FromRegex("b(a|b)*abb");
  auto before_minify_dot2 = dfa.to_dot();
  dfa.minify();
  auto after_minify_dot2 = dfa.to_dot();
  EXPECT_LT(after_minify_dot2.size(), before_minify_dot2.size());
}
