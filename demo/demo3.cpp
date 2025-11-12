#include <fmt/base.h>
#include <algorithm>
#include <array>
#include <cstdint>
#include <functional>
#include <iostream>
#include <ranges>
#include <set>
#include <string>
#include <vector>

#include "accat/auxilia/details/BNFLexer.hpp"
#include "accat/auxilia/details/Generator.hpp"
#include "accat/auxilia/details/format.hpp"
const char *str = R"~~(
E -> E+T | T
T -> T*F | F
F -> (E) | id)
)~~";

class Parser {
  struct Grammar {
    using elem_t = std::string;
    struct Piece {
      using rhs_elem_t = std::vector<elem_t>;
      elem_t lhs;
      std::vector<rhs_elem_t> rhs;
    };
    std::vector<Piece> pieces;
    std::set<elem_t> terminals;
    std::set<elem_t> non_terminals;
    Piece *find_piece(const elem_t &lhs) [[clang::lifetimebound]] {
      for (auto &piece : pieces) {
        if (piece.lhs == lhs) {
          return &piece;
        }
      }
      return nullptr;
    }
  };
  Grammar grammar;
};
using namespace accat::auxilia;
int main() {
  Lexer lexer(str);
  auto tokens = lexer.lexAll();

  if (lexer.error()) {
    println("Error:");
    std::ranges::for_each(tokens, [](const Token &token) {
      if (token.is_type(Token::Type::kLexError)) {
        println(
            "   {}",
            std::views::drop_while(token.to_string(),
                                   [](const char ch) { return ch != ','; }) |
                std::views::drop(1) | //
                std::ranges::to<std::string>());
      }
    });
    println("Lex process finished with {} error(s).", lexer.error());
    return 1;
  }

  if (std::ranges::any_of(tokens, [&](const Token &token) {
        // allowed type in this Left Recursion Grammar.
        using enum Token::Type;
        static constexpr auto allowed = {kMonostate,
                                         kEndOfFile,
                                         kPlus,
                                         kStar,
                                         kIdentifier,
                                         kBitwiseOr,
                                         kLeftArrow,
                                         kLeftParen,
                                         kRightParen};
        const auto invalid = std::ranges::all_of(
            allowed, [&token](auto &&type) { return !token.is_type(type); });
        if (invalid)
          println("\'{1}\' Contains non-allowed type \'{0}\'",
                  token.type_str(),
                  token.to_string(FormatPolicy::kBrief));
        return invalid;
      })) {
    println("Validation failure.");
    return 1;
  }
  println("Lex process successfully finished.");
  // split according to line number
  std::vector<std::vector<Token>> lines{};

  auto sv = std::views::chunk_by(std::move(tokens), [](auto &&a, auto &&b) {
    return a.line() == b.line();
  });
  println("{}", sv);
}
