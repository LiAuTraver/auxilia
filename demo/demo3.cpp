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

public:
};
using namespace accat::auxilia;
int main() {
  Lexer lexer(str);
  auto tokens = lexer.lexAll();

  if (lexer.error()) {
    Println(stderr, "Error:");
    std::ranges::for_each(tokens, [](const Token &token) {
      if (token.is_type(Token::Type::kLexError)) {
        Println(
            "   {}",
            std::views::drop_while(token.to_string(),
                                   [](const char ch) { return ch != ','; }) |
                std::views::drop(1) | //
                std::ranges::to<std::string>());
      }
    });
    Println(stderr, "Lex process finished with {} error(s).", lexer.error());
    return 1;
  }
  if (tokens.size() == 1) {
    AC_RUNTIME_ASSERT(tokens.back().is_type(kEndOfFile))
    Println("nothing to do");
    return 0;
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
          Println("\'{1}\' Contains non-allowed type \'{0}\'",
                  token.type_str(),
                  token.to_string(FormatPolicy::kBrief));
        return invalid;
      })) {
    Println(stderr, "Validation failure.");
    return 1;
  }
  Println("Lex process successfully finished.");

  // split according to line number
  // std::ranges::chunk_by_view<owning_view<vector<Token,allocator<Token>>>,(lambda)>
  auto lines =
      std::move(tokens) // xvalue to form a owning view rather than a ref view
      | std::ranges::views::take(tokens.size() - 1) // drop that kEndOfFile
      | std::views::chunk_by(
            [](auto &&a, auto &&b) { return a.line() == b.line(); }) //
      ;
  // Println("lines: {}, \n{}", typeid(lines), fmt::join(lines, "\n"));
  // token.is_type(kLeftArrow)
  // lhs kLeftArrow rhs
  // get [lhs, rhs] for each line then continue to check.
  // auto checked = std::move(lines) //
  //                | std::views::filter([](auto &&line) {
  //                    return std::ranges::none_of(line, [](const Token &t) {
  //                      return t.is_type(Token::Type::kLeftArrow);
  //                    });
  //                  }) //
  //                | std::views::transform([](auto &&line) {
  //                    // Process each line with kLeftArrow
  //                    return line;
  //                  });
  std::ranges::for_each(lines, [](auto &&line) {
    if (line.size() < 2) {
    }
  });
  // if (!checked.empty()) {
  //   Println(stderr, )
  // }
  // Println("lines: {}, \n{}", typeid(checked), fmt::join(checked, "\n"));
}
