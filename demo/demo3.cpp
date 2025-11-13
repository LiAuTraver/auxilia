#include <fmt/xchar.h>
#include <algorithm>
#include <array>
#include <cstdint>
#include <functional>
#include <iostream>
#include <iterator>
#include <ostream>
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

using namespace accat::auxilia;
struct Parser {
  struct Grammar {
    using elem_t = Token;
    struct Piece {
      using rhs_elem_t = std::vector<elem_t>;
      elem_t lhs;
      rhs_elem_t rhs;
    };
    std::vector<Piece> pieces;
    std::set<elem_t> terminals;
    std::set<elem_t> non_terminals;
    Piece *find_piece(const elem_t &lhs) [[clang::lifetimebound]] {
      for (auto &piece : pieces) {
        if (piece.lhs.lexeme() == lhs.lexeme()) {
          return &piece;
        }
      }
      return nullptr;
    }
  };
  Grammar grammar;

public:
};
int main() {
  Lexer lexer(str);
  auto tokens = lexer.lexAll();

  if (lexer.error()) {
    Println(stderr, "Error:");
    std::ranges::for_each(tokens, [](const Token &token) {
      if (token.is_type(Token::Type::kLexError)) {
        Println(
            "   {}",
            std::ranges::views::drop_while(
                token.to_string(), [](const char ch) { return ch != ','; }) |
                std::ranges::views::drop(1) | //
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
      | std::ranges::views::as_rvalue               //
      | std::ranges::views::take(tokens.size() - 1) // drop that kEndOfFile
      | std::ranges::views::chunk_by(
            [](auto &&a, auto &&b) { return a.line() == b.line(); }) //
      | std::ranges::views::transform([](auto &&line) {
          return std::move(line)
                 // lhs kLeftArrow rhs
                 | std::ranges::views::chunk_by([](auto &&lhs, auto &&rhs) {
                     return lhs.is_type(kLeftArrow) == rhs.is_type(kLeftArrow);
                   })                            //
                 | std::ranges::views::enumerate //
                 // get [lhs, rhs] for each line while performing validation.
                 | std::ranges::views::filter( //
                       [](auto &&pair) {
                         auto &&[index, chunk] = pair;
                         if (index == 0) {
                           if (chunk.size() != 1 ||
                               std::ranges::any_of(chunk, [](auto &&t) {
                                 return t.is_type(kLeftArrow);
                               })) {
                             AC_DEBUG_BREAK
                           }
                           // error
                           return true;
                         }
                         if (index == 1) {
                           if (chunk.size() != 1 ||
                               !chunk.front().is_type(kLeftArrow)) {
                             AC_DEBUG_BREAK
                             return false;
                           }
                           return false;
                         }
                         if (index == 2) {
                           if (std::ranges::any_of(chunk, [](auto &&t) {
                                 return t.is_type(kLeftArrow);
                               })) {
                             AC_DEBUG_BREAK
                             return false;
                           }
                           return true;
                         }
                         AC_DEBUG_BREAK
                         return false;
                       })                     //
                 | std::ranges::views::values //
                 | std::ranges::views::common //
              ;
        })                         //
      | std::ranges::views::common //
      ;
  std::cout << typeid(lines).name() << std::endl;
  Parser::Grammar grammar;
  for (auto &&l : lines) {
    Parser::Grammar::Piece piece;
    piece.lhs = (l.front().front());
    piece.rhs.assign_range(*std::ranges::next(std::move(l).begin()));
    grammar.pieces.emplace_back(std::move(piece));
  }
  Println("");
}
