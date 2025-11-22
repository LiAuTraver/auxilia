#include <cctype>
#include <charconv>
#include <cstddef>
#include <expected>
#include <locale>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "Lexing.hpp"

namespace accat::cp {
constexpr auto Token::token_type_operator() const -> std::string_view {
  using namespace std::string_view_literals;
  switch (type_) {
  case Type::kLeftParen:
    return "("sv;
  case Type::kRightParen:
    return ")"sv;
  case Type::kLeftBrace:
    return "{"sv;
  case Type::kRightBrace:
    return "}"sv;
  case Type::kComma:
    return ","sv;
  case Type::kDot:
    return "."sv;
  case Type::kMinus:
    return "-"sv;
  case Type::kPlus:
    return "+"sv;
  case Type::kSemicolon:
    return "sv;"sv;
  case Type::kSlash:
    return "/"sv;
  case Type::kAmpersand:
    return "&"sv;
  case Type::kStar:
    return "*"sv;
  case Type::kBang:
    return "!"sv;
  case Type::kBangEqual:
    return "!="sv;
  case Type::kEqual:
    return "="sv;
  case Type::kEqualEqual:
    return "=="sv;
  case Type::kGreater:
    return ">"sv;
  case Type::kGreaterEqual:
    return ">="sv;
  case Type::kLess:
    return "<"sv;
  case Type::kLessEqual:
    return "<="sv;
  case Type::kLeftArrow:
    return "->"sv;
  case Type::kBitwiseOr:
    return "|"sv;
  case Type::kSquareBracketOpen:
    return "["sv;
  case Type::kSquareBracketClose:
    return "]"sv;
  case Type::kCaret:
    return "^"sv;
  case Type::kMonostate:
    [[fallthrough]];
  case Type::kIdentifier:
    [[fallthrough]];
  case Type::kString:
    [[fallthrough]];
  case Type::kNumber:
    [[fallthrough]];
  case Type::kLexError:
    [[fallthrough]];
  case Type::kEndOfFile:
    break;
  }
  return lexeme_;
}
auto Token::to_string(const auxilia::FormatPolicy &format_policy) const
    -> string_type {
  auto str = string_type{};
  if (format_policy == auxilia::FormatPolicy::kBrief)
    str = _do_format(format_policy);
  else
    str = auxilia::Format(
        "type: {}, {}, line: {}", type_str(), _do_format(format_policy), line_);

  return str;
}
void Token::_do_move(Token &&that) noexcept {
  // Destroy old active member
  if (type_ != Type::kNumber && type_ != Type::kMonostate)
    lexeme_.~string_type();

  type_ = that.type_;
  line_ = that.line_;

  if (that.type_ == Type::kNumber) {
    if (that.number_is_integer_) {
      num_ll_ = that.num_ll_;
      number_is_integer_ = true;
    } else {
      num_ld_ = that.num_ld_;
      number_is_integer_ = false;
    }
  } else {
    ::new (std::addressof(lexeme_)) std::string(std::move(that.lexeme_));
  }

  that.type_ = Type::kMonostate;
}
auto Token::_do_format(const auxilia::FormatPolicy format_policy) const
    -> string_type {
  auto str = string_type{};
  const auto format_number = [this]() -> long double {
    return number_is_integer_ ? static_cast<long double>(num_ll_) : num_ld_;
  };
  using namespace std::string_literals;
  if (format_policy == auxilia::FormatPolicy::kBrief) {
    if (type_ == Type::kNumber)
      str = auxilia::Format("{}", format_number());
    else if (type_ == Type::kLexError)
      str = lexeme_;
    else if (type_ == Type::kMonostate)
      str = "monostate"s;
    else
      str = token_type_operator();
  } else {
    if (type_ == Type::kNumber)
      str = auxilia::Format("number: '{}'", format_number());
    else if (type_ == Type::kLexError)
      str = auxilia::Format("error: '{}'", lexeme_);
    else if (type_ == Type::kMonostate)
      str = "monostate"s;
    else
      str = auxilia::Format("lexeme: '{}'", token_type_operator());
  }
  return str;
}
auto Lexer::lexAll_or_error()
    -> std::expected<std::vector<token_t>, string_type> {
  std::vector<token_t> result;
  while (not is_at_end()) {
    head = cursor;
    if (auto token = next_token(); token.type() != kMonostate) {
      result.emplace_back(std::move(token));
    }
  }
  result.emplace_back(add_token(kEndOfFile));
  if (!error())
    return result;

  return std::unexpected(result //
                         | std::ranges::views::filter([](const token_t &t) {
                             return t.is_type(Token::Type::kLexError);
                           }) //
                         | std::ranges::views::transform([](const token_t &t) {
                             return std::string(t.error_message());
                           }) |
                         std::ranges::views::join_with('\n') //
                         | std::ranges::to<string_type>()    //
  );
}
bool Lexer::is_at_end(const size_t offset) const {
  return cursor + offset >= contents.size();
}
Lexer::token_t Lexer::add_identifier_or_keyword() {
  auto value = lex_identifier();
  AC_DEBUG_LOGGING(trace, "identifier: {}", value)
  return add_token(kIdentifier);
}
Lexer::token_t Lexer::add_number() {
  if (auto value = lex_number()) {
    return add_token(*value);
  }
  return add_error_token("Invalid number: " +
                         contents.substr(head, cursor - head));
}
Lexer::token_t Lexer::add_string() {
  // hard to do...

  if (auto status = lex_string(); !status.ok()) {
    // not null-terminated, passing `.data()` only will include the rest of
    // the whole contents.
    return add_error_token({status.message().data(), status.message().size()});
  }
  return add_token(kString);
}
Lexer::token_t Lexer::add_comment() {
  while (peek() != '\n' && !is_at_end())
    get();
  return {};
}
Lexer::token_t Lexer::next_token() {
  // token1 token2
  // 			 ^ cursor position
  AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")

  // UTF-8 epsilon is 2 bytes (0xCE 0xB5)
  if (cursor + 2 <= contents.size() && auxilia::is_epsilon(&contents[cursor])) {
    cursor += 2;
    return add_token(kIdentifier);
  }

  // we use `monostate` to indicate that the token is a
  switch (auto c = get()) {
  case '(':
    return add_token(kLeftParen);
  case ')':
    return add_token(kRightParen);
  case '{':
    return add_token(kLeftBrace);
  case '}':
    return add_token(kRightBrace);
  case ',':
    return add_token(kComma);
  case '.':
    return add_token(kDot);
  case '-':
    return add_token(advance_if_is('>') ? kLeftArrow : kMinus);
  case '|':
    return add_token(kBitwiseOr);
  case '+':
    return add_token(kPlus);
  case ';':
    return add_token(kSemicolon);
  case '*':
    return add_token(kStar);
  case '&':
    return add_token(kAmpersand);
  case '[':
    return add_token(kSquareBracketOpen);
  case ']':
    return add_token(kSquareBracketClose);
  case '^':
    return add_token(kCaret);
  case '!':
    return add_token(advance_if_is('=') ? kBangEqual : kBang);
  case '=':
    return add_token(advance_if_is('=') ? kEqualEqual : kEqual);
  case '<':
    return add_token(advance_if_is('=') ? kLessEqual : kLess);
  case '>':
    return add_token(advance_if_is('=') ? kGreaterEqual : kGreater);
  case '/':
    return advance_if_is('/') ? add_comment() : add_token(kSlash);
  default:
    if (whitespace_chars.contains(c))
      return {}; // continue
    if (newline_chars.contains(c)) {
      current_line++;
      return {}; // continue
    }
    if (c == '"') {
      return add_string();
    }
    // first, numbers(order matters!)
    if (std::isdigit(c, std::locale())) {
      return add_number();
    }
    // finally, letters
    if (std::isalpha(c, std::locale()) or tolerable_chars.contains(c)) {
      return add_identifier_or_keyword();
    }
    return add_error_token(auxilia::Format(
        "Unexpected character: '{}' at line {}", c, current_line));
  }
}
auto Lexer::add_token(const Token::Type type) -> token_t {
  if (type == kEndOfFile) { // FIXME: lexeme bug at EOF(not critical)
    return token_t::eof(current_line);
  }
  auto lexeme = string_view_type(contents.data() + head, cursor - head);
  AC_DEBUG_LOGGING(trace, "lexeme: {}", lexeme)
  return token_t::Lexeme(type, lexeme, current_line);
}
auto Lexer::add_token(const number_value_t number) const -> token_t {
  AC_DEBUG_LOGGING(trace, "lexeme: number")
  return token_t::Number(number, current_line);
}
auto Lexer::add_error_token(string_type &&msg) -> token_t {
  error_count++;
  // return {kLexError, msg, current_line};
  return token_t::Error(std::move(msg), current_line);
}
auto Lexer::lex_string() -> auxilia::Status {
  while (peek() != '"' && !is_at_end()) {
    if (peek() == '\n')
      current_line++; // multiline string, of course we dont want act like
                      // C/C++ which will result in a compile error if the
                      // string is not closed at the same current_line.
    get();
  }
  if (is_at_end() && peek() != '"') {
    return auxilia::InvalidArgumentError("Unterminated string: " +
                                         contents.substr(head, cursor - head));
  }
  // "i am a string..."
  // 						      ^ cursor position
  else
    get(); // consume the closing quote.
  return {};
}
auto Lexer::lex_number() -> std::optional<number_value_t> {
  // 0x123456
  //  ^ cursor position(one after the first digit)
  auto Base = 10;
  if (auto c = peek(); is_valid_base(c)) {
    if (c == 'x' || c == 'X')
      Base = 16;
    else if (c == 'b' || c == 'B')
      Base = 2;
    else if (c == 'o' || c == 'O')
      Base = 8;
    else if (c == 'd' || c == 'D')
      Base = 10;
    get(); // consume the 'x', 'b', or 'o'
  }
  while (is_valid_digit_of_base(peek(), Base)) {
    get();
  }
  bool is_floating_point = false;
  // maybe a '.'?
  if (peek() == '.' && std::isdigit(peek(1), std::locale())) {
    get(); // consume the '.'
    while (is_valid_digit_of_base(peek(), Base)) {
      get();
    }
    // 123.456
    // 		    ^ cursor position(one after the last digit)
    is_floating_point = true;
  }
  // 789
  //    ^ cursor position
  auto value = contents.substr(head, cursor - head);
  return to_number(value, is_floating_point, Base);
}
Lexer::string_view_type Lexer::lex_identifier() {
  while (std::isalnum(peek(), std::locale()) ||
         tolerable_chars.find(peek()) != string_view_type::npos) {
    get();
  }
  // 123_abc
  //       ^ cursor position
  return {contents.data() + head, cursor - head};
}
auto Lexer::to_number(const string_view_type value,
                      const bool isFloating,
                      const int Base) -> std::optional<number_value_t> {
  auto realValStr = value;
  if (value.size() >= 2 && value[0] == '0' && is_valid_base(value[1]))
    realValStr = value.substr(2);

  std::from_chars_result res;
  if (isFloating) {
    if (Base != 10) {
      auxilia::Println("Only base 10 is supported for floating point");
      return {};
    }
    long double number;
    res = std::from_chars(
        realValStr.data(), realValStr.data() + realValStr.size(), number);
    if (res.ec == std::errc())
      return {{number}};
  } else {
    long long number;
    res = std::from_chars(
        realValStr.data(), realValStr.data() + realValStr.size(), number, Base);
    if (res.ec == std::errc())
      return {{number}};
  }
  auxilia::Println("Unable to convert string '{0}' to number: at {1}, "
                   "error: {2}",
                   realValStr,
                   res.ptr,
                   std::make_error_code(res.ec).message());
  return {};
}
Lexer::char_t Lexer::peek(const size_t offset) const {
  if (is_at_end(offset))
    return 0; // equivalent to '\0'
  return contents[cursor + offset];
}
const Lexer::char_t &Lexer::get(const size_t offset) {
  AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")
  AC_POSTCONDITION(cursor <= contents.size(), "cursor out of bounds")
  auto &c = contents[cursor];
  cursor += offset;
  return c;
}
bool Lexer::advance_if_is(const char_t expected) {
  if (is_at_end() || contents[cursor] != expected)
    return false;
  cursor++;
  return true;
}
constexpr auto Lexer::is_valid_base(const char c) noexcept -> bool {
  return c == 'x' || c == 'X' || c == 'b' || c == 'B' || c == 'o' || c == 'O' ||
         c == 'd' || c == 'D';
}
constexpr auto Lexer::is_valid_digit_of_base(const char c,
                                             const int base) noexcept -> bool {
  switch (base) {
  case 2:
    return c == '0' || c == '1';
  case 8:
    return c >= '0' && c <= '7';
  case 10:
    return std::isdigit(c, std::locale());
  case 16:
    return std::isxdigit(c, std::locale());
  default:
    return false;
  }
}
} // namespace accat::cp
