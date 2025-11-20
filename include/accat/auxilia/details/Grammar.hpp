#pragma once
#include <algorithm>
#include <cctype>
#include <charconv>
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <expected>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ostream>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "./config.hpp"
#include "./format.hpp"
#include "./Generator.hpp"
#include "./Status.hpp"
#include "./StatusOr.hpp"
#include "./chars.hpp"
#include "./Trie.hpp"

namespace accat::auxilia {

class Token : public Printable {
public:
  enum class Type : uint8_t {
    // clang-format off
    kMonostate = 0,
    // Single-character tokens.
    kLeftParen, kRightParen, kLeftBrace, kRightBrace, kComma, 
    kDot, kMinus, kPlus, kSemicolon, kSlash, kAmpersand, kStar,
    // One or two character tokens.
    kBang, kBangEqual, kEqual, kEqualEqual, 
    kGreater, kGreaterEqual, kLess, kLessEqual,
    // Literals.
    kIdentifier, kString, kNumber,

    // added to suit this grammar parser
    kLeftArrow, // ->
    kBitwiseOr, // |
    kSquareBracketOpen, // [
    kSquareBracketClose, // ]
    kCaret, // ^

    kLexError,
    // end of file.
    kEndOfFile = std::numeric_limits<uint8_t>::max()
    // clang-format on
  };
  static constexpr auto token_type_str(Type type) -> std::string_view;
  constexpr auto token_type_operator() const -> std::string_view;
  Token() = default;
  Token(Type type, std::string_view lexeme, uint_least32_t line)
      : type_(type), lexeme_(std::string{lexeme}), line_(line) {}
#if __cpp_deleted_function >= 202403L
#  define AC_TOKEN_DELETE delete ("use Token::copy() const noexcept")
#else
#  define AC_TOKEN_DELETE delete
#endif
  Token(const Token &) = AC_TOKEN_DELETE;
  Token &operator=(const Token &) = AC_TOKEN_DELETE;
  Token(Token &&that) noexcept { _do_move(std::move(that)); }
  Token &operator=(Token &&that) noexcept {
    if (this != &that)
      _do_move(std::move(that));
    return *this;
  }
  AC_CONSTEXPR20 ~Token() noexcept {
    if (type_ != Type::kNumber && type_ != Type::kMonostate)
      lexeme_.~string_type();
  }

public:
  auto lexeme() const AC_NOEXCEPT [[clang::lifetimebound]] -> std::string_view {
    AC_PRECONDITION(type_ != Type::kNumber && type_ != Type::kLexError,
                    "lexeme() called on a non-lexeme token")
    return lexeme_;
  }
  auto number() const AC_NOEXCEPT {
    AC_PRECONDITION(type_ == Type::kNumber,
                    "number() called on a non-number token");
    using NumberType = std::variant<long long, long double>;
    return number_is_integer_ ? NumberType{num_ll_} : NumberType{num_ld_};
  }
  auto error_message() const AC_NOEXCEPT [[clang::lifetimebound]]
  -> std::string_view {
    AC_PRECONDITION(type_ == Type::kLexError,
                    "error_message() called on a non-error token")
    return lexeme_;
  }
  constexpr auto type() const noexcept { return type_; }
  constexpr auto type_str() const noexcept { return token_type_str(type_); }

public:
  template <typename... Ts>
  constexpr auto is_type(const Ts... types) const noexcept {
    static_assert((std::same_as<Ts, Type> && ...),
                  "All arguments must be of type Token::Type");
    if constexpr (sizeof...(Ts) == 0)
      return false;
    else
      return ((type_ == types) || ...);
  }
  constexpr auto line() const noexcept { return line_; }
  auto to_string(const auxilia::FormatPolicy &format_policy =
                     auxilia::FormatPolicy::kDefault) const -> string_type {
    auto str = string_type{};
    if (format_policy == auxilia::FormatPolicy::kBrief)
      str = _do_format(format_policy);
    else
      str = Format("type: {}, {}, line: {}",
                   type_str(),
                   _do_format(format_policy),
                   line_);

    return str;
  }

protected:
  Token(Type type, std::string &&error_message, uint_least32_t line)
      : type_(type), lexeme_(std::move(error_message)), line_(line) {}
  Token(Type type, long double number, uint_least32_t line)
      : type_(type), number_is_integer_(false), num_ld_(number), line_(line) {}
  Token(Type type, long long number, uint_least32_t line)
      : type_(type), number_is_integer_(true), num_ll_(number), line_(line) {}

public:
  static auto Number(const std::variant<long long, long double> value,
                     const uint_least32_t line = npos32) noexcept {
    return std::visit(
        [line](auto &&v) { return Token{Type::kNumber, v, line}; }, value);
  }
  static auto Lexeme(const Type type,
                     const std::string_view lexeme,
                     const uint_least32_t line = npos32) noexcept {
    return Token{type, lexeme, line};
  }
  static auto Error(std::string &&message,
                    const uint_least32_t line = npos32) noexcept {
    return Token{Type::kLexError, std::move(message), line};
  }
  static auto Identifier(const std::string_view ident,
                         const uint_least32_t line = npos32) noexcept {
    return Token{Type::kIdentifier, ident, line};
  }
  static auto eof(const uint_least32_t line /* required */) noexcept {
    using namespace std::string_view_literals;
    return Token{Type::kEndOfFile, ""sv, line};
  }

public:
  auto operator<=>(const Token &that) const
      noexcept(noexcept(lexeme_ <=> that.lexeme_)) -> std::partial_ordering {
    if (this == std::addressof(that))
      return std::partial_ordering::equivalent;
    if (type_ != that.type_)
      return type_ <=> that.type_;
    switch (type_) {
    case Type::kLexError:
      return std::partial_ordering::unordered;
    case Type::kNumber:
      if (number_is_integer_ && that.number_is_integer_)
        return num_ll_ <=> that.num_ll_; // strong ordering convert to partial
      return num_ld_ <=> that.num_ld_;
    case Type::kMonostate:
      return std::partial_ordering::equivalent;
    default:
      return lexeme_ <=> that.lexeme_;
    }
  }
  auto operator!=(const Token &that) const
      noexcept(noexcept(this->operator<=>(that))) -> bool {
    return *this <=> that != std::partial_ordering::equivalent;
  }
  Token copy() const noexcept {
    Token newT;
    newT.type_ = type_;
    if (type_ == Type::kNumber) {
      if ((newT.number_is_integer_ = number_is_integer_)) {
        newT.num_ll_ = num_ll_;
      } else {
        newT.num_ld_ = num_ld_;
      }
    } else if (type_ != Type::kMonostate) {
      ::new (std::addressof(newT.lexeme_)) std::string((lexeme_));
    }
    AC_DEBUG_ONLY(else { newT.monostate_ = monostate_; })
    AC_DEBUG_ONLY(newT.line_ = line_;)

    return newT;
  }

private:
  Type type_ = Type::kMonostate;
  union {
    AC_STATIC_ASSERT(std::is_trivial_v<std::monostate>);
    std::monostate monostate_{};
    string_type lexeme_;
    struct {
      bool number_is_integer_;
      union {
        long double num_ld_;
        long long num_ll_;
      };
    };
  };
  uint_least32_t line_ = std::numeric_limits<uint_least32_t>::signaling_NaN();

private:
  void _do_move(Token &&that) noexcept {
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
  auto _do_format(const auxilia::FormatPolicy format_policy) const
      -> string_type {
    auto str = string_type{};
    const auto format_number = [this]() -> long double {
      return number_is_integer_ ? static_cast<long double>(num_ll_) : num_ld_;
    };
    using namespace std::string_literals;
    if (format_policy == auxilia::FormatPolicy::kBrief) {
      if (type_ == Type::kNumber)
        str = Format("{}", format_number());
      else if (type_ == Type::kLexError)
        str = lexeme_;
      else if (type_ == Type::kMonostate)
        str = "monostate"s;
      else
        str = token_type_operator();
    } else {
      if (type_ == Type::kNumber)
        str = Format("number: '{}'", format_number());
      else if (type_ == Type::kLexError)
        str = Format("error: '{}'", lexeme_);
      else if (type_ == Type::kMonostate)
        str = "monostate"s;
      else
        str = Format("lexeme: '{}'", token_type_operator());
    }
    return str;
  }
} inline AC_CONSTEXPR20 nulltok{};
class Lexer {
public:
  using size_type = typename std::string::size_type;
  using string_type = std::string;
  using string_view_type = std::string_view;
  using status_t = auxilia::Status;
  using token_t = Token;
  using token_type_t = Token::Type;
  using char_t = typename string_type::value_type;
  using generator_t = auxilia::Generator<token_t, uint_least32_t>;
  using number_value_t = std::variant<long long, long double>;
  using enum token_type_t;
  static constexpr auto tolerable_chars = as_chars("_`$");
  static constexpr auto whitespace_chars = as_chars(" \t\r");
  static constexpr auto newline_chars = as_chars("\n\v\f");

public:
  Lexer() = delete;
  Lexer(string_type &&s) : contents(std::move(s)) {}
  Lexer(const Lexer &other) = delete;
  Lexer(Lexer &&other) noexcept = default;
  Lexer &operator=(const Lexer &other) = delete;
  Lexer &operator=(Lexer &&other) noexcept = default;
  ~Lexer() = default;

public:
  auto lexAsync() -> generator_t {
    while (not is_at_end()) {
      head = cursor;
      if (auto token = next_token(); token.type() != kMonostate) {
        co_yield token;
      }
    }
    co_yield add_token(kEndOfFile);
    co_return error_count;
  }
  auto lexAll_or_error() -> std::expected<std::vector<token_t>, string_type> {
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
                           |
                           std::ranges::views::transform([](const token_t &t) {
                             return std::string(t.error_message());
                           }) |
                           std::ranges::views::join_with('\n') //
                           | std::ranges::to<string_type>()    //
    );
  }
  /// @brief get the number of errors
  /// @return the number of errors
  auto error() const noexcept -> uint_least32_t { return error_count; }

private:
  bool is_at_end(const size_t offset = 0) const {
    return cursor + offset >= contents.size();
  }
  inline token_t add_identifier_or_keyword() {
    auto value = lex_identifier();
    AC_DEBUG_LOGGING(trace, "identifier: {}", value)
    return add_token(kIdentifier);
  }
  inline token_t add_number() {
    if (auto value = lex_number()) {
      return add_token(*value);
    }
    return add_error_token("Invalid number: " +
                           contents.substr(head, cursor - head));
  }
  inline token_t add_string() {
    // hard to do...

    if (auto status = lex_string(); !status.ok()) {
      // not null-terminated, passing `.data()` only will include the rest of
      // the whole contents.
      return add_error_token(
          {status.message().data(), status.message().size()});
    }
    return add_token(kString);
  }
  inline token_t add_comment() {
    while (peek() != '\n' && !is_at_end())
      get();
    return {};
  }
  inline token_t next_token() {
    // token1 token2
    // 			 ^ cursor position
    AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")

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
      return add_error_token(
          Format("Unexpected character: '{}' at line {}", c, current_line));
    }
  }

  inline auto add_token(const Token::Type type) -> token_t {
    if (type == kEndOfFile) { // FIXME: lexeme bug at EOF(not critical)
      return token_t::eof(current_line);
    }
    auto lexeme = string_view_type(contents.data() + head, cursor - head);
    AC_DEBUG_LOGGING(trace, "lexeme: {}", lexeme)
    return token_t::Lexeme(type, lexeme, current_line);
  }
  inline auto add_token(const number_value_t number) const -> token_t {
    AC_DEBUG_LOGGING(trace, "lexeme: number")
    return token_t::Number(number, current_line);
  }
  inline auto add_error_token(string_type &&msg) -> token_t {
    error_count++;
    // return {kLexError, msg, current_line};
    return token_t::Error(std::move(msg), current_line);
  }
  inline auto lex_string() -> status_t {
    while (peek() != '"' && !is_at_end()) {
      if (peek() == '\n')
        current_line++; // multiline string, of course we dont want act like
                        // C/C++ which will result in a compile error if the
                        // string is not closed at the same current_line.
      get();
    }
    if (is_at_end() && peek() != '"') {
      return auxilia::InvalidArgumentError(
          "Unterminated string: " + contents.substr(head, cursor - head));
    }
    // "i am a string..."
    // 						      ^ cursor position
    else
      get(); // consume the closing quote.
    return {};
  }
  inline auto lex_number() -> std::optional<number_value_t> {
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
  inline string_view_type lex_identifier() {
    while (std::isalnum(peek(), std::locale()) ||
           tolerable_chars.find(peek()) != string_view_type::npos) {
      get();
    }
    // 123_abc
    //       ^ cursor position
    return {contents.data() + head, cursor - head};
  }

private:
  auto to_number(const string_view_type value,
                 const bool isFloating,
                 const int Base) -> std::optional<number_value_t> {
    auto realValStr = value;
    if (value.size() >= 2 && value[0] == '0' && is_valid_base(value[1]))
      realValStr = value.substr(2);

    std::from_chars_result res;
    if (isFloating) {
      if (Base != 10) {
        Println("Only base 10 is supported for floating point");
        return {};
      }
      long double number;
      res = std::from_chars(
          realValStr.data(), realValStr.data() + realValStr.size(), number);
      if (res.ec == std::errc())
        return {{number}};
    } else {
      long long number;
      res = std::from_chars(realValStr.data(),
                            realValStr.data() + realValStr.size(),
                            number,
                            Base);
      if (res.ec == std::errc())
        return {{number}};
    }
    Println("Unable to convert string '{0}' to number: at {1}, "
            "error: {2}",
            realValStr,
            res.ptr,
            std::make_error_code(res.ec).message());
    return {};
  }

  /// @brief lookaheads; we have only consumed the character before the cursor
  char_t peek(const size_t offset = 0) const {
    if (is_at_end(offset))
      return 0; // equivalent to '\0'
    return contents[cursor + offset];
  }
  /// @brief get current character and advance the cursor
  const char_t &get(const size_t offset = 1) {
    AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")
    AC_POSTCONDITION(cursor <= contents.size(), "cursor out of bounds")
    auto &c = contents[cursor];
    cursor += offset;
    return c;
  }

  /// @brief advance the cursor if the character is the expected character
  /// @return true if the character is the expected character and the cursor is
  /// advanced, false otherwise
  bool advance_if_is(const char_t expected) {
    if (is_at_end() || contents[cursor] != expected)
      return false;
    cursor++;
    return true;
  }

  /// @brief advance the cursor if the predicate is true
  /// @tparam Predicate the predicate to check
  /// @param predicate the predicate to check
  /// @return true if the predicate is true and the cursor is advanced, false
  template <typename Predicate> bool advance_if(Predicate &&predicate) {
    static_assert(std::invocable<Predicate, char_t> &&
                  std::convertible_to<Predicate, bool>);
    if (is_at_end() ||
        !std::invoke(std::forward<Predicate>(predicate), contents[cursor]))
      return false;
    cursor++;
    return true;
  }

  static constexpr auto is_valid_base(const char c) noexcept -> bool {
    return c == 'x' || c == 'X' || c == 'b' || c == 'B' || c == 'o' ||
           c == 'O' || c == 'd' || c == 'D';
  }
  static constexpr auto is_valid_digit_of_base(const char c,
                                               const int base) noexcept
      -> bool {
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

private:
  /// @brief head of a token
  size_type head = 0;
  /// @brief current cursor position
  size_type cursor = 0;
  /// @brief the contents of the file
  string_type contents = string_type();
  /// @brief current source line number
  uint_least32_t current_line = 1;
  /// @brief error count
  uint_least32_t error_count = 0;
};
constexpr auto Token::token_type_str(const Type type) -> std::string_view {
  using namespace std::string_view_literals;
  switch (type) {
  case Type::kMonostate:
    return "Monostate"sv;
  case Type::kLeftParen:
    return "LeftParen"sv;
  case Type::kRightParen:
    return "RightParen"sv;
  case Type::kLeftBrace:
    return "LeftBrace"sv;
  case Type::kRightBrace:
    return "RightBrace"sv;
  case Type::kComma:
    return "Comma"sv;
  case Type::kDot:
    return "Dot"sv;
  case Type::kMinus:
    return "Minus"sv;
  case Type::kPlus:
    return "Plus"sv;
  case Type::kSemicolon:
    return "Semicolon"sv;
  case Type::kSlash:
    return "Slash"sv;
  case Type::kAmpersand:
    return "Ampersand"sv;
  case Type::kStar:
    return "Star"sv;
  case Type::kBang:
    return "Bang"sv;
  case Type::kBangEqual:
    return "BangEqual"sv;
  case Type::kEqual:
    return "Equal"sv;
  case Type::kEqualEqual:
    return "EqualEqual"sv;
  case Type::kGreater:
    return "Greater"sv;
  case Type::kGreaterEqual:
    return "GreaterEqual"sv;
  case Type::kLess:
    return "Less"sv;
  case Type::kLessEqual:
    return "LessEqual"sv;
  case Type::kIdentifier:
    return "Identifier"sv;
  case Type::kString:
    return "String"sv;
  case Type::kNumber:
    return "Number"sv;
  case Type::kLexError:
    return "LexError"sv;
  case Type::kEndOfFile:
    return "EndOfFile"sv;
  case Type::kLeftArrow:
    return "LeftArrow"sv;
  case Type::kBitwiseOr:
    return "BitwiseOr"sv;
  case Type::kSquareBracketOpen:
    return "SquareBracketOpen"sv;
  case Type::kSquareBracketClose:
    return "SquareBracketClose"sv;
  case Type::kCaret:
    return "Caret"sv;
  default:
    break;
  }
  return "Unknown"sv;
}
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

class Grammar : public Printable {
  using elem_t = string_type;
  struct Piece;

public:
  Grammar() noexcept = default;
  Grammar(Grammar &&) noexcept = default;
  Grammar &operator=(Grammar &&) noexcept = default;
  Grammar(const Grammar &other) = delete;
  Grammar &operator=(const Grammar &other) = delete;

  using NonTerminal = Piece;
  using Terminal = elem_t;

private:
  /// @note I named thie struct `Piece` at first but it turned out to be
  /// non-terminal at the end.
  struct Piece : Printable {
    friend class Grammar;
    using lhs_t = elem_t;
    using rhs_elem_t = std::vector<elem_t>;
    using rhs_t = std::vector<rhs_elem_t>;
    using set_t = std::unordered_set<string_type>;
    auto nullable() const {
      return std::ranges::any_of(rhs_, [this](auto &&rhsElem) {
        return rhsElem.size() == 1 && rhsElem.front() == epsilon;
      });
    }
    auto to_string(FormatPolicy policy = FormatPolicy::kDefault) const {
      return (lhs_ + (" -> "))
          .append_range(
              rhs_ //
              | std::ranges::views::transform([](auto &&alt) {
                  return alt | std::ranges::views::join_with(' ');
                })                                                     //
              | std::ranges::views::join_with(std::string_view(" | ")) //
              // ^^^ workaround, pass const char* seems cause issue
          );
    }
    auto &lhs() const noexcept { return lhs_; }
    auto &rhs() const noexcept { return rhs_; }
    auto &first_set() const noexcept { return first_set_; }
    auto &follow_set() const noexcept { return follow_set_; }
    auto &select_set() const noexcept { return select_set_; }

  private:
    lhs_t lhs_;
    rhs_t rhs_;
    set_t first_set_;
    set_t follow_set_;
    set_t select_set_;
  };
  std::vector<Piece> pieces_;
  std::unordered_set<string_type> terminals_;

public:
  auto non_terminals_view(this auto &&self) -> decltype(auto) {
    return self.pieces_ | std::ranges::views::transform(&Piece::lhs_);
  }
  auto non_terminals(this auto &&self) -> decltype(auto) {
    return self.pieces_;
  }
  auto terminals(this auto &&self) -> decltype(auto) {
    return self.terminals_ | std::ranges::views::common;
  }
  decltype(auto) terminal(this auto &&self, const std::string &str) {
    auto term = self.terminals_.find(str);
    if (term == self.terminals_.end())
      return static_cast<decltype(&*term)>(nullptr);
    return &*term;
  }
  decltype(auto) non_terminal(this auto &&self, const std::string &str) {
    auto nonT = std::ranges::find(self.pieces_, str, &Piece::lhs_);
    if (nonT == self.pieces_.end()) {
      return static_cast<decltype(&*nonT)>(nullptr);
    }
    return &*nonT;
  }

private:
  /// ensure uniqueness of the name.
  /// @note this function only returns a unique name, and does NOT add it into
  /// the `index_map`.
  auto _new_unique_non_terminal_name(const std::string_view origName,
                                     const char *prime) const {
    string_type newName = origName.data();
    do {
      newName += prime;
    } while (non_terminal(newName));
    return newName;
  }

private:
  void _immediate_left_recursion(Piece &A,
                                 Piece::rhs_t &&recRhsElems,
                                 Piece::rhs_t &&nonRecRhsElems) {
    auto prime = _new_unique_non_terminal_name(A.lhs_, "'");
    // nonTerminals_[prime] = pieces.size();

    // create new piece for A'
    Piece newPiece;
    newPiece.lhs_ = (prime);

    // A -> beta A'
    Piece::rhs_t new_A_rhs;
    for (auto &&beta : nonRecRhsElems | std::ranges::views::as_rvalue) {
      auto &betaAprime = new_A_rhs.emplace_back();
      betaAprime.reserve(beta.size() + 1);
      betaAprime.append_range(beta | std::ranges::views::as_rvalue);
      betaAprime.emplace_back(newPiece.lhs_);
    }
    A.rhs_ = std::move(new_A_rhs);

    // A' -> alpha A' | epsilion
    for (auto &&alpha : recRhsElems | std::ranges::views::as_rvalue) {
      auto &alphaAprime = newPiece.rhs_.emplace_back();
      alphaAprime.reserve(alpha.size() + 1);
      alphaAprime.assign_range(alpha | std::ranges::views::as_rvalue);
      alphaAprime.emplace_back(newPiece.lhs_);
    }
    newPiece.rhs_.emplace_back().emplace_back(epsilon);
    terminals_.emplace(epsilon);

    pieces_.emplace_back(std::move(newPiece));
  }
  // eliminate direct left recursion for A
  Status _analyze_left_recursion(Piece &A) {
    Piece::rhs_t nonRecRhsElems;
    Piece::rhs_t recRhsElems; // store alpha (without leading A)
    // for readability I choose not to remove braces and got alpha and beta as
    // aliases (, though I really want to write them one line).
    for (auto &&rhsElem : std::move(A.rhs_) | std::ranges::views::as_rvalue) {
      if (rhsElem.front() == A.lhs_) {
        // has left recursion
        auto &alpha = recRhsElems.emplace_back();
        alpha.assign_range(rhsElem | std::ranges::views::drop(1) |
                           std::ranges::views::as_rvalue);
      } else {
        auto &beta = nonRecRhsElems.emplace_back();
        beta.assign_range(rhsElem | std::ranges::views::as_rvalue);
      }
    }
    if (recRhsElems.empty()) {
      // no direct left recursion
      // note: A.rhs invalid for we marked it as xvalue previously,
      // so we shall move it back here.
      A.rhs_.assign_range(std::move(nonRecRhsElems) |
                          std::ranges::views::as_rvalue);
      return OkStatus();
    }

    if (nonRecRhsElems.empty())
      // A -> A
      return ResourceExhaustedError("infinite loop");

    // create A'
    _immediate_left_recursion(
        A, std::move(recRhsElems), std::move(nonRecRhsElems));
    return OkStatus();
  }

  // eliminate indirect left recursion
  void _indirect_left_recursion(Piece &A, const Piece &B) const {
    Piece::rhs_t new_rhs;
    for (auto &&rhsElem : std::move(A.rhs_) | std::ranges::views::as_rvalue) {
      AC_RUNTIME_ASSERT(!rhsElem.empty(), "should not happen")
      AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(rhsElem)>);

      if ((rhsElem.front() == B.lhs_)) {
        // A -> B gamma  =>  substitute B -> delta into A
        for (const auto &delta : B.rhs_) {

          auto &combined = new_rhs.emplace_back();

          combined.reserve(delta.size() + (rhsElem.size() - 1));
          std::ranges::copy(delta, std::back_inserter(combined));

          std::ranges::copy(rhsElem                            //
                                | std::ranges::views::as_const //
                                | std::ranges::views::drop(1),
                            std::back_inserter(combined));
          // ^^^ as_const is for readability:
          // we prevent/didn't perform move here from xvalue `rhsElem`
          // (we may use rhsElem again during next iteration of delta)
        }
      } else {
        // keep untouched
        new_rhs.emplace_back(std::move(rhsElem));
      }
    }
    A.rhs_ = std::move(new_rhs);
  }
  static Status preprocess(const std::vector<Token> &tokens) {
    if (tokens.size() == 1) {
      AC_RUNTIME_ASSERT(tokens.back().is_type(Token::Type::kEndOfFile))
      Println("nothing to do");
      return OkStatus();
    }
    if (string_type str; std::ranges::any_of(tokens, [&](const Token &token) {
          // allowed type in this Left Recursion Grammar.
          using enum Token::Type;
          const auto invalid = false; // workaround
                                      // std::ranges::all_of(Grammar::allowed,
                                      // [&token](auto &&type) {
                                      //   return !token.is_type(type);
                                      // });
          if (invalid)
            str.append(Format(R"('{1}' Contains non-allowed type '{0}')",
                              token.type_str(),
                              token.to_string(FormatPolicy::kBrief)));
          return invalid;
        })) {
      return UnimplementedError(std::move(str).append(
          "Grammar contains non-allowed token types; Validation failure."));
    }
    return OkStatus("Lex process successfully finished.");
  }
  void postprocess(std::ranges::common_range auto &&lines) {

    for (auto &&l : lines) {
      AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(l)>);
      auto &piece = pieces_.emplace_back();
      piece.lhs_ = l.front().front().lexeme();

      for (auto &&chunk_view : l // already rvalue
                                   | std::ranges::views::drop(1) //
                                   | std::ranges::views::as_rvalue) {
        piece.rhs_.emplace_back().assign_range(
            chunk_view | std::ranges::views::transform([](auto &&token) {
              return token.lexeme().data();
            }));
      }
    }
    // 1. use for_each to enable possible vectorization(not really in this case)
    // 2. todo: improve this: scan through again, performance heavy?
    std::ranges::for_each(pieces_, [this](auto &&piece) {
      std::ranges::for_each(piece.rhs_, [this](auto &&rhsElem) {
        std::ranges::for_each(rhsElem, [this](auto &&str) {
          if (!this->non_terminal(str))
            this->terminals_.emplace(str);
        });
      });
    });
  }
  auto do_parse(std::vector<Token> &&tokens) {
    using enum Token::Type;

    // I admit it's a bit messy here, but as the saying goes:
    // "If it works, don't touch it".

    string_type errorMsg_lazy;

    constexpr auto stmtSeperator = [](auto &&a, auto &&b) {
      return a.line() == b.line() // same line
                                  // or different line but same statement
             || ((a.line() < b.line()) && (a.is_type(kBitwiseOr, kLeftArrow) ||
                                           b.is_type(kBitwiseOr, kLeftArrow)));
    };

    // first should be a single Identifier.
    const auto is_first_chunk_valid = [&](auto &&chunk) -> bool {
      if (chunk.size() != 1) {
        errorMsg_lazy += Format(
            "line {}: First chunk should only contain a single Identifier\n",
            chunk.front().line());
        return false;
      }
      if (chunk.front().is_type(kLeftArrow)) {
        errorMsg_lazy +=
            Format("line {}: First chunk should be an Identifier"
                   "(special characters as non-terminal is not accepted).\n",
                   chunk.front().line());
        return false;
      }
      return true;
    };

    // Second should be a single LeftArrow.
    // We don't include this chunk in the final output, so always return false.
    // Only append an error if it's malformed.
    const auto is_second_chunk_valid = [&](auto &&chunk) -> bool {
      if (chunk.size() != 1 || !chunk.front().is_type(kLeftArrow)) {
        errorMsg_lazy += Format(
            "line {}: Second chunk should only contain a single LeftArrow.\n",
            chunk.front().line());
      }
      return false;
    };

    // following chunks must not contain additional LeftArrow tokens.
    const auto is_following_chunk_valid = [&](auto &&chunk) -> bool {
      if (std::ranges::any_of(chunk,
                              [](auto &&t) { return t.is_type(kLeftArrow); })) {
        errorMsg_lazy += Format("line {}: Unexpected LeftArrow in rhs.\n",
                                chunk.front().line());
        return false;
      }
      // BitwiseOr indicates an alternative separator;
      // if present, it must be the only token in this chunk.
      if (std::ranges::any_of(chunk,
                              [](auto &&t) { return t.is_type(kBitwiseOr); })) {
        if (chunk.size() != 1) {
          errorMsg_lazy += Format("line {}: Unexpected BitwiseOr in rhs.\n",
                                  chunk.front().line());
          return false;
        }
        // valid BitwiseOr, but we don't need it as a chunk to keep.
        return false;
      }
      // valid elem of rhs.
      return true;
    };

    const auto validSep = [&](auto &&pair) {
      auto &&[index, chunk] = pair;
      if (index == 0)
        // lhs
        return is_first_chunk_valid(chunk);
      if (index == 1)
        // the LeftArrow
        return is_second_chunk_valid(chunk);
      // rhs
      return is_following_chunk_valid(chunk);
    };
    constexpr auto segmentsSep = [](auto &&lhs, auto &&rhs) {
      return (lhs.is_type(kLeftArrow) == rhs.is_type(kLeftArrow)) &&
             (lhs.is_type(kBitwiseOr) == rhs.is_type(kBitwiseOr));
    };

    // is this necessary??? seems not during testing
    const auto tokSize = tokens.size() - 1;

    // transform_view<chunk_by_view<take_view<as_rvalue_view<owning_view<vector<Token>>>>,(l)>,(l)>
    auto lines =
        std::move(tokens) // xvalue to form a owning view rather than a ref view
        | std::ranges::views::as_rvalue     // mark token in tokens as rvalue
        | std::ranges::views::take(tokSize) // drop that kEndOfFile
        | std::ranges::views::chunk_by(stmtSeperator) // split by line
        |
        std::ranges::views::transform([&](auto &&line) {
          return line // now: lhs kLeftArrow rhs_elem1 kBitwiseOr rhs_elem2 ...
                 | std::ranges::views::chunk_by(segmentsSep) // result ^^^
                 | std::ranges::views::enumerate             // [index, chunk]
                 | std::ranges::views::filter(validSep) // extract lhs and rhs.
                 | std::ranges::views::values // drop views::enum's key.
                 | std::ranges::views::common // idk, prevent compat issues
              ;                      // returned: lhs rhs_elem1 rhs_elem2 ...
        })                           //
        | std::ranges::views::common // ditto
        ;

    postprocess(std::move(lines));

    // IMPORTANT: ranges::views are lazy-evaluated, so the validation is done
    // during the post_parse call(where we actually iterate through the views).
    // Thus we should check errorMsg after that;
    // otherwise errorMsg will always be empty here.
    if (!errorMsg_lazy.empty()) {
      return InvalidArgumentError(std::move(errorMsg_lazy));
    }

    return OkStatus();
  }

  void do_factoring(const size_t index) {
    auto &piece = pieces_[index];
    using rhs_elem_t = Piece::rhs_elem_t;
    using rhs_t = Piece::rhs_t;

    Trie<rhs_elem_t> trie;
    trie.assign_range(piece.rhs_);

    auto [lcpPath, lcpNode] = trie.longest_common_prefix();

    if (lcpPath.empty())
      // nothing to factor for this piece
      return;

    // collect suffixes of all productions sharing bestPrefix.
    auto APrimeRhs = trie.collect(lcpNode, epsilon);

    // filter original rhs: keep those not starting with bestPrefix.
    rhs_t newARhs;
    for (auto &&rhsElem :
         std::move(piece.rhs_) | std::ranges::views::as_rvalue) {
      AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(rhsElem)>);
      if (!std::ranges::equal(
              lcpPath, rhsElem | std::ranges::views::take(lcpPath.size()))) {
        // deos not have the lcp
        newARhs.emplace_back(rhsElem);
      }
    }
    auto newName = _new_unique_non_terminal_name(piece.lhs_, "@");

    // add factored prefix + newName.
    // auto factoredPrefix = lcpPath;
    // factoredPrefix.emplace_back(newName);
    // newRhs.emplace_back(std::move(factoredPrefix));

    // ^^^ equivalent to vvv
    newARhs.emplace_back(std::move(lcpPath)).emplace_back(newName);
    piece.rhs_ = std::move(newARhs);

    // new piece for factored suffixes
    auto &newPiece = pieces_.emplace_back();
    newPiece.lhs_ = std::move(newName);

    // if suffix is single epsilon token, keep as-is;
    // else just the sequence.
    // Remove standalone epsilon marker if prefer empty production;
    // here we just keep it.
    newPiece.rhs_.append_range(std::move(APrimeRhs) |
                               std::ranges::views::as_rvalue);
  }
  // IMPORTANT: only Piece::first_set should be modified, other shall as_const
  auto _compute_first_set() {

    const auto computeFirstSet = [this](this auto &&self, Piece &A) -> void {
      if (!A.first_set_.empty()) {
        return;
      }
      for (auto &rhsElem : A.rhs_) {
        AC_RUNTIME_ASSERT(!rhsElem.empty())
        auto f = rhsElem.cbegin();
        if (terminal(*f)) {
          A.first_set_.emplace(*f);
          if (*f != epsilon)
            continue;
        }

        if (*f == epsilon) {
          if (++f == rhsElem.cend())
            continue;
          // else, continue search
        }

        auto &&piece = *non_terminal(*f);
        self(piece);
        A.first_set_.insert_range(piece.first_set_);
      }
    };

    std::ranges::for_each(pieces_,
                          [&](auto &&piece) { computeFirstSet(piece); });
  }
  // ditto: follow_set
  auto _compute_follow_set() {
    // default: the first added non-terminal as start of the grammar
    constexpr static auto dollar = "$";
    pieces_.front().follow_set_.emplace(dollar);

    for (auto changed = true; changed;) {
      changed = false;

      // helper for cleaner code
      const auto isNonTerminal = [this](auto &&str) {
        return non_terminal(str);
      };

      for (const auto &piece : pieces_) {
        for (const auto &reversedView :
             piece.rhs_ | std::ranges::views::transform([](auto &&rhsElem) {
               return rhsElem                        //
                      | std::ranges::views::reverse  //
                      | std::ranges::views::as_const //
                   ;
             })) {

          if (std::ranges::none_of(reversedView, isNonTerminal))
            // nothing to do
            continue;

          Piece::set_t currentAccStartSet;

          auto myrfNonTermIt =
              std::ranges::find_if(reversedView, isNonTerminal); // G
          // guranteed to not pass end
          auto myrlNonTermIt =
              std::ranges::find_last_if(reversedView, isNonTerminal)
                  .begin(); // D

          auto &&myrfFollowSet = non_terminal(*myrfNonTermIt)->follow_set_;

          auto myOldSize = myrfFollowSet.size();

          if (myrfNonTermIt == std::ranges::begin(reversedView)) {
            currentAccStartSet.insert_range(piece.follow_set_);
            myrfFollowSet.insert_range(currentAccStartSet);
          } else {
            // guranteed to not before begin
            auto &&rbeforeTerm = *std::ranges::prev(myrfNonTermIt);
            myrfFollowSet.emplace(rbeforeTerm);
          }

          auto myNewSize = myrfFollowSet.size();
          if (myNewSize != myOldSize)
            changed = true;

          // A -> b c D E f G h i
          for (auto symbol = myrfNonTermIt; symbol != myrlNonTermIt;
               std::ranges::advance(symbol, 1L)) {
            // guranteed not to be end
            auto &&obj = std::ranges::next(symbol);
            if (auto objPtr = non_terminal(*obj)) {
              auto &&objFollowSet = objPtr->follow_set_;

              myOldSize = objFollowSet.size();

              if (auto rfPtr = non_terminal(*symbol)) {
                if (rfPtr->nullable()) {
                  currentAccStartSet.insert_range(rfPtr->first_set_);
                  objFollowSet.insert_range(currentAccStartSet);
                } else {
                  objFollowSet.insert_range(rfPtr->first_set_);
                  currentAccStartSet.clear();
                }
                objFollowSet.erase(epsilon);
              }

              myNewSize = objFollowSet.size();
              if (myNewSize != myOldSize)
                changed = true;

            } else {
              currentAccStartSet.clear();
            }
          }
        }
      }
    }
  }
  auto _compute_select_set() {}

public:
  auto to_string(FormatPolicy = FormatPolicy::kDefault) const {
    return pieces_                                             //
           | std::ranges::views::transform(Printable::Default) //
           | std::ranges::views::join_with('\n')               //
           | std::ranges::to<string_type>()                    //
        ;
  }
  static StatusOr<Grammar> parse(std::vector<Token> &&tokens) {
    if (auto status = preprocess(tokens); !status)
      return {status};

    Grammar grammar;

    if (auto status = grammar.do_parse(std::move(tokens)); !status) {
      return {status};
    }

    return {std::move(grammar)};
  }
  auto eliminate_left_recursion() {
    // only need to examine the original grammar, no need to inspect newly
    // generated one; newly generated is appended after the originals.
    const auto mySize = pieces_.size();
    for (size_t i = 0; i < mySize; ++i) {
      auto &A = pieces_[i];
      for (size_t j = 0; j < i; ++j) {
        auto &B = pieces_[j];
        _indirect_left_recursion(A, B);
      }

      if (auto status = _analyze_left_recursion(A); !status)
        return status;
    }
    return OkStatus();
  }

  auto apply_left_factorization() -> decltype(auto) {

    for (auto changed = true; changed;) {
      changed = false;
      // here I just append new piece, hence it's the same `piece` with
      // `piece[index]`, however, to avoid dangling reference problem here we
      // did not use range-based for loop.
      for (auto index = 0ull; index < pieces_.size(); ++index) {
        const auto before = pieces_[index].rhs_.size();
        do_factoring(index);
        if (pieces_[index].rhs_.size() != before) {
          changed = true;
        }
      }
    }

    return *this;
  }

  decltype(auto) calculate_set() {
    _compute_first_set();
    _compute_follow_set();
    _compute_select_set();

    return *this;
  }

  static StatusOr<Grammar> ContextFree(auto &&tokens) {
    auto grammar = parse(std::forward<decltype(tokens)>(tokens));

    if (!grammar)
      return {std::move(grammar).as_status()};

    if (auto status = grammar->eliminate_left_recursion(); !status)
      return {std::move(status)};

    return {std::move(grammar->apply_left_factorization())};
  }
};
} // namespace accat::auxilia
