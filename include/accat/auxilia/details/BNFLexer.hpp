#include <algorithm>
#include <cctype>
#include <charconv>
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <limits>
#include <locale>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "./config.hpp"
#include "./format.hpp"
#include "./Status.hpp"
#include "./StatusOr.hpp"
#include "./Generator.hpp"

namespace accat::auxilia {
struct Token : auxilia::Printable {
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

    kLeftArrow, // ->
    kBitwiseOr, // |

    // Keywords.
    // kAnd, kClass, kElse, kFalse, kFun, kFor, kIf, kNil, 
    // kOr, kPrint, kReturn, kSuper, kThis, kTrue, kVar, kWhile,
    // lex error.
    kLexError,
    // end of file.
    kEndOfFile = std::numeric_limits<uint8_t>::max()
    // clang-format on
  };
  static constexpr auto token_type_str(const Type type) {
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
    // case Type::kAnd:
    //   return "And"sv;
    // case Type::kClass:
    //   return "Class"sv;
    // case Type::kElse:
    //   return "Else"sv;
    // case Type::kFalse:
    //   return "False"sv;
    // case Type::kFun:
    //   return "Fun"sv;
    // case Type::kFor:
    //   return "For"sv;
    // case Type::kIf:
    //   return "If"sv;
    // case Type::kNil:
    //   return "Nil"sv;
    // case Type::kOr:
    //   return "Or"sv;
    // case Type::kPrint:
    //   return "Print"sv;
    // case Type::kReturn:
    //   return "Return"sv;
    // case Type::kSuper:
    //   return "Super"sv;
    // case Type::kThis:
    //   return "This"sv;
    // case Type::kTrue:
    //   return "True"sv;
    // case Type::kVar:
    //   return "Var"sv;
    // case Type::kWhile:
    //   return "While"sv;
    case Type::kLexError:
      return "LexError"sv;
    case Type::kEndOfFile:
      return "EndOfFile"sv;
    case Type::kLeftArrow:
      return "LeftArrow"sv;
    case Type::kBitwiseOr:
      return "BitwiseOr"sv;
    }
    return "Unknown"sv;
  }
  Token() = default;
  Token(Type type, std::string_view lexeme, uint_least32_t line)
      : type_(type), lexeme_(std::string{lexeme}), line_(line) {}
  Token(const Token &) = delete;
  Token &operator=(const Token &) = delete;
  Token(Token &&that) noexcept { _do_move(std::move(that)); }
  Token &operator=(Token &&that) noexcept {
    if (this == &that)
      return *this;
    return _do_move(std::move(that));
  }
  AC_CONSTEXPR20_ ~Token() noexcept {
    if (type_ != Type::kNumber && type_ != Type::kMonostate)
      lexeme_.~string_type();
  }

public:
  auto lexeme() const AC_NOEXCEPT [[clang::lifetimebound]] -> std::string_view {
    AC_PRECONDITION(type_ != Type::kNumber && type_ != Type::kLexError,
                    "lexeme() called on a non-lexeme token")
    return lexeme_;
  }
  // Changed number() to return a variant instead of long double.
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
  constexpr auto is_type(const Type type) const noexcept {
    return type_ == type;
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
                     const uint_least32_t line) noexcept {
    return std::visit(
        [line](auto &&v) { return Token{Type::kNumber, v, line}; }, value);
  }
  static auto Lexeme(const Type type,
                     const std::string_view lexeme,
                     const uint_least32_t line) noexcept {
    return Token{type, lexeme, line};
  }
  static auto Error(std::string &&message, const uint_least32_t line) noexcept {
    return Token{Type::kLexError, std::move(message), line};
  }
  static auto eof(const uint_least32_t line) noexcept {
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

private:
  Type type_ = Type::kMonostate;
  union {
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
  Token &_do_move(Token &&that) noexcept {
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
    return *this;
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
        str = type_str();
    } else {
      if (type_ == Type::kNumber)
        str = Format("number: '{}'", format_number());
      else if (type_ == Type::kLexError)
        str = Format("error: '{}'", lexeme_);
      else if (type_ == Type::kMonostate)
        str = "monostate"s;
      else
        str = Format("lexeme: '{}'", type_str());
    }
    return str;
  }
} inline AC_CONSTEXPR20_ nulltok{};
class Lexer {
public:
  using size_type = typename std::string::size_type;
  using string_type = std::string;
  using string_view_type = std::string_view;
  using path_type = std::filesystem::path;
  using status_t = auxilia::Status;
  using token_t = Token;
  using token_type_t = Token::Type;
  using char_t = typename string_type::value_type;
  using generator_t = auxilia::Generator<token_t, uint_least32_t>;
  using number_value_t = std::variant<long long, long double>;

public:
  Lexer() = delete;
  Lexer(string_type &&s) : contents(std::move(s)) {}
  Lexer(const Lexer &other) = delete;
  Lexer(Lexer &&other) noexcept = default;
  Lexer &operator=(const Lexer &other) = delete;
  Lexer &operator=(Lexer &&other) noexcept = default;
  ~Lexer() = default;

public:
  auto lexAsync() -> generator_t;
  auto lexAll() -> std::vector<token_t>;
  /// @brief get the number of errors
  /// @return the number of errors
  auto error() const noexcept -> uint_least32_t { return error_count; }

private:
  token_t add_identifier_or_keyword();
  token_t add_number();
  token_t add_string();
  token_t add_comment();
  token_t next_token();
  token_t add_token(token_type_t);
  token_t add_token(number_value_t) const;
  token_t add_error_token(string_type &&);
  bool is_at_end(size_t = 0) const;
  auto lex_string() -> Lexer::status_t;
  auto lex_identifier() -> string_view_type;
  auto lex_number() -> std::optional<number_value_t>;

private:
  auto to_number(string_view_type, bool, int) -> std::optional<number_value_t>;

  /// @brief lookaheads; we have only consumed the character before the cursor
  char_t peek(size_t = 0) const;
  /// @brief get current character and advance the cursor
  const char_t &get(size_t = 1);

  /// @brief advance the cursor if the character is the expected character
  /// @return true if the character is the expected character and the cursor is
  /// advanced, false otherwise
  bool advance_if_is(char_t);

  /// @brief advance the cursor if the predicate is true
  /// @tparam Predicate the predicate to check
  /// @param predicate the predicate to check
  /// @return true if the predicate is true and the cursor is advanced, false
  template <typename Predicate> bool advance_if(Predicate &&predicate);

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
constexpr auto is_valid_base(const char c) noexcept -> bool {
  return c == 'x' || c == 'X' || c == 'b' || c == 'B' || c == 'o' || c == 'O' ||
         c == 'd' || c == 'D';
}
constexpr auto is_valid_digit_of_base(const char c, const int base) noexcept
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
template <typename Predicate>
inline bool Lexer::advance_if(Predicate &&predicate) {
  static_assert(std::invocable<Predicate, char_t> &&
                std::convertible_to<Predicate, bool>);
  if (is_at_end() ||
      !std::invoke(std::forward<Predicate>(predicate), contents[cursor]))
    return false;
  cursor++;
  return true;
}
inline auto Lexer::to_number(string_view_type value, bool isFloating, int Base)
    -> std::optional<number_value_t> {
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
    res = std::from_chars(
        realValStr.data(), realValStr.data() + realValStr.size(), number, Base);
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
using enum Lexer::token_type_t;
using std::literals::operator""s;
using std::literals::operator""sv;
// inline const auto keywords =
//     std::unordered_map<std::string_view, Token::Type>{
//         {"and"sv, {kAnd}},
//         {"class"sv, {kClass}},
//         {"else"sv, {kElse}},
//         {"false"sv, {kFalse}},
//         {"for"sv, {kFor}},
//         {"fun"sv, {kFun}},
//         {"if"sv, {kIf}},
//         {"nil"sv, {kNil}},
//         {"or"sv, {kOr}},
//         {"print"sv, {kPrint}},
//         {"return"sv, {kReturn}},
//         {"super"sv, {kSuper}},
//         {"this"sv, {kThis}},
//         {"true"sv, {kTrue}},
//         {"var"sv, {kVar}},
//         {"while"sv, {kWhile}},
//     };
// inline constexpr auto conditional_tolerable_chars = "@$#"sv;
inline constexpr auto tolerable_chars = "_`$"sv;
inline constexpr auto whitespace_chars = " \t\r"sv;
inline constexpr auto newline_chars = "\n\v\f"sv;

inline auto Lexer::lexAll() -> std::vector<token_t> {
  std::vector<token_t> result;
  while (not is_at_end()) {
    head = cursor;
    if (auto token = next_token(); token.type() != kMonostate) {
      result.emplace_back(std::move(token));
    }
  }
  result.emplace_back(add_token(kEndOfFile));
  return result;
}
inline auto Lexer::lexAsync() -> generator_t {
  while (not is_at_end()) {
    head = cursor;
    if (auto token = next_token(); token.type() != kMonostate) {
      co_yield token;
    }
  }
  co_yield add_token(kEndOfFile);
  co_return error_count;
}
inline Lexer::token_t Lexer::add_identifier_or_keyword() {
  auto value = lex_identifier();
  // if (keywords.contains(value)) {
  //   return add_token(keywords.at(value));
  // }
  AC_DEBUG_LOGGING(trace, "identifier: {}", value)
  return add_token(kIdentifier);
}
inline Lexer::token_t Lexer::add_number() {
  if (auto value = lex_number()) {
    return add_token(*std::move(value));
  }
  return add_error_token("Invalid number: " +
                         contents.substr(head, cursor - head));
}
inline Lexer::token_t Lexer::add_string() {
  // hard to do...

  if (auto status = lex_string(); !status.ok()) {
    // not null-terminated, passing `.data()` only will include the rest of the
    // whole contents.
    return add_error_token({status.message().data(), status.message().size()});
  }
  return add_token(kString);
}
inline Lexer::token_t Lexer::add_comment() {
  while (peek() != '\n' && !is_at_end())
    get();
  return {};
}
inline Lexer::token_t Lexer::next_token() {
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
    return add_error_token("Unexpected character: "s + c);
  }
}
inline Lexer::char_t Lexer::peek(const size_t offset) const {
  if (is_at_end(offset))
    return 0; // equivalent to '\0'
  return contents[cursor + offset];
}
inline const Lexer::char_t &Lexer::get(const size_t offset) {
  AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")
  AC_POSTCONDITION(cursor <= contents.size(), "cursor out of bounds")
  auto &c = contents[cursor];
  cursor += offset;
  return c;
}
inline bool Lexer::advance_if_is(const char_t expected) {
  if (is_at_end() || contents[cursor] != expected)
    return false;
  cursor++;
  return true;
}
inline bool Lexer::is_at_end(const size_t offset) const {
  return cursor + offset >= contents.size();
}
inline auto Lexer::add_token(Token::Type type) -> token_t {
  if (type == kEndOfFile) { // FIXME: lexeme bug at EOF(not critical)
    return token_t::eof(current_line);
  }
  auto lexeme = string_view_type(contents.data() + head, cursor - head);
  AC_DEBUG_LOGGING(trace, "lexeme: {}", lexeme)
  return token_t::Lexeme(type, lexeme, current_line);
}
inline auto Lexer::add_token(number_value_t number) const -> token_t {
  AC_DEBUG_LOGGING(trace, "lexeme: {}", number)
  return token_t::Number(number, current_line);
}
inline auto Lexer::add_error_token(string_type &&msg) -> token_t {
  error_count++;
  // return {kLexError, msg, current_line};
  return token_t::Error(std::move(msg), current_line);
}
inline auto Lexer::lex_string() -> status_t {
  while (peek() != '"' && !is_at_end()) {
    if (peek() == '\n')
      current_line++; // multiline string, of course we dont want act like C/C++
                      // which will result in a compile error if the string is
                      // not closed at the same current_line.
    get();
  }
  if (is_at_end() && peek() != '"') {
    return auxilia::InvalidArgumentError("Unterminated string: "s +
                                         contents.substr(head, cursor - head));
  }
  // "i am a string..."
  // 						      ^ cursor position
  else
    get(); // consume the closing quote.
  return {};
}
inline auto Lexer::lex_number() -> std::optional<number_value_t> {
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
inline Lexer::string_view_type Lexer::lex_identifier() {
  while (std::isalnum(peek(), std::locale()) ||
         tolerable_chars.find(peek()) != string_view_type::npos) {
    get();
  }
  // 123_abc
  //       ^ cursor position
  return {contents.data() + head, cursor - head};
}
} // namespace accat::auxilia
