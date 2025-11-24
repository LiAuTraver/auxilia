#pragma once
#include <algorithm>
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <accat/auxilia/auxilia.hpp>

namespace accat::cp {

using auxilia::npos;
using auxilia::npos32;

#pragma region Token
class Token : public auxilia::Printable {

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
  decltype(auto) lexeme_str() const AC_NOEXCEPT [[clang::lifetimebound]] {
    AC_PRECONDITION(type_ != Type::kNumber && type_ != Type::kLexError,
                    "lexeme() called on a non-lexeme token")
    return (lexeme_);
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
  decltype(auto) error_message_str() const AC_NOEXCEPT
      [[clang::lifetimebound]] {
    AC_PRECONDITION(type_ == Type::kLexError,
                    "error_message() called on a non-error token")
    return (lexeme_);
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
                     auxilia::FormatPolicy::kDefault) const -> string_type;

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
  void _do_move(Token &&that) noexcept;
  auto _do_format(auxilia::FormatPolicy format_policy) const -> string_type;
  static constexpr auto token_type_str(Type type) -> std::string_view;
} inline AC_CONSTEXPR20 nulltok{};
#pragma endregion Token

#pragma region Lexer
class [[clang::coro_lifetimebound]] Lexer {
  static inline size_t utf8_cp_len(unsigned char lead) {
    if (lead < 0x80)
      return 1;
    if ((lead >> 5) == 0x6)
      return 2;
    if ((lead >> 4) == 0xE)
      return 3;
    if ((lead >> 3) == 0x1E)
      return 4;
    // treat invalid as single byte
    return 1;
  }

public:
  using size_type = std::string::size_type;
  using string_type = std::string;
  using string_view_type = std::string_view;
  using token_t = Token;
  using token_type_t = Token::Type;
  using char_t = string_type::value_type;
  using generator_t = auxilia::Generator<token_t, uint_least32_t>
      // std::generator<token_t>
      ;
  using number_value_t = std::variant<long long, long double>;
  using enum token_type_t;
  static constexpr auto tolerable_chars = auxilia::as_chars("_`$@");
  static constexpr auto whitespace_chars = auxilia::as_chars(" \t\r");
  static constexpr auto newline_chars = auxilia::as_chars("\n\v\f");

public:
  Lexer() = delete;
  Lexer(string_type &&s) : contents(std::move(s)) {}
  Lexer(const Lexer &other) = delete;
  Lexer(Lexer &&other) noexcept = default;
  Lexer &operator=(const Lexer &other) = delete;
  Lexer &operator=(Lexer &&other) noexcept = default;
  ~Lexer() = default;

public:
  /// @note when use this function, you need to ensure the lifetime of `this`
  /// covers the coroutine, otherwise unexpected situation would happen. In my
  /// own case, the coroutine finished and only produces a kEOF.
  [[deprecated("use the static version to avoid the lifetime issue.")]]
  auto lexAsync() [[clang::lifetimebound]] -> generator_t {
    while (not is_at_end()) {
      head = cursor;
      if (auto token = next_token(); token.type() != kMonostate) {
        co_yield std::move(token);
      }
    }
    co_yield add_token(kEndOfFile);
    co_return error_count;
  }
  static auto LexAsync(string_type &&str) -> generator_t {
    Lexer lexer{std::forward<string_type>(str)};
    while (not lexer.is_at_end()) {
      lexer.head = lexer.cursor;
      if (auto token = lexer.next_token(); token.type() != kMonostate) {
        co_yield std::move(token);
      }
    }
    co_yield lexer.add_token(kEndOfFile);
    co_return lexer.error_count;
  }
  auto lexAll_or_error() -> std::expected<std::vector<token_t>, string_type>;
  /// @brief get the number of errors
  /// @return the number of errors
  auto error() const noexcept -> uint_least32_t { return error_count; }

private:
  bool is_at_end(size_t offset = 0) const;
  token_t add_identifier_or_keyword();
  token_t add_number();
  token_t add_string();
  token_t add_comment();
  token_t next_token();
  token_t add_token(Token::Type type);
  auto add_token(number_value_t number) const -> token_t;
  auto add_error_token(string_type &&msg) -> token_t;
  auto lex_string() -> auxilia::Status;
  auto lex_number() -> std::optional<number_value_t>;
  string_view_type lex_identifier();

private:
  auto to_number(string_view_type value, bool isFloating, int Base)
      -> std::optional<number_value_t>;

  /// @brief lookaheads; we have only consumed the character before the cursor
  char_t peek(size_t offset = 0) const;
  /// @brief get current character and advance the cursor
  const char_t &get(size_t offset = 1);

  /// @brief advance the cursor if the character is the expected character
  /// @return true if the character is the expected character and the cursor is
  /// advanced, false otherwise
  bool advance_if_is(char_t expected);

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
#pragma endregion Enums
#pragma region Enums
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
#pragma endregion Enums
} // namespace accat::cp
