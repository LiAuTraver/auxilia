#include <accat/auxilia/details/BNFLexer.hpp>
#include <iostream>
#include <ostream>
const char *str = R"~~(
E -> E+T | T
T -> T*F | F
F -> (E) | id)
)~~";
int main() {
  accat::auxilia::Lexer l(str);
  auto l2 = l.lex();
  while (auto v = l2.next()) {
    std::cout << *v << std::endl;
  }
  std::cout << "error count: " << l2.get();
}
