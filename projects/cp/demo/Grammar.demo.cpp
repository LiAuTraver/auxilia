#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <ostream>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
#include <optional>

#include <accat/auxilia/defines.hpp>

#include "Lexing.hpp"
#include "Grammar.hpp"
#include "accat/auxilia/status/StatusOr.hpp"

using namespace accat::auxilia;
using namespace accat::cp;

constexpr auto flight = R"(
E -> E+T | T
T -> T*F | F
F -> (E) | id
)";

constexpr auto factors = R"(
A -> A B C | A C B | a
B -> B A C | B C A | b  
C -> C A B | C B A | c
)";
constexpr auto flight2 = R"(
E -> E+T | T
T -> T*F | F
F -> E+F | id
)";

constexpr auto simple = R"(
S -> ( X
|	E sq)
|	F )
X -> E )
|	F sq)
E -> A
F -> A
A -> ε
)";

extern const char *const sysc;
AC_SPDLOG_INITIALIZATION("demo", debug)
int main() {
  set_console_output_cp_utf8();

  auto grammar = Grammar::FromStr(simple);
  if (!grammar) {
    Println(stderr, "Error: {}", grammar.message());
    exit(1);
  }
  auto good = grammar->eliminate_left_recursion();
  if (!good) {
    Println(stderr, "Error: {}", good.message());
    exit(1);
  }

  grammar->apply_left_factorization();

  Out << grammar << "\n\n\n";

  Println("TERMINALS: \n{}", grammar->terminals());
  Println("NON-TERMINALS: \n{}", grammar->non_terminals_view());

  grammar->compute_first_set();
  grammar->compute_follow_set();

  auto &pieces = grammar->non_terminals();
  Println("FIRST: {}",
          pieces |
              std::ranges::views::transform(&Grammar::NonTerminal::first_set));
  Println("FOLLOW: {}",
          pieces |
              std::ranges::views::transform(&Grammar::NonTerminal::follow_set));
  Println("SELECT: {}",
          pieces |
              std::ranges::views::transform(&Grammar::NonTerminal::select_set));

  Out << "\n\n"
      << (grammar->isLL1() ? "this grammar is LL1."
                           : "this grammar is not LL1.")
      << "\n\n";

  auto res = grammar->parse(") ( ) sq) ( sq)");
  Out << (res ? "Parsing successful." : res.message());

  return 0;
}

constexpr const char *const sysc = R"~~(
CompUnit     -> [ CompUnit ] ( Decl | FuncDef ) 
Decl         -> ConstDecl | VarDecl 
ConstDecl    -> "const" BType ConstDef { "," ConstDef } ";" 
BType        -> "int" | "float" 
ConstDef -> Ident { "[" ConstExp "]" } "=" ConstInitVal 
ConstInitVal -> ConstExp | "{" [ ConstInitVal { "," ConstInitVal } ] "}" 
VarDecl      -> BType VarDef { "," VarDef } ";" 
VarDef       -> Ident { "[" ConstExp "]" } | Ident { "[" ConstExp "]" } "=" InitVal  
InitVal -> Exp | "{" [ InitVal { "," InitVal } ] "}" 
FuncDef      -> FuncType Ident "(" [FuncFParams] ")" Block 
FuncType     -> "void" | "int" | "float" 
FuncFParams -> FuncFParam { "," FuncFParam } 
FuncFParam   -> BType Ident ["[" "]" { "[" Exp "]" }] 
Block        -> "{" { BlockItem } "}" 
BlockItem    -> Decl | Stmt 
Stmt         -> LVal "=" Exp ";" 
| [Exp] ";" | Block 
| "if" "(" Cond ")" Stmt [ "else" Stmt ] 
| "while" "(" Cond ")" Stmt 
| "break" ";" 
| "continue" ";" 
| "return" [Exp] ";" 
Exp          -> AddExp
Cond         -> LOrExp  
LVal         -> Ident {"[" Exp "]"} 
PrimaryExp -> "(" Exp ")" | LVal | Number 
Number -> IntConst | FloatConst 
UnaryExp -> PrimaryExp | Ident "(" [FuncRParams] ")" | UnaryOp UnaryExp 
UnaryOp      -> "+" | "-" | "!"    
FuncRParams -> Exp { "," Exp } 
MulExp -> UnaryExp | MulExp ( "*" | "/" | "%" ) UnaryExp 
AddExp -> MulExp | AddExp ( "+" | "-" ) MulExp 
RelExp -> AddExp | RelExp ( "<" | ">" | "<=" | ">=" ) AddExp 
EqExp -> RelExp | EqExp ( "==" | "!=" ) RelExp 
LAndExp -> EqExp | LAndExp "&&" EqExp 
LOrExp -> LAndExp | LOrExp "||" LAndExp 
ConstExp -> AddExp
)~~";
