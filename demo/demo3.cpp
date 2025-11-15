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

#include "accat/auxilia/details/EBNF.hpp"
#include "accat/auxilia/details/views.hpp"

using namespace accat::auxilia;

constexpr auto complex_expr = R"~~(
Expr -> Expr + Term | Expr - Term | Term
Term -> Term * Factor | Term / Factor | Factor
Factor -> (Expr) | num | id
)~~";

// After elimination:
constexpr auto complex_expr_fixed = R"~~(
Expr -> Term Expr'
Term -> Factor Term'
Factor -> ( Expr ) | num | id
Expr' -> + Term Expr' | - Term Expr' | ε
Term' -> * Factor Term' | / Factor Term' | ε
)~~";

extern const char *const sysc;
#include "accat/auxilia/defines.hpp"
int main() {
  set_console_output_cp_utf8();
  Lexer lexer(sysc);
  auto tokens = lexer.lexAll_or_error();

  if (!tokens) {
    Println(stderr, "Error: {}", tokens.error());
    Println(stderr, "Lex process finished with {} error(s).", lexer.error());
    exit(1);
  }

  auto grammar = Grammar::parse(*std::move(tokens));
  if (!grammar) {
    Println(stderr, "Error: {}", grammar.message());
    exit(1);
  }
  auto good = grammar->eliminate_left_recursion();
  if (!good) {
    Println(stderr, "Error: {}", good.message());
    exit(1);
  }

  // print result
  std::cout << grammar << std::endl;
  // std::cout << complex_expr_fixed << std::endl;
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
Stmt         -> LVal "=" Exp ";" | [Exp] ";" | Block | "if" "(" Cond ")" Stmt [ "else" Stmt ] | "while" "(" Cond ")" Stmt | "break" ";" | "continue" ";" | "return" [Exp] ";" 
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
