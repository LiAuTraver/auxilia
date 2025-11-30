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

extern const char *const sysY;
extern const char *const sysY_BNF;
AC_SPDLOG_INITIALIZATION("demo", debug)
int main() {
  set_console_output_cp_utf8();

  auto grammar = Grammar::FromStr(sysY_BNF);
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

constexpr const char *const sysY = R"~~(
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
constexpr const char *const sysY_BNF = R"~~(
CompUnit -> CompUnit DeclOrFunc | DeclOrFunc
DeclOrFunc -> Decl | FuncDef
Decl -> ConstDecl | VarDecl
ConstDecl -> "const" BType ConstDefList ";"
ConstDefList -> ConstDef | ConstDefList "," ConstDef
BType -> "int" | "float"
ConstDef -> Ident ConstArrayDims "=" ConstInitVal
ConstArrayDims -> ε | ConstArrayDims "[" ConstExp "]"
ConstInitVal -> ConstExp | "{" ConstInitValListOpt "}"
ConstInitValListOpt -> ε | ConstInitValList
ConstInitValList -> ConstInitVal | ConstInitValList "," ConstInitVal
VarDecl -> BType VarDefList ";"
VarDefList -> VarDef | VarDefList "," VarDef
VarDef -> Ident ConstArrayDims | Ident ConstArrayDims "=" InitVal
InitVal -> Exp | "{" InitValListOpt "}"
InitValListOpt -> ε | InitValList
InitValList -> InitVal | InitValList "," InitVal
FuncDef -> FuncType Ident "(" FuncFParamsOpt ")" Block
FuncFParamsOpt -> ε | FuncFParams
FuncType -> "void" | "int" | "float"
FuncFParams -> FuncFParam | FuncFParams "," FuncFParam
FuncFParam -> BType Ident FuncFParamArray
FuncFParamArray -> ε | "[" "]" FuncFParamArrayMore
FuncFParamArrayMore -> ε | FuncFParamArrayMore "[" Exp "]"
Block -> "{" BlockItemList "}"
BlockItemList -> ε | BlockItemList BlockItem
BlockItem -> Decl | Stmt
Stmt -> LVal "=" Exp ";" | ExpOpt ";" | Block | "if" "(" Cond ")" Stmt ElseOpt | "while" "(" Cond ")" Stmt | "break" ";" | "continue" ";" | "return" ExpOpt ";"
ElseOpt -> ε | "else" Stmt
ExpOpt -> ε | Exp
Exp -> AddExp
Cond -> LOrExp
LVal -> Ident LValDims
LValDims -> ε | LValDims "[" Exp "]"
PrimaryExp -> "(" Exp ")" | LVal | Number
Number -> IntConst | FloatConst
UnaryExp -> PrimaryExp | Ident "(" FuncRParamsOpt ")" | UnaryOp UnaryExp
UnaryOp -> "+" | "-" | "!"
FuncRParamsOpt -> ε | FuncRParams
FuncRParams -> Exp | FuncRParams "," Exp
MulExp -> UnaryExp | MulExp MulOp UnaryExp
MulOp -> "*" | "/" | "%"
AddExp -> MulExp | AddExp AddOp MulExp
AddOp -> "+" | "-"
RelExp -> AddExp | RelExp RelOp AddExp
RelOp -> "<" | ">" | "<=" | ">="
EqExp -> RelExp | EqExp EqOp RelExp
EqOp -> "==" | "!="
LAndExp -> EqExp | LAndExp "&&" EqExp
LOrExp -> LAndExp | LOrExp "||" LAndExp
ConstExp -> AddExp
)~~";
