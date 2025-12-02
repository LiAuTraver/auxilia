constexpr const char *const sysY = R"~~(
CompUnit     -> [ CompUnit ] ( Decl | FuncDef ) ;
Decl         -> ConstDecl | VarDecl ;
ConstDecl    -> "const" BType ConstDef { "," ConstDef } ";" ;
BType        -> "int" | "float" ;
ConstDef -> Ident { "[" ConstExp "]" } "=" ConstInitVal ;
ConstInitVal -> ConstExp | "{" [ ConstInitVal { "," ConstInitVal } ] "}" ;
VarDecl      -> BType VarDef { "," VarDef } ";" ;
VarDef       -> Ident { "[" ConstExp "]" } | Ident { "[" ConstExp "]" } "=" InitVal  ;
InitVal -> Exp | "{" [ InitVal { "," InitVal } ] "}" ;
FuncDef      -> FuncType Ident "(" [FuncFParams] ")" Block ;
FuncType     -> "void" | "int" | "float" ;
FuncFParams -> FuncFParam { "," FuncFParam } ;
FuncFParam   -> BType Ident ["[" "]" { "[" Exp "]" }] ;
Block        -> "{" { BlockItem } "}" ;
BlockItem    -> Decl | Stmt ;
Stmt         -> LVal "=" Exp ";" 
| [Exp] ";" | Block 
| "if" "(" Cond ")" Stmt [ "else" Stmt ] 
| "while" "(" Cond ")" Stmt 
| "break" ";" 
| "continue" ";" 
| "return" [Exp] ";" ;
Exp          -> AddExp ;
Cond         -> LOrExp ;
LVal         -> Ident {"[" Exp "]"} ;
PrimaryExp -> "(" Exp ")" | LVal | Number ;
Number -> IntConst | FloatConst ;
UnaryExp -> PrimaryExp | Ident "(" [FuncRParams] ")" | UnaryOp UnaryExp ;
UnaryOp      -> "+" | "-" | "!"   ; 
FuncRParams -> Exp { "," Exp } ;
MulExp -> UnaryExp | MulExp ( "*" | "/" | "%" ) UnaryExp ;
AddExp -> MulExp | AddExp ( "+" | "-" ) MulExp ;
RelExp -> AddExp | RelExp ( "<" | ">" | "<=" | ">=" ) AddExp ;
EqExp -> RelExp | EqExp ( "==" | "!=" ) RelExp ;
LAndExp -> EqExp | LAndExp "&&" EqExp ;
LOrExp -> LAndExp | LOrExp "||" LAndExp ;
ConstExp -> AddExp ;
)~~";

constexpr const char *const sysY_BNF = R"~~(
CompUnit -> CompUnit DeclOrFunc | DeclOrFunc ;
DeclOrFunc -> Decl | FuncDef ;
Decl -> ConstDecl | VarDecl ;
ConstDecl -> "const" BType ConstDefList ";" ;
ConstDefList -> ConstDef | ConstDefList "," ConstDef ;
BType -> "int" | "float" ;
ConstDef -> Ident ConstArrayDims "=" ConstInitVal ;
ConstArrayDims -> ε | ConstArrayDims "[" ConstExp "]" ;
ConstInitVal -> ConstExp | "{" ConstInitValListOpt "}" ;
ConstInitValListOpt -> ε | ConstInitValList ;
ConstInitValList -> ConstInitVal | ConstInitValList "," ConstInitVal ;
VarDecl -> BType VarDefList ";" ;
VarDefList -> VarDef | VarDefList "," VarDef ;
VarDef -> Ident ConstArrayDims | Ident ConstArrayDims "=" InitVal ;
InitVal -> Exp | "{" InitValListOpt "}" ;
InitValListOpt -> ε | InitValList ;
InitValList -> InitVal | InitValList "," InitVal ;
FuncDef -> FuncType Ident "(" FuncFParamsOpt ")" Block ;
FuncFParamsOpt -> ε | FuncFParams ;
FuncType -> "void" | "int" | "float" ;
FuncFParams -> FuncFParam | FuncFParams "," FuncFParam ;
FuncFParam -> BType Ident FuncFParamArray ;
FuncFParamArray -> ε | "[" "]" FuncFParamArrayMore ;
FuncFParamArrayMore -> ε | FuncFParamArrayMore "[" Exp "]" ;
Block -> "{" BlockItemList "}" ;
BlockItemList -> ε | BlockItemList BlockItem ;
BlockItem -> Decl | Stmt ;
Stmt -> LVal "=" Exp ";" | ExpOpt ";" | Block | "if" "(" Cond ")" Stmt ElseOpt | "while" "(" Cond ")" Stmt | "break" ";" | "continue" ";" | "return" ExpOpt ";"  ;
ElseOpt -> ε | "else" Stmt ;
ExpOpt -> ε | Exp ;
Exp -> AddExp ;
Cond -> LOrExp ;
LVal -> Ident LValDims ;
LValDims -> ε | LValDims "[" Exp "]" ;
PrimaryExp -> "(" Exp ")" | LVal | Number ;
Number -> IntConst | FloatConst ;
UnaryExp -> PrimaryExp | Ident "(" FuncRParamsOpt ")" | UnaryOp UnaryExp ;
UnaryOp -> "+" | "-" | "!" ;
FuncRParamsOpt -> ε | FuncRParams ;
FuncRParams -> Exp | FuncRParams "," Exp ;
MulExp -> UnaryExp | MulExp MulOp UnaryExp ;
MulOp -> "*" | "/" | "%" ;
AddExp -> MulExp | AddExp AddOp MulExp ;
AddOp -> "+" | "-" ;
RelExp -> AddExp | RelExp RelOp AddExp ;
RelOp -> "<" | ">" | "<=" | ">=" ;
EqExp -> RelExp | EqExp EqOp RelExp ;
EqOp -> "==" | "!=" ;
LAndExp -> EqExp | LAndExp "&&" EqExp ;
LOrExp -> LAndExp | LOrExp "||" LAndExp ;
ConstExp -> AddExp ;
)~~";
