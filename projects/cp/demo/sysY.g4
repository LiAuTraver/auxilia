grammar sysY;

INT: 'int';
FLOAT: 'float';
VOID: 'void';
CONST: 'const';
IF: 'if';
ELSE: 'else';
WHILE: 'while';
BREAK: 'break';
CONTINUE: 'continue';
RETURN: 'return';

PLUS: '+';
MINUS: '-';
MUL: '*';
DIV: '/';
MOD: '%';
ASSIGN: '=';
EQ: '==';
NEQ: '!=';
LT: '<';
GT: '>';
LE: '<=';
GE: '>=';
AND: '&&';
OR: '||';
NOT: '!';

LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
LBRACE: '{';
RBRACE: '}';
COMMA: ',';
SEMICOLON: ';';


IDENT: [a-zA-Z_][a-zA-Z_0-9]*;
INT_CONST: [0-9]+;
FLOAT_CONST: [0-9]+ '.' [0-9]* | '.' [0-9]+;


WS: [ \t\r\n]+ -> skip;


compUnit: (decl | funcDef)* EOF;

decl: constDecl | varDecl;

constDecl: CONST btype constDef (COMMA constDef)* SEMICOLON;

btype: INT | FLOAT;

constDef: IDENT (LBRACK constExp RBRACK)* ASSIGN constInitVal;

constInitVal: 
    constExp
    | LBRACE (constInitVal (COMMA constInitVal)*)? RBRACE
    ;

varDecl: btype varDef (COMMA varDef)* SEMICOLON;

varDef: 
    IDENT (LBRACK constExp RBRACK)*                 # varDefNoInit
    | IDENT (LBRACK constExp RBRACK)* ASSIGN initVal # varDefWithInit
    ;

initVal:
    exp
    | LBRACE (initVal (COMMA initVal)*)? RBRACE
    ;

funcDef: funcType IDENT LPAREN funcFParams? RPAREN block;

funcType: VOID | INT | FLOAT;

funcFParams: funcFParam (COMMA funcFParam)*;

funcFParam: btype IDENT (LBRACK RBRACK (LBRACK exp RBRACK)*)?;

block: LBRACE blockItem* RBRACE;

blockItem: decl | stmt;

stmt:
    lVal ASSIGN exp SEMICOLON                       # assignStmt
    | exp? SEMICOLON                                # expStmt
    | block                                         # blockStmt
    | IF LPAREN cond RPAREN stmt (ELSE stmt)?       # ifStmt
    | WHILE LPAREN cond RPAREN stmt                 # whileStmt
    | BREAK SEMICOLON                               # breakStmt
    | CONTINUE SEMICOLON                            # continueStmt
    | RETURN exp? SEMICOLON                         # returnStmt
    ;

exp: addExp;

cond: lOrExp;

lVal: IDENT (LBRACK exp RBRACK)*;

primaryExp:
    LPAREN exp RPAREN
    | lVal
    | number
    ;

number: INT_CONST | FLOAT_CONST;

unaryExp:
    primaryExp                                      # primaryExpUnary
    | IDENT LPAREN funcRParams? RPAREN              # funcCallUnary
    | unaryOp unaryExp                              # unaryOpUnary
    ;

unaryOp: PLUS | MINUS | NOT;

funcRParams: exp (COMMA exp)*;

mulExp:
    unaryExp                                        # unaryExpMul
    | mulExp (MUL | DIV | MOD) unaryExp            # binaryOpMul
    ;

addExp:
    mulExp                                          # mulExpAdd
    | addExp (PLUS | MINUS) mulExp                  # binaryOpAdd
    ;

relExp:
    addExp                                          # addExpRel
    | relExp (LT | GT | LE | GE) addExp            # binaryOpRel
    ;

eqExp:
    relExp                                          # relExpEq
    | eqExp (EQ | NEQ) relExp                      # binaryOpEq
    ;

lAndExp:
    eqExp                                           # eqExpAnd
    | lAndExp AND eqExp                            # binaryOpAnd
    ;

lOrExp:
    lAndExp                                         # andExpOr
    | lOrExp OR lAndExp                            # binaryOpOr
    ;

constExp: addExp;