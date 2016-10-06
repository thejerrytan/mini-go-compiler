/* File parser.mly */
%{
  Open go
%}
%token <char> LETTER
%token <int> DIGIT
%token PLUS MINUS DIVIDE TIMES
%token EQUAL AMP BAR COLON SEMICOLON RANGLE LANGLE
%token EXCLAIM DASH COMMA LPAREN RPAREN LBRACE RBRACE
%token EOL EOF
%start main             /* the entry point */
%type <Go.go> prog
%%
prog:
    proc              { Prog ([]::$1) }
  | prog proc         { Prog ([]::$1::$2) }
  | prog block        { Prog ($1, $2) }
;
proc:
    FUNC NAME LBRACE param RBRACE type block    { Proc ($2, List($4), Some($6), $7) }
  | FUNC NAME LBRACE param RBRACE block         { Proc ($2, List($4), None, $6) }
;
param:
     vars type                      {  }
  |  param COMMA vars type          {  }
;
block:
    LBRACE statement RBRACE    { stmt ($2) }
;
statement:
    statement SEMICOLON statement         { Seq ($1, $3) }
  | GO block                              { Go ($2) }
  | vars LANGLE DASH aexp                 { RcvStmt ($1, $4) }
  | vars COLON EQUAL bexp                 { Decl ($1, $4) }
  | vars COLON EQUAL NEWCHANNEL           { DeclChan ($1) }
  | vars EQUAL bexp                       { Assign ($1, $3) }
  | WHILE bexp block                      { While ($1, $2) }
  | IF bexp block ELSE block              { ITE ($1, $2, $3) }
  | RETURN bexp                           { Return ($2) }
  | NAME LPAREN arg RPAREN                { FuncCall ($1, $3) }
  | PRINT bexp                            { Print ($2) }
;
bexp:
    cexp AMP AMP cexp        { And ($1, $4) }
    | cexp                   { $1 }
;
cexp:
    cterm EQUAL EQUAL cterm  { Eq ($1, $4) }
    | cterm                  { $1 }
;
cterm:
    aexp RANGLE aexp         { Gt ($1, $3) }
    | aexp                   { $1 }
;
aexp:
    term PLUS term          { Plus ($1, $2) }
    | term MINUS term       { Minus ($1, $2) }
    | term                  { $1 }
;
term:
    factor TIMES factor     { Times ($1, $3) }
    | factor DIVIDE factor  { Division ($1, $3) }
    | factor                { $1 }
;
factor:
    ints                          { $1 }
    | bools                       { $1 }
    | vars                        { $1 }
    | LANGLE DASH vars            { RcvExp ($3) }
    | EXCLAIM factor              { Not ($2) }
    | LPAREN bexp RPAREN          { $1 }
    | NAME LPAREN arg RPAREN      { FuncExp ($1, $3) }
;
arg:
    bexp
    | arg COMMA bexp
;
ints:
    INT                     { IConst ($1) }
;
bools:
    TRUE                    { BConst (true) }
    | FALSE                 { BConst (false) }
;
vars:
    VARS
;
name:
    NAME           { Var ($1) }
;
type:
    INT_TYPE          { TyInt ($1) }
    | BOOL_TYPE       { TyBool ($1) }
    | CHANNEL_TYPE    { TyChan }
;