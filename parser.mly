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
    block                          { Prog ([], $1) }
  | proc_list block                { Prog ($1, $2) }
;
proc_list:
    proc                           { [$1] }
  | proc_list proc                 { $1 @ [$2] }
;
proc:
    FUNC NAME LBRACE param RBRACE type block    { Proc ($2, $4, Some($6), $7) }
  | FUNC NAME LBRACE RBRACE type block          { Proc ($2, [], Some($5), $6) }
  | FUNC NAME LBRACE param RBRACE block         { Proc ($2, $4, None, $6) }
  | FUNC NAME LBRACE RBRACE block               { Proc ($2, [], None, $6) }
;
param:
     vars type                      { [($1, $2)] }
  |  param COMMA vars type          { $1 @ [($3, $4)] }
;
block:
    LBRACE statement RBRACE    { stmt ($2) }
;
statement:
    statement SEMICOLON statement         { Seq ($1, $3) }
  | GO block                              { Go ($2) }
  | vars LANGLE DASH aexp                 { Transmit ($1, $4) }
  | LANGLE DASH vars                      { RcvStmt $3 }
  | vars COLON EQUAL bexp                 { Decl ($1, $4) }
  | vars COLON EQUAL NEWCHANNEL           { DeclChan ($1) }
  | vars EQUAL bexp                       { Assign ($1, $3) }
  | WHILE bexp block                      { While ($2, $3) }
  | IF bexp block ELSE block              { ITE ($2, $3, $5) }
  | RETURN bexp                           { Return ($2) }
  | NAME LPAREN arg RPAREN                { FuncCall ($1, $3) }
  | NAME LPAREN RPAREN                    { FuncCall ($1, []) }
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
    term PLUS aexp          { Plus ($1, $3) }
    | term MINUS aexp       { Minus ($1, $3) }
    | term                  { $1 }
;
term:
    factor TIMES term       { Times ($1, $3) }
    | factor DIVIDE term    { Division ($1, $3) }
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
    bexp                          { [$1] }
  | arg COMMA bexp                { $1 @ [$2] }
;
ints:
    INT                     { IConst ($1) }
;
bools:
    TRUE                    { BConst (true) }
    | FALSE                 { BConst (false) }
;
vars:
    VARS                    { $1 }
;
type:
    INT_TYPE          { TyInt ($1) }
    | BOOL_TYPE       { TyBool ($1) }
    | CHANNEL_TYPE    { TyChan }
;