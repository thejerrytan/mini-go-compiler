/* File parser.mly */
%{
  open Go
%}
%token <string> NAME VARS
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS DIVIDE TIMES
%token EQUAL AMP BAR COLON SEMICOLON RANGLE LANGLE
%token EXCLAIM DASH COMMA LPAREN RPAREN LBRACE RBRACE
%token GO IF ELSE RETURN WHILE
%token PRINT NEWCHANNEL FUNC INT_TYPE BOOL_TYPE CHANNEL_TYPE
%token EOL EOF
%start prog             /* the entry point */
%type <Go.prog> prog
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
    FUNC NAME LBRACE param RBRACE types block    { Proc ($2, $4, Some($6), $7) }
  | FUNC NAME LBRACE RBRACE types block          { Proc ($2, [], Some($5), $6) }
  | FUNC NAME LBRACE param RBRACE block         { Proc ($2, $4, None, $6) }
  | FUNC NAME LBRACE RBRACE block               { Proc ($2, [], None, $5) }
;
param:
     vars types                      { [($1, $2)] }
  |  param COMMA vars types          { $1 @ [($3, $4)] }
;
block:
    LBRACE statement RBRACE    { $2 }
;
statement:
    statement SEMICOLON statement         { Seq ($1, $3) }
  | GO block                              { Go ($2) }
  | VARS LANGLE DASH aexp                 { Transmit ($1, $4) }
  | LANGLE DASH VARS                      { RcvStmt $3 }
  | VARS COLON EQUAL bexp                 { Decl ($1, $4) }
  | VARS COLON EQUAL NEWCHANNEL           { DeclChan ($1) }
  | VARS EQUAL bexp                       { Assign ($1, $3) }
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
    | LANGLE DASH VARS            { RcvExp ($3) }
    | EXCLAIM factor              { Not ($2) }
    | LPAREN bexp RPAREN          { $2 }
    | NAME LPAREN arg RPAREN      { FuncExp ($1, $3) }
;
arg:
    bexp                          { [$1] }
  | arg COMMA bexp                { $1 @ [$3] }
;
ints:
    INT                     { IConst ($1) }
;
bools:
    TRUE                    { BConst (true) }
    | FALSE                 { BConst (false) }
;
vars:
    VARS                    { Var ($1) }
types:
    INT_TYPE          { TyInt }
    | BOOL_TYPE       { TyBool }
    | CHANNEL_TYPE    { TyChan TyInt}
;