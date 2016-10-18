/* File parser.mly */
%{
  open Go
%}
%token <char> LETTER DIGIT
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
    FUNC name LPAREN param RPAREN types block    { Proc ($2, $4, Some($6), $7) }
  | FUNC name LPAREN RPAREN types block          { Proc ($2, [], Some($5), $6) }
  | FUNC name LPAREN param RPAREN block         { Proc ($2, $4, None, $6) }
  | FUNC name LPAREN RPAREN block               { Proc ($2, [], None, $5) }
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
  | vars_s LANGLE DASH aexp               { Transmit ($1, $4) }
  | LANGLE DASH vars_s                    { RcvStmt $3 }
  | vars_s COLON EQUAL bexp               { Decl ($1, $4) }
  | vars_s COLON EQUAL NEWCHANNEL         { DeclChan ($1) }
  | vars_s EQUAL bexp                     { Assign ($1, $3) }
  | WHILE bexp block                      { While ($2, $3) }
  | IF bexp block ELSE block              { ITE ($2, $3, $5) }
  | RETURN bexp                           { Return ($2) }
  | name LPAREN arg RPAREN                { FuncCall ($1, $3) }
  | name LPAREN RPAREN                    { FuncCall ($1, []) }
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
    | LANGLE DASH vars_s          { RcvExp ($3) }
    | EXCLAIM factor              { Not ($2) }
    | LPAREN bexp RPAREN          { $2 }
    | name LPAREN arg RPAREN      { FuncExp ($1, $3) }
    | name LPAREN RPAREN          { FuncExp ($1, []) }
;
arg:
    bexp                          { [$1] }
  | arg COMMA bexp                { $1 @ [$3] }
;
ints:
    DIGIT                         { IConst (int_of_string (String.make 1 $1)) }
    | ints DIGIT                  { IConst ((match $1 with IConst(y) -> y | _ -> 0) + int_of_string (String.make 1 $2)) }
;
bools:
    TRUE                          { BConst (true) }
    | FALSE                       { BConst (false) }
;
vars:
    LETTER                        { Var (String.make 1 $1) }
    | vars LETTER                 { Var (match $1 with Var(y) -> y ^ String.make 1 $2 | _ -> "" ) }
    | vars DIGIT                  { Var (match $1 with Var(y) -> y ^ String.make 1 $2 | _ -> "" ) }
;
vars_s:
    LETTER                        { String.make 1 $1 }
    | vars_s LETTER               { $1 ^ String.make 1 $2 }
    | vars_s DIGIT                { $1 ^ String.make 1 $2 }
;
name:
    LETTER                        { String.make 1 $1 }
    | name LETTER                 { $1 ^ String.make 1 $2 }
;
types:
    INT_TYPE          { TyInt }
    | BOOL_TYPE       { TyBool }
    | CHANNEL_TYPE    { TyChan TyInt }
;