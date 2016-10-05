/* File parser.mly */
%{

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
    proc block                { $1 }
  | prog proc block EOL          { $2:: }
;
proc:
    FUNC NAME LBRACE param RBRACE type block    { Letter $1 }
;
param:
     vars type COMMA
  |  param vars type
  |
;
block:
    LBRACE statement RBRACE
;
statement:
    statement SEMICOLON statement
  | GO block
  | vars LANGLE DASH aexp
  | vars COLON EQUAL bexp
  | vars COLON EQUAL NEWCHANNEL
  | vars EQUAL bexp
  | WHILE bexp block
  | IF bexp block ELSE block
  | RETURN bexp
  | NAME LPAREN arg RPAREN
  | PRINT bexp
;
bexp:
    cexp AMP AMP cexp
    | cexp
;
cexp:
    cterm EQUAL EQUAL cterm
    | cterm
;
cterm:
    aexp RANGLE aexp
    | aexp
;
aexp:
    term PLUS term
    | term MINUS term
    | term
;
term:
    factor TIMES factor
    | factor DIVIDE factor
    | factor
;
factor:
    ints
    | bools
    | vars
    | LANGLE EQUAL vars
    | EXCLAIM factor
    | LPAREN bexp RPAREN
    | NAME LPAREN arg RPAREN
;
arg:
    bexp COMMA
    | arg bexp
;
ints:
    INT { INT }
;
bools:
    TRUE
    | FALSE
vars:
    VARS
;
name:
    NAME
;
type:
    INT_TYPE
    | BOOL_TYPE
    | CHANNEL_TYPE
;