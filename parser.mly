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
  | expr EOL                { $1 }
;
expr:
    LETTER                     { Letter $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Alt ($1,$3) }
  | expr expr               { Seq ($1,$2) }
  | expr STAR               { Star $1 }
;