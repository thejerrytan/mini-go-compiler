/* File parser.mly */
        %{
     open Re 
  
    %}
        %token <char> LETTER
        %token PLUS STAR
        %token LPAREN RPAREN
        %token EOL
        %nonassoc STAR           /* highest precedence */
        %start main             /* the entry point */
        %type <Re.re> main
        %%
        main:
            expr EOL                { $1 }
        ;
        expr:
            LETTER                     { Letter $1 }
          | LPAREN expr RPAREN      { $2 }
          | expr PLUS expr          { Alt ($1,$3) }
          | expr expr               { Seq ($1,$2) }
          | expr STAR               { Star $1 }
        ;