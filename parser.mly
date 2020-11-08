/* Analyseur syntaxique pour petit Julia */

%{
    
%}

%token EOF
%token PLUS "+"
%token<int> JINT
%token SEMICOLON

%start file

%type <int> file

%%

file:
    | d=decl EOF { d }

decl:
    | e=expr { e }

expr:
    | n=JINT { n }
    |e1=expr "+" e2=expr {e1+e2}
