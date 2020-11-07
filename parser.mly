/* Analyseur syntaxique pour petit Julia */

%{
    
%}

%token EOF
%token PLUS "+"
%token<int> JINT

file:
    | decl* EOF { }

decl:
    | expr

expr:
    | n=JINT { n }
    | e1=expr "+" e2=expr { e1 + e2 }
