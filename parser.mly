/* Analyseur syntaxique pour petit Julia */

%{
    open Ast
    open Parsing
%}

%token EOF
%token PLUS "+"
%token MINUS "-"
%token TIMES "*" 
%token <int> JINT
%token SEMICOLON ";"

%start file

%type <Ast.file> file

%%

file:
    | d=decl* EOF { d }

decl:
    | e=expr ";" { e }

expr:
    | n=JINT { Eint n }
    | e1=expr "+" e2=expr { Ebinop (Ar(Plus), e1, e2) }
    | e1=expr "*" e2=expr { Ebinop (Ar(Times), e1, e2) }
