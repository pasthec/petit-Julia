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
%token <string> IDENT
%token <int*string> INT_IDENT
%token <int> INT_LPAR
%token LPAR "("
%token RPAR ")"
%token SEMICOLON ";"


%start file

%type <Ast.file> file

%left "*"
%left "+" "-"

%%

file:
    | d=decl* EOF { d }

decl:
    | e=expr ";" { e }

expr:
    | n=JINT { Eint n }
    | s=IDENT {Evar s}
    | c=INT_IDENT {Ebinop (Ar(Times), Eint (fst c), Evar (snd c))}
    | n=INT_LPAR e=expr ")" {Ebinop (Ar(Times), Eint n, e)}
    | e1=expr "+" e2=expr { Ebinop (Ar(Plus), e1, e2) }
    | e1=expr "*" e2=expr { Ebinop (Ar(Times), e1, e2) }
    | e1=expr "-" e2=expr { Ebinop (Ar(Minus), e1, e2) }
    | "(" e=expr ")" {e}
