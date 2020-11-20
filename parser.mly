/* Analyseur syntaxique pour petit Julia */

%{
    open Ast
    open Parsing
%}

%token EOF
%token PLUS "+"
%token MINUS "-"
%token TIMES "*" 
%token DIV "/"
%token IDIV

%token ELSE ELSEIF END FALSE FOR FUNCTION IF MUTABLE RETURN STRUCT TRUE WHILE
%token AND "&&" 
%token OR "||"
%token NOT "!"
%token <int> JINT
%token <string> IDENT
%token <int*string> INT_IDENT
%token <int> INT_LPAR
%token <string> RPAR_IDENT
%token LPAR "("
%token RPAR ")"
%token SEMICOLON ";"



%start file

%type <Ast.file> file

%left "||"
%left "&&"
%left "+" "-"
%left "*" "/" IDIV
%nonassoc "!"

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
    | "(" e=expr s=RPAR_IDENT {Ebinop (Ar(Times),e,Evar s)}
    
    | e1=expr "+" e2=expr { Ebinop (Ar(Plus), e1, e2) }
    | e1=expr "*" e2=expr { Ebinop (Ar(Times), e1, e2) }
    | e1=expr "-" e2=expr { Ebinop (Ar(Minus), e1, e2) }
    | e1=expr "/" e2=expr { Ebinop (Ar(Div), e1, e2) }
    | e1=expr IDIV e2=expr { Ebinop (Ar(Div), e2, e1) }
    
    | "(" e=expr ")" {e}
    | "-" e=expr { Eminus e}
    
    | TRUE {Ebool true}
    | FALSE {Ebool false}
    | e1=expr "&&" e2=expr {Ebinop (Bop(And),e1,e2)}
    | e1=expr "||" e2=expr {Ebinop (Bop(Or),e1,e2)}
    | "!" e=expr {Enot e}

    | IF e=expr b=bloc el=else_stmt { IfElse(e,b,el) }

else_stmt:
    | END { [] }
    | ELSE b=bloc END { b }
    | ELSEIF e=expr b=bloc el=else_stmt { [IfElse (e,b,el)] }

bloc:
    | b=separated_list(";",expr) { b }
