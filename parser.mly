/* Analyseur syntaxique pour petit Julia */

%{
    open Ast
    open Parsing

exception Parse_error

%}

%token EOF
%token PLUS "+"
%token MINUS "-"
%token TIMES "*" 
%token DIV "/"
%token IDIV
%token EXP "^"

%token ELSE ELSEIF END FALSE FOR FUNCTION IF MUTABLE RETURN STRUCT TRUE WHILE
%token AND "&&" 
%token OR "||"
%token NOT "!"
%token BEQUAL "=="
%token EQUAL "="
%token DIFFERENT "!="
%token INFEQ "<="
%token INF "<"
%token SUPEQ ">="
%token SUP ">"
%token <int> JINT
%token <string> IDENT
%token <string> JSTRING
%token <int*string> INT_IDENT
%token <int> INT_LPAR
%token <string> RPAR_IDENT
%token <string> IDENT_LPAR
%token LPAR "("
%token RPAR ")"
%token SEMICOLON ";"
%token COLON ":"
%token DOUBLE_COLON "::"
%token COMMA ","
%token DOT "."
%token MOD "%"



%start file

%type <Ast.file> file

%nonassoc RETURN
%right "="
%left "||"
%left "&&"
%left "==" "!=" "<=" "<" ">=" ">" 
%left "+" "-"
%left "*" "%" "/" IDIV
%nonassoc "!"
%right "^"
%left "."

%%

file:
    | d=decl* EOF { d }

decl:
    | e=expr ";" { e }
    |f=func ";" {{desc=Efun f;loc=($startpos,$endpos)}}
    |s=structure ";" {{desc=Estruct s;loc=($startpos,$endpos)}}

expr:
    | n=JINT { {desc=Eint n ; loc=($startpos,$endpos)} }
    | v=value { v } /*les valeurs gauches seules sont aussi des expressions*/

    | v=value "=" e=expr {{desc=Eaffect(v,e);loc=($startpos,$endpos)}} /*affectation*/

    /*$startpost et $endpos sont des objets de type Lexing.position, information fournie par le lexeur*/
    
    | c=INT_IDENT {let n={desc=Eint (fst c);loc=($startpos,$endpos)} 
        and s={desc=Evar (snd c); loc=($startpos,$endpos)}
        in 
        {desc=Ebinop (Ar(Times), n, s);loc=($startpos,$endpos)}}

    | n=int_lpar e=expr ")" {{desc=Ebinop (Ar(Times), n, e);loc=($startpos,$endpos)}}
    | "(" e=expr s=rpar_ident {{desc=Ebinop (Ar(Times),e,s);loc=($startpos,$endpos)}}
    
    | e1=expr "+" e2=expr { {desc=Ebinop (Ar(Plus), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "*" e2=expr { {desc=Ebinop (Ar(Times), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "-" e2=expr { {desc=Ebinop (Ar(Minus), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "/" e2=expr { {desc=Ebinop (Ar(Div), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr IDIV e2=expr { {desc=Ebinop (Ar(Div), e2, e1);loc=($startpos,$endpos)} }
    | e1=expr "%" e2=expr { {desc=Ebinop (Ar(Mod), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "^" e2=expr { {desc=Ebinop (Ar(Exp), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "==" e2=expr { {desc=Ebinop (Comp(Equal), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "!=" e2=expr { {desc=Ebinop (Comp(Different), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "<=" e2=expr { {desc=Ebinop (Comp(Infeq), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "<" e2=expr { {desc=Ebinop (Comp(Inf), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr ">=" e2=expr { {desc=Ebinop (Comp(Supeq), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr ">" e2=expr { {desc=Ebinop (Comp(Sup), e1, e2);loc=($startpos,$endpos)} }

    | s=JSTRING {{desc=Estring s;loc=($startpos,$endpos)}}
    
    | "(" e=expr ")" {e}
    | "-" e=expr { {desc=Eminus e;loc=($startpos,$endpos)}}
    
    | TRUE {{desc=Ebool true;loc=($startpos,$endpos)}}
    | FALSE {{desc=Ebool false;loc=($startpos,$endpos)}}
    | e1=expr "&&" e2=expr {{desc=Ebinop (Bop(And),e1,e2);loc=($startpos,$endpos)}}
    | e1=expr "||" e2=expr {{desc=Ebinop (Bop(Or),e1,e2);loc=($startpos,$endpos)}}
    | "!" e=expr {{desc=Enot e;loc=($startpos,$endpos)}}

    | IF e=expr b=bloc el=else_stmt { {desc=IfElse(e,b,el);loc=($startpos,$endpos)} }

    | s=IDENT_LPAR arg=separated_list(",",expr) {{desc=Ecall (s,arg); loc=($startpos,$endpos)}}

    | RETURN {{desc= Ereturn None; loc=($startpos,$endpos)}}
    | RETURN e=expr {{desc=Ereturn (Some e); loc=($startpos,$endpos)}} /*j'ai très peur des conflits que ça va provoquer*/

    | FOR x=IDENT "=" e1=expr ":" e2=expr b=bloc END {{desc=Efor(x,e1,e2,b); loc=($startpos,$endpos)}}
    | WHILE e=expr b=bloc END {{desc=Ewhile(e,b); loc=($startpos,$endpos)}}





value: /*valeurs gauches*/
      s=IDENT {{desc=Evar s; loc=($startpos,$endpos)}}
    | e=expr "." s=IDENT {{desc=Earg (e,s);loc=($startpos,$endpos)}}


int_lpar:
    n=INT_LPAR {{desc= Eint n;loc=($startpos,$endpos)}} /*pour récupérer la position de n*/

rpar_ident:
    s=RPAR_IDENT {{desc=Evar s;loc=($startpos,$endpos)}} /*de même*/

else_stmt:
    | END { [] }
    | ELSE b=bloc END { match b with
            |[] ->b
            |e::q->begin match e.desc with
                    |IfElse(_,_,_) -> assert false (*le bloc commence par un if, on a donc else if, 
                    c'est une erreur de syntaxe*)
                    |_ ->b end}

    | ELSEIF e=expr b=bloc el=else_stmt { [{desc=IfElse (e,b,el);loc=($startpos,$endpos)}] }

bloc:
    | b=separated_list(";",expr) { b }

func:
      FUNCTION f=IDENT_LPAR p=separated_list(",",param) b=bloc END {
        {fname=f ; fpar=p; ftype=Tany ; finstr= b}}
    |FUNCTION f=IDENT_LPAR p=separated_list(",",param) "::" t=IDENT b=bloc END {
        {fname=f ; fpar=p; ftype=Ast.type_of_string t ; finstr= b}}

structure:
      STRUCT s=IDENT p=separated_list(";",param) END {
        {smut=false; sname=s; spar=p}
    }

    | MUTABLE STRUCT s=IDENT p=separated_list(";",param) END {
        {smut=true; sname=s; spar=p}
    }

param:
     s=IDENT {{pname=s; ptype=Tany}} /*sucre syntaxique : l'omission d'un type équivaut au type Any*/
    |s=IDENT "::" t=IDENT {{pname=s; ptype=Ast.type_of_string t}}