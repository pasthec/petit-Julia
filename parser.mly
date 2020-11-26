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

%nonassoc WHILE FOR
%nonassoc RETURN
%nonassoc IF

%right "="
%left "||"
%left "&&"
%left "==" "!=" "<=" "<" ">=" ">" 
%left "+" "-"
%left "*" "%" "/" IDIV
%nonassoc "!" uminus
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

    | n=int_lpar e=bloc1 ")" {{desc=Ebinop (Ar(Times), n, e);loc=($startpos,$endpos)}}
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
    
    | "(" b=bloc1 ")" {b}

    | "-" e=expr %prec uminus { {desc=Eminus e;loc=($startpos,$endpos)}}
    
    | TRUE {{desc=Ebool true;loc=($startpos,$endpos)}}
    | FALSE {{desc=Ebool false;loc=($startpos,$endpos)}}
    | e1=expr "&&" e2=expr {{desc=Ebinop (Bop(And),e1,e2);loc=($startpos,$endpos)}}
    | e1=expr "||" e2=expr {{desc=Ebinop (Bop(Or),e1,e2);loc=($startpos,$endpos)}}
    | "!" e=expr {{desc=Enot e;loc=($startpos,$endpos)}}

    | IF c=expr_bloc el=else_stmt { {desc=IfElse(fst c,snd c,el);loc=($startpos,$endpos)} }

    | s=IDENT_LPAR arg=separated_list(",",expr) ")" {
        match s,arg with
            |"div",e1::[e2] -> {desc=Ebinop (Ar(Div), e2, e1);loc=($startpos,$endpos)} 
            (*fonction div transformée en l'opérateur de division*)
            |"div", _ -> assert false (*mauvais nombre d'arguments pour div*)
            |"println", _ -> let en= {desc=Estring "\n"; loc=($startpos,$endpos)} in 
                    (*on gère le sucre syntaxique pour println*)
                            {desc=Ecall ("print",(arg @ [en]));loc=($startpos,$endpos)}        
        
        
            |_,_ -> {desc=Ecall (s,arg); loc=($startpos,$endpos)}}

    
    | RETURN e=ioption(expr) {{desc=Ereturn e; loc=($startpos,$endpos)}} /*j'ai très peur des conflits que ça va provoquer*/

    
    | FOR x=IDENT "=" e1=expr ":" c=expr_bloc END {{desc=Efor(x,e1,fst c,snd c); loc=($startpos,$endpos)}}
    | e=while_c b=bloc END {{desc=Ewhile (e,b); loc=($startpos,$endpos)}}

while_c:
    WHILE e=expr {e} /*je suis désespéré*/

expr_bloc:
    e=expr b=bloc {(e,b)} /*a réglé certains conflits par magie, n'en règle pas d'autres pour la même raison*/


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

    | ELSEIF c=expr_bloc el=else_stmt { [{desc=IfElse (fst c,snd c,el);loc=($startpos,$endpos)}] }

bloc:
    | ";"* {[]} /*fin de bloc avec autant de ; que nécessaire, y compris aucun
                        inclut le bloc vide*/
    | ";"* e=expr ";" b=bloc {e::b} /*on peut avoir autant de ; que nécessaire avant, et forcément un à la fin, 
                                        le bloc peut continuer après*/
    | ";"* e=expr { [e] } /*fin éventuelle de liste sans point-virgule derrière*/
 
bloc1: 
    | e=expr {e}
    | e=expr ";" b=bloc {{desc=Eblock (e::b); loc=($startpos,$endpos)}}

func:
      FUNCTION f=IDENT_LPAR p=separated_list(",",param) ")" b=bloc END {
        {fname=f ; fpar=p; ftype=Tany ; finstr= b}}
    |FUNCTION f=IDENT_LPAR p=separated_list(",",param) ")" "::" t=IDENT b=bloc END {
        {fname=f ; fpar=p; ftype=Ast.type_of_string t ; finstr= b}}

structure:
      STRUCT s=IDENT p=param_list END {
        {smut=false; sname=s; spar= p}
    }

    | MUTABLE STRUCT s=IDENT p=param_list END {
        {smut=true; sname=s; spar=p}
    }

param:
     s=IDENT {{pname=s; ptype=Tany}} /*sucre syntaxique : l'omission d'un type équivaut au type Any*/
    |s=IDENT "::" t=IDENT {{pname=s; ptype=Ast.type_of_string t}}


param_list:
      ";"* {[]}
    | ";"* p=param ";" l=param_list {p::l}
    | ";"* p=param {[p]}