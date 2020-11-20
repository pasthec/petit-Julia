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
    /*|f=function {{desc=Efun f;loc=($startpos,$endpos)}}
    |s=structure {{desc=Estruct s;loc=($startpos,$endpos)}}*/

expr:
    | n=JINT { {desc=Eint n ; loc=($startpos,$endpos)} }
    | s=IDENT {{desc=Evar s ; loc=($startpos,$endpos)} }

    /*$startpost et $endpos sont des objets de type Lexing.position, information fournie par le lexeur*/
    
    | c=INT_IDENT {let n={desc=Eint (fst c);loc=($startpos,$endpos)} 
        and s={desc=Evar (snd c); loc=($startpos,$endpos)}
        in 
        {desc=Ebinop (Ar(Times), n, s);loc=($startpos,$endpos)}}
        /*solution peut-être provisoire ? : l'entier et l'ident sont donnés comme ayant la même 
        localisation que tout l'entier ident
        est-ce vraiment gênant ? */
    | n=int_lpar e=expr ")" {{desc=Ebinop (Ar(Times), n, e);loc=($startpos,$endpos)}}
    | "(" e=expr s=rpar_ident {{desc=Ebinop (Ar(Times),e,s);loc=($startpos,$endpos)}}
    
    | e1=expr "+" e2=expr { {desc=Ebinop (Ar(Plus), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "*" e2=expr { {desc=Ebinop (Ar(Times), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "-" e2=expr { {desc=Ebinop (Ar(Minus), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr "/" e2=expr { {desc=Ebinop (Ar(Div), e1, e2);loc=($startpos,$endpos)} }
    | e1=expr IDIV e2=expr { {desc=Ebinop (Ar(Div), e2, e1);loc=($startpos,$endpos)} }
    
    | "(" e=expr ")" {e}
    | "-" e=expr { {desc=Eminus e;loc=($startpos,$endpos)}}
    
    | TRUE {{desc=Ebool true;loc=($startpos,$endpos)}}
    | FALSE {{desc=Ebool false;loc=($startpos,$endpos)}}
    | e1=expr "&&" e2=expr {{desc=Ebinop (Bop(And),e1,e2);loc=($startpos,$endpos)}}
    | e1=expr "||" e2=expr {{desc=Ebinop (Bop(Or),e1,e2);loc=($startpos,$endpos)}}
    | "!" e=expr {{desc=Enot e;loc=($startpos,$endpos)}}

    | IF e=expr b=bloc el=else_stmt { {desc=IfElse(e,b,el);loc=($startpos,$endpos)} }

int_lpar:
    n=INT_LPAR {{desc= Eint n;loc=($startpos,$endpos)}} /*je fais ça pour récupérer la position de n*/

rpar_ident:
    s=RPAR_IDENT {{desc=Evar s;loc=($startpos,$endpos)}} /*de même*/

else_stmt:
    | END { [] }
    | ELSE b=bloc END { b }
    | ELSEIF e=expr b=bloc el=else_stmt { [{desc=IfElse (e,b,el);loc=($startpos,$endpos)}] }

bloc:
    | b=separated_list(";",expr) { b }

/*function:

structure:*/
