type ident = string

type compare_op = Equal | Different | Infeq | Inf | Supeq | Sup

type bool_op = And | Or 

type arith_op = Plus | Minus | Times | Div | Exp | Mod 

type binop = Comp of compare_op | Bop of bool_op | Ar of arith_op 
(*opérateurs binaires, je sépare pour que ça soit peut-etre plus simple pour l'interpréteur ? *)

type loc= Lexing.position * Lexing.position

type expr ={desc : desc; loc : loc}

and desc =
    Eint of int 
  | Estring of string
  | Ebool of bool
  | Evar of ident 
  | Ebinop of binop*expr*expr
  | Enot of expr 
  | Eminus of expr
  | IfElse of expr*bloc*bloc
  | Efun of func (*déclaration de fonction*)
  | Estruct of structure (*déclaration de type*)
  | Ecall of ident*expr list (*appel de fonction*)
  | Earg of expr*ident (*champ d'une structure, Earg (e,x)=e.x *)
  | Eaffect of expr*expr (*affectation e1=e2, l'analyseur syntaxique garantit que e1 est une valeur gauche*)
  | Ereturn of expr option (*juste return ou return e*)
  | Efor of ident*expr*expr*bloc (*variable,début,fin,instructions*)
  | Ewhile of expr*bloc (*condition,instructions*)
  | Eblock of bloc 

and bloc = expr list

and param = {pname : ident; ptype : typ }

and typ = 
    Tint64 
  | Tnothing
  | Tbool
  | Tstring
  | Tany
  | Tstruct of string
  | Tfun of typ list*typ list

and func = { fname : ident ; fpar : param list ; ftype : typ; finstr : expr list }

and structure = { smut : bool ; sname : ident ; spar : param list}

type file = expr list 

let type_of_string s=
  match s with
   "Int64" -> Tint64
  |"Any" -> Tany
  |"Nothing" -> Tnothing
  |"Bool" -> Tbool
  |"String" -> Tstring
  |_ -> Tstruct s