type ident = string

type compare_op = Equal | Different | Infeq | Inf | Supeq | Sup

type bool_op = And | Or 

type arith_op = Plus | Minus | Times | Div | Exp

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
  (*| Efun of func
  | Estruct of structure*)

and bloc = expr list

and param = ident (*en fait il faudra gérer les déclarations de type en temps et en heure*)

and func = { fname : ident ; fpar : param list ; instr : expr list }

and structure = { mut : bool ; sname : ident ; spar : param list}

type file = expr list 
