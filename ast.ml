type ident = string

type compare_op = Equal | Different | Infeq | Inf | Supeq | Sup

type bool_op = And | Or 

type arith_op = Plus | Minus | Times | Div | Exp

type binop = Comp of compare_op | Bop of bool_op | Ar of arith_op 
(*opérateurs binaires, je sépare pour que ça soit peut-etre plus simple pour l'interpréteur ? *)

type expr =
    Eint of int 
  | Estring of string
  | Ebool of bool
  | Evar of ident 
  | Ebinop of binop*expr*expr
  | Enot of expr 
  | Eminus of expr
  | IfElse of expr*bloc*bloc

and bloc = expr list

type param = ident (*en fait il faudra gérer les déclarations de type en temps et en heure*)

type func = { name : ident ; par : param list ; instr : expr list }

type structure = { mut : bool ; name : ident ; par : param list}

type decl = expr

type file = decl list 
