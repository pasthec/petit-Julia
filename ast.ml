type ident=string

type compare_op = Equal | Different | Infeq | Inf | Supeq | Sup

type bool_op = And | Or 

type arith_op= Plus | Minus | Times | Exp

type binop= Comp of compare_op | Bol of bool_op | Ar of arith_op 
(*opérateurs binaires, je sépare pour que ça soit peut-etre plus simple pour l'interpréteur ? *)


type expr= 
  Eint of int 
  |Estring of string
  |Ebool of bool
  |Evar of ident 
  |Ebinop of binop*expr*expr
  |Enot of expr 





type param=ident (*en fait il faudra gérer les déclarations de type en temps et en heure*)

type function={ name=ident ; par = param list ; instr = expr list }

type structure={ mut : bool ; name : ident ; par = param list}

type file = decl list 