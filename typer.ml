

exception Type_error of Ast.loc*string*string

(*type donné, type attendu*)

exception Unbound_value of Ast.loc*string

(*variable non définie*)

(*fichier qui type

fonction typing f : prend en argument l'objet de type Ast.file du parseur*)