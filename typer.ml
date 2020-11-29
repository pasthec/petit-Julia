open Ast

exception Type_error of Ast.loc*string*string

(*type donné, type attendu*)

exception Error of Ast.loc*string

let error (loc,s)= 
  raise (Error (loc,s))

(*variable non définie*)

(*fichier qui type

fonction typing f : prend en argument l'objet de type Ast.file du parseur*)

module Smap=Map.Make(String)



type tfun={tfarg : typ list ; tfres : typ}

type tstruct={tspar : typ Smap.t}

let type_of_string s=
  match s with
   "Int64" -> Tint64
  |"Any" -> Tany
  |"Nothing" -> Tnothing
  |"Bool" -> Tbool
  |"String" -> Tstring
  |_ -> Tstruct s

let first_search_fun f funs structs=

  (*vérifie que les types arguments et résultats de f sont bien formés, sachant quelles sont les structures déjà déclarées
  et renvoie le nouveau dictionnaire des fonctions
  
  En fait c'est faux, plusieurs fonctions peuvent avoir le même nom
  
  Les lier à des listes ? *)

  let well_formed t loc= (*vérifie qu'un type t dans une déclaration de fonction est bien formé
                        sachant que les structures déjà déclarées sont s
                        ne fait rien si oui, lève une erreur sinon*)
    match t with 
      | Tstruct u when Smap.mem u structs ->()
      | Tstruct u -> error (loc,"unbound type"^u) (*erreur à préciser et localiser*)
      | _ ->() 

  in 
  if List.mem (f.fname) ["print";"println";"div"] then error (f.floc,(f.fname )^" is a Julia reserved function.") ;
                    (*erreur à préciser et localiser, will do*)
  let l=List.map (fun p-> well_formed (p.ptype) (p.ploc); p.ptype ) (f.fpar)

                (*on vérifie que tous les types des arguments de f sont bien formés et on renvoie la liste
                de leurs types le cas échéants*)

  in well_formed f.ftype (f.floc); (*même vérification pour le type de retour*)
                
  (Smap.add (f.fname) {tfarg=l ; tfres = f.ftype} funs)


let first_search_struct s structs fields_of_structs= 

  (*appliqué à une structure s, sachant que les structures précédentes sont structs et les champs fields_of_struct
  vérifie que les champs de s sont bien formés et renvoie les nouveaux structs et fields_of_structs*)

  let well_formed t loc =
    (*si t bien formé, on ne fait rien, sinon on lève une erreur*)
    match t with 
    | Tstruct u when (u=s.sname || Smap.mem u structs) -> ()
    | Tstruct u -> error (loc,"unbound type"^u)
    | _ -> ()

  in 

  let expl_param (sargs,fields) p =
    (*ajoute le paramètre p et son type au dictionnaire des paramètres de s, 
        et (s,type(p)) au dictionnaire des structures possédant le champ p *)

      well_formed (p.ptype) (p.ploc);

      if Smap.mem (p.pname) fields then error(p.ploc,"already used field "^(p.pname));

    (*on n'a le droit qu'à une structure par nom de champ*)

      (Smap.add (p.pname) (p.ptype) sargs,Smap.add (p.pname) (s.sname) fields)

  in let sargs,fields=List.fold_left expl_param (Smap.empty,fields_of_structs) s.spar

 in Smap.add (s.sname) sargs structs,fields


let first_search ast= (*construit l'environnement constitué des structures, 
    des déclarations de fonctions et des variables globales*)

    let vars=Smap.singleton "nothing" Tnothing and funs=Smap.empty and structs=Smap.empty 
    (*variables globales, fonctions et structures*)
    and fields_of_structs= Smap.empty in (*champs apparaissant dans des structures*)

    
    let search_decl (vars,funs,structs,fields_of_structs) d=
       match d with 
        | Func f -> vars, first_search_fun f funs structs, structs, fields_of_structs
        | Struct s -> let c =first_search_struct s structs fields_of_structs in 
                      vars, funs, fst c, snd c

        | Expr e -> (vars,funs,structs,fields_of_structs) (*pour que ça compile, c'est faux bien sûr*)

    in 
    List.fold_left search_decl (vars,funs,structs,fields_of_structs) ast

let typing f=
  let env=first_search f in 

  Printf.printf "first step done\n";;
