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
module Sset=Set.Make(String)


type tfun={tfarg : (string*typ) list ; tfres : typ}

type tstruct={tsmut : bool; tsfields : (string*typ) list }

let type_of_string s=
  match s with
   "Int64" -> Tint64
  |"Any" -> Tany
  |"Nothing" -> Tnothing
  |"Bool" -> Tbool
  |"String" -> Tstring
  |_ -> Tstruct s

let string_of_type t =
  match t with
  | Tint64 -> "Int64"
  | Tany -> "Any"
  | Tnothing -> "Nothing"
  | Tbool -> "Bool"
  | Tstring -> "String"
  | Tstruct s -> s

let first_search_fun f funs structs=

  (*vérifie que les types arguments et résultats de f sont bien formés, sachant quelles sont les structures déjà déclarées
  et renvoie le nouveau dictionnaire des fonctions
   *)

  if Smap.mem (f.fname) structs then error (f.floc,f.fname^" is already a structure. It cannot denote a function.");

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
  
  let rec expl_param set p_l=
    match p_l with 
    | [] ->[]
    | p::q -> well_formed (p.ptype) (p.ploc);
            if Sset.mem (p.pname) set then error (p.ploc,"multiple arguments with name "^p.pname);
            (p.pname,p.ptype)::(expl_param (Sset.add p.pname set) q)

  in let l=expl_param (Sset.empty) (f.fpar)


                (*on vérifie que tous les types des arguments de f sont bien formés et on renvoie la liste
                de leurs types le cas échéants*)

  in well_formed f.ftype (f.floc); (*même vérification pour le type de retour*)
  
  let compare_entry_types fp = (*renvoie une erreur si fp a exactement les mêmes types d'entrée que f*)
    
    try 
      if not(List.exists2 (fun t1 -> fun t2 -> t1<>t2) l (fp.tfarg) ) then 
        error (f.floc,f.fname^" is already a function with same entry types.")
    with Invalid_argument _ -> ()

  in 
  try let fs=Smap.find (f.fname) funs in
      List.iter compare_entry_types fs ;
      Smap.add (f.fname) ({tfarg=l ; tfres = f.ftype}::fs) funs
  with Not_found -> (Smap.add (f.fname) [{tfarg=l ; tfres = f.ftype}] funs)

  (*soit on a déjà une fonction de même nom et on ajoute celle-là, soit on crée la lsite singleton*)

let first_search_struct s funs structs fields_of_structs= 

  (*appliqué à une structure s, sachant que les structures précédentes sont structs et les champs fields_of_struct
  vérifie que les champs de s sont bien formés et renvoie les nouveaux structs et fields_of_structs*)
  if Smap.mem (s.sname) structs then error (s.sloc,"multiple structures with name "^(s.sname));

  if Smap.mem (s.sname) funs then error (s.sloc,s.sname^" is already a function. It cannot be a structure.");

  let well_formed t loc =
    (*si t bien formé, on ne fait rien, sinon on lève une erreur*)
    match t with 
    | Tstruct u when (u=s.sname || Smap.mem u structs) -> ()
    | Tstruct u -> error (loc,"unbound type "^u)
    | _ -> ()

  in 

  let rec expl_param fields p_l =
    (*ajoute le paramètre p et son type au dictionnaire des paramètres de s, 
        et (s,type(p)) au dictionnaire des structures possédant le champ p *)
    match p_l with 
      | [] ->[],fields
      | p::q -> 
              well_formed (p.ptype) (p.ploc);

              if Smap.mem (p.pname) fields then error(p.ploc,"already used field "^(p.pname));

              (*on n'a le droit qu'à une structure par nom de champ*)
            let l,f=expl_param (Smap.add (p.pname) (s.sname) fields) q in 

            (p.pname,p.ptype)::l,f
     

  in let sargs,fields=expl_param fields_of_structs s.spar

 in Smap.add (s.sname) {tsmut=s.smut; tsfields= sargs} structs,fields


let const_type e vars=
    match e.desc with
  | Eint _ -> Tint64
  | Estring _ -> Tstring 
  | Ebool _ -> Tbool
  | Evar x -> begin try Smap.find x vars 
              with Not_found ->error(e.loc,"unbound variable "^x) end 

  | _ -> Tany


let rec first_search_expr vars e=
    match e.desc with 
    | Eaffect (e1,e2) -> begin match e1.desc with 
                        | Evar x when not(Smap.mem x vars)-> Smap.add x (const_type e2 vars) vars 
                        | Evar x -> (let t =Smap.find x vars in 
                                    match t,const_type e2 vars with 
                                    | Tany,t2 -> Smap.add x (t2) vars 
                                    | _, Tany -> vars 
                                    | _,t2 when t=t2 -> vars 
                                    | _,t2 -> raise (Type_error(e2.loc,string_of_type t2,string_of_type t)))
                        | _ -> vars end 
    | Eblock b-> List.fold_left first_search_expr vars b 
    | _ -> vars 

let first_search ast= (*construit l'environnement constitué des structures, 
    des déclarations de fonctions et des variables globales*)

    let vars=Smap.singleton "nothing" Tnothing and funs=Smap.empty and structs=Smap.empty 
    (*variables globales, fonctions et structures*)
    and fields_of_structs= Smap.empty in (*champs apparaissant dans des structures*)

    
    let search_decl (vars,funs,structs,fields_of_structs) d=
       match d with 
        | Func f -> vars, first_search_fun f funs structs, structs, fields_of_structs
        | Struct s -> let c =first_search_struct s funs structs fields_of_structs in 
                      vars, funs, fst c, snd c

        | Expr e -> first_search_expr vars e,funs,structs,fields_of_structs (*pour que ça compile, c'est faux bien sûr*)

    in 
    List.fold_left search_decl (vars,funs,structs,fields_of_structs) ast

let compatible t t' =
    t=Tany || t'=Tany || t=t'

let rec local_env env b = env (* on construit l'environnement à l'intérieur de f *)
    (* maintenant qu'on sait à quoi ça sert il faut le faire *)

let type_expr env e = Tany (* donne le type d'une expression *)

let rec type_block env b = (* donne le type d'un bloc *)
    let (vars,funs,structs,fields_of_structs) = env in
    match b with
    | [] -> Tnothing
    | [e] -> type_expr env e
    | e::q ->
            begin match e.desc with
                | Eaffect({desc=Evar x; loc=_},f) -> let t = type_expr env f in
                  type_block (Smap.add x (type_expr env f) vars,funs,structs,
                                                           fields_of_structs) q
                | _ -> type_expr env e
            end

let check_returns t b = () (* vérifie que les return d'un bloc sont bien de type t *)

let check_func (vars,funs,structs,fields_of_structs) f = 
    
    let rec add_params vars = function
        | [] -> vars
        | p::q -> Smap.add p.pname p.ptype (add_params vars q)
    in

    let envf = local_env (add_params vars f.fpar,funs,structs,fields_of_structs)
    f.finstr in

    let tb = type_block envf f.finstr in
    let tres = (Smap.find f.fname funs).tfres in
    if not (compatible tb tres) then
        error (f.floc, "Function "^(f.fname)^
        " expected to have some type but has another")

    (* à remplacer par Type_error (loc,given,expected) une fois qu'on aura un
     * string_of_type *)

    check_returns tres f.finstr 

let check_expr env e = ()

let second_search ast env = (*On type le corps des fonctions et des expressions et on
                              vérifie qu'ils conviennent avec l'environnement global *)
    let check_decl env d = match d with
        | Func f -> check_func env f
        | Expr e -> check_expr env e
        | _ -> ()

   in List.iter (check_decl env) ast

let typing f=
  let env=first_search f in 

  Printf.printf "first step done\n";;
