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


type tfun={tfarg : (string*typ) list ; tfres : typ; tfinstr : bloc; tfloc : loc}



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

let terror (loc,g,e)=
  raise (Type_error(loc,string_of_type g,string_of_type e))

type tbinding={mutable t : typ }

type texpr = {ttype : typ ; tdesc : tdesc}

and tdesc =
  TEint of Int64.t 
| TEstring of string
| TEbool of bool
| TEvar of ident 
| TEbinop of binop*texpr*texpr
| TEnot of texpr 
| TEminus of texpr
| TIfElse of texpr*texpr*texpr (*les bloc sont devenus des expr TEblock (pour leur donner un type) *)
| TEcallf of ident*int list*texpr list (*appel de fonction*)
| TEcalls of ident*texpr list (*création de structure*)
| Tprint of texpr list (*le print est une fonction à part*)
| TEarg of texpr*ident (*champ d'une structure, Earg (e,x)=e.x *)
| TEaffect of texpr*texpr (*affectation e1=e2, l'analyseur syntaxique garantit que e1 est une valeur gauche*)
| TEreturn of texpr option (*juste return ou return e*)
| TEfor of ident*texpr*texpr*texpr (*variable,début,fin,instructions*)
| TEwhile of texpr*texpr (*condition,instructions*)
| TEblock of texpr list*tbinding Smap.t

type tfunr={tfargr : (string*typ) list; tfresr : typ; tfinstrr : texpr}

type tdecl=Te of texpr | Tf of ident*param list | Ts of structure

type tfile=tdecl list* tfunr Smap.t * tstruct Smap.t

let first_search_fun f funs structs=

  (*vérifie que les types arguments et résultats de f sont bien formés, sachant quelles sont les structures déjà déclarées
  et renvoie le nouveau dictionnaire des fonctions
   *)

  if Smap.mem (f.fname) structs then error (f.floc,f.fname^" is already a structure. It cannot denote a function.");

  let well_formed t loc= (* vérifie qu'un type t dans une déclaration de fonction est
                            bien formé sachant que les structures déjà déclarées sont
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
      Smap.add (f.fname) ({tfarg=l ; tfres = f.ftype; tfinstr= f.finstr; tfloc=f.floc}::fs) funs
  with Not_found -> (Smap.add (f.fname) [{tfarg=l ; tfres = f.ftype;tfinstr= f.finstr; tfloc=f.floc}] funs)

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
            let l,f=expl_param (Smap.add (p.pname) (s.sname,p.ptype) fields) q in 

            (p.pname,p.ptype)::l,f
     

  in let sargs,fields=expl_param fields_of_structs s.spar

 in Smap.add (s.sname) {tsmut=s.smut; tsfields= sargs} structs,fields


(*let const_type e vars=
    match e.desc with
  | Eint _ -> Tint64
  | Estring _ -> Tstring 
  | Ebool _ -> Tbool
  | Evar x -> begin try Smap.find x vars 
              with Not_found ->error(e.loc,"unbound variable "^x) end 

  | _ -> Tany*)


let rec first_search_expr vars e=
    match e.desc with 
    | Eaffect (e1,e2) -> let loc_p=first_search_expr vars e2 in 
      
                        begin match e1.desc with 
                        | Evar x when not(Smap.mem x loc_p)-> Smap.add x {t=Tany} loc_p
                        (*| Evar x -> (let t =Smap.find x vars in 
                                    match t,const_type e2 vars with 
                                    | Tany,t2 -> Smap.add x (t2) vars 
                                    | _, Tany -> vars 
                                    | _,t2 when t=t2 -> vars 
                                    | _,t2 -> raise (Type_error(e2.loc,string_of_type t2,string_of_type t)))*)
                        | _ -> loc_p end 
    | Eblock b-> List.fold_left first_search_expr vars b 
    | IfElse (e1,b1,b2) -> let v1= first_search_expr vars e1 in 
                          let v2=List.fold_left first_search_expr v1 b1 in 
                          List.fold_left first_search_expr v2 b2 
    | Ebinop (_,e1,e2) -> List.fold_left first_search_expr vars [e1;e2]
    | Enot e1 -> first_search_expr vars e1 
    | Eminus e1 -> first_search_expr vars e1 
    | Ereturn (Some e1) -> first_search_expr vars e1  
    | _ -> vars 

let first_search ast= (*construit l'environnement constitué des structures, 
    des déclarations de fonctions et des variables globales*)

    let vars=Smap.singleton "nothing" {t=Tnothing} and funs=Smap.empty and structs=Smap.empty 
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

let assert_compatible t t_c t_c_loc= (*type qu'on veut, type que l'on a, localisation de l'expression*)

  (*lève une erreur si lexpression de type t_c situé à la localisation t_c_loc n'est pas compatible avec le type t*)
  if not(compatible t t_c) then terror(t_c_loc,t_c,t)

 
let loc_env loc_sup b l = 
  (*va récursivement chercher les variables locales déclarées au niveau k connaissant celles au niveau k-1 loc_sup
   Si une variable est déjà locale au niveau k-1, on ne l'ajoute pas
   les variables globales ne sont pas locales au niveau 0
   l est un argument muet de type loc qui permet que l'appel récursif initial soit de bon type*)

  let rec local_env loc e =
  
    match e.desc with 
    | Eaffect (e1,e2) -> let loc_p=local_env loc e2 in 
      
                        begin match e1.desc with 
                        | Evar x when not(Smap.mem x loc_sup)-> Smap.add x {t=Tany} loc_p 

                        | _ -> loc_p end

    | Eblock b1-> List.fold_left local_env loc b1 
    | IfElse (e1,b1,b2) -> let v1= local_env loc e1 in 
                          let v2=List.fold_left local_env v1 b1 in 
                          List.fold_left local_env v2 b2 
    | Ebinop (_,e1,e2) -> List.fold_left local_env  loc [e1;e2]
    | Enot e1 -> local_env loc e1 
    | Eminus e1 -> local_env loc e1
    | Ereturn (Some e1) -> local_env loc e1
    | _ -> loc

  in local_env Smap.empty {loc=l;desc=Eblock b}

let fc k v1 v2 = Some v1 (*fonction nécessaire comme argument de l'union
                    sert pour le cas d'égalité, mais inutile car les environnements 
                    que l'on unit sont par construction disjoints*)

let rec type_expr t_return env e = (*renvoie la nouvelle expression avec son type dans le nouvel ast*)
  (*t_return est le type avec lequel les return doivent être compatibles, Any si on n'est pas dans une déclaration de fonction*)
  let varsglob,varsloc,funs,structs,fields=env in (*vars loc est l'environnement des variables locales supérieures*)
  match e.desc with 
  | Eint n -> {ttype= Tint64; tdesc= TEint n}
  | Ebool b -> {ttype= Tbool; tdesc= TEbool b}
  | Estring s -> {ttype= Tstring; tdesc= TEstring s}

  | Ecall ("print",l) -> let lp=List.map (type_expr t_return env) l in (*on vérifie que tous les arguments de print sont bien typés*)
                        {ttype=Tnothing ; tdesc=Tprint lp}

  | Ebinop(Ar(op),e1,e2) -> let t1=type_expr t_return env e1 in 
                          assert_compatible Tint64 t1.ttype e1.loc;

                          let t2=type_expr t_return env e2 in 
                          assert_compatible Tint64 t2.ttype e2.loc;

                          {ttype=Tint64; tdesc= TEbinop(Ar(op),t1,t2)}

  | Ebinop(Comp(op),e1,e2) ->let t1=type_expr t_return env e1 in 
                            assert_compatible Tint64 t1.ttype e1.loc;

                            let t2=type_expr t_return env e2 in 
                            assert_compatible Tint64 t2.ttype e2.loc;

                            {ttype=Tbool; tdesc= TEbinop(Comp(op),t1,t2)}

  | Ebinop(Bop(op),e1,e2)-> let t1=type_expr t_return env e1 in 
                          assert_compatible Tbool t1.ttype e1.loc;

                          let t2=type_expr t_return env e2 in 
                          assert_compatible Tbool t2.ttype e2.loc;

                          {ttype=Tbool; tdesc= TEbinop(Bop(op),t1,t2)}     

  | Ebinop (Eq(op),e1,e2) -> let t1=type_expr t_return env e1 in 
                            
                            let t2=type_expr t_return env e2 in (*aucune contrainte sur la relation entre deux types pour 
                              une comparaison*)
                            
                          {ttype=Tbool; tdesc= TEbinop(Eq(op),t1,t2)}

  | Eminus e1 -> let t1=type_expr t_return env e1 in 
              assert_compatible Tint64 t1.ttype e1.loc;

              {ttype= Tint64; tdesc= TEminus t1}

  | Enot e1 -> let t1=type_expr t_return env e1 in 
              assert_compatible Tbool t1.ttype e1.loc;

              {ttype= Tbool; tdesc=TEnot t1}

  | Eaffect(e1,e2) -> begin match e1.desc with 
                      | Evar x -> (let te=type_expr t_return env e2 
                                  and tx=(try Smap.find x varsloc 
                                          with Not_found-> try Smap.find x varsglob
                                        with Not_found -> error(e1.loc,"Unbound value "^x))

                            in match tx.t with
                            |Tany -> tx.t <- te.ttype; (*si c'est un Any, on prend en compte l'affectation*)
                                    let xp={ttype=te.ttype; tdesc=TEvar x}

                                    in {ttype=te.ttype; tdesc=TEaffect(xp,te)}

                            |_ -> assert_compatible tx.t te.ttype e2.loc ;
                            let xp={ttype=tx.t; tdesc=TEvar x} in 
                            {ttype=te.ttype; tdesc=TEaffect(xp,te)} )

                      | Earg (e3,x) -> (let te2=type_expr t_return env e2 
                                      and te3=type_expr t_return env e3 in

                                    let s,tx=(try Smap.find x fields 
                                              with Not_found -> error (e1.loc,"No such field "^x))
                                      (*on cherche quel est la structure qui a pour champ x et le type associé, on échoue si elle n'existe pas*)
                        
                                    in assert_compatible (Tstruct s) te3.ttype e3.loc;
                                    (*on vérifie que e3 est bien de type compatible avec cette structure*)
                                    assert_compatible tx te2.ttype e2.loc;
                                    (*et que e2 l'est avec s.x*)

                                    let ts=Smap.find s structs in 
                                    if not ts.tsmut then error (e.loc,s^" object is not mutable");

                                    let te1={ttype=tx; tdesc= TEarg(te3,x)} in 
                                    {ttype=te3.ttype;tdesc= TEaffect(te1,te2)})
                      |_ -> failwith "impossible matching case given the parser" end 

  | Evar x -> let tx=(try Smap.find x varsloc 
                      with Not_found-> try Smap.find x varsglob
                      with Not_found -> error(e.loc,"Unbound value "^x))
                      
                    in {ttype=tx.t; tdesc= TEvar x} (*pas redondant avec les cas précédents, car au-dessus on doit 
                                faire l'affectation et la modification de l'environnement*)

  | Earg(e1,x) -> let s,tx=(try Smap.find x fields 
                  with Not_found -> error (e1.loc,"No such field "^x))

                in let te1= type_expr t_return env e1 in 

                assert_compatible (Tstruct s) te1.ttype e1.loc;

                {ttype= tx; tdesc= TEarg(te1,x)}

  | Eblock b -> let t,b1=type_block t_return env b in 
                {ttype=t; tdesc=TEblock(b1,Smap.empty)}
                
  | IfElse (e1,b1,b2) -> let t1=type_expr t_return env e1 in 
                    assert_compatible Tbool t1.ttype e1.loc;

                    let tb1,b1prime=type_block t_return env b1 and tb2,b2prime=type_block t_return env b2 in 

                    let t=(if tb1=tb2 then tb1 else Tany) in 

                    {ttype=t; tdesc=TIfElse(t1,
                      {ttype=tb1;tdesc=TEblock (b1prime, Smap.empty)},
                      {ttype=tb2;tdesc=TEblock (b2prime, Smap.empty)})}


  | Ereturn None -> assert_compatible t_return Tnothing e.loc;
                    {ttype=Tany; tdesc=TEreturn None}

  | Ereturn (Some e1) -> let t1=type_expr t_return env e1 in 
                          assert_compatible t_return t1.ttype e1.loc;

                        {ttype=Tany; tdesc=TEreturn (Some t1)}

  | Ewhile (e1,b) -> let t1=type_expr t_return env e1 in 
                    assert_compatible Tbool t1.ttype e1.loc ;

                  let envloc=loc_env varsloc b e.loc in 
                  let t,l=type_block t_return (varsglob,Smap.union fc varsloc envloc,funs,structs,fields) b in 

                  let bp={ttype=t; tdesc=TEblock (l,envloc)} (*envloc sont les variables locales spécifiques à la boucle*)
              in {ttype=Tnothing; tdesc=TEwhile (t1,bp)}

  | Efor (x,e1,e2,b) -> let t1=type_expr t_return env e1 in 
                        assert_compatible Tint64 t1.ttype e1.loc;
                        let t2=type_expr t_return env e2 in 
                        assert_compatible Tint64 t1.ttype e1.loc;
                        
                        let partial_envloc=loc_env varsloc b e.loc in 

                        let envloc=Smap.add x {t=Tint64} partial_envloc in 

                        let t,l=type_block t_return (varsglob,Smap.union fc varsloc envloc,funs,structs,fields) b in 

                        let bp={ttype=t; tdesc= TEblock (l,envloc)} in 

                        {ttype=Tnothing; tdesc= TEfor(x,t1,t2,bp)}
  | Ecall(name,pl)-> try let s=Smap.find name structs in 
                      
                          let rec expl_param arg par=(*renvoie la liste des types des arguments ar en vérifiant 
                                  qu'ils sont compatibles avec les types attendus par et qu'il y en a le bon nombre*)
                            match arg,par with 
                            |[],[] ->[]
                            |[],_ -> error (e.loc,name^" expects more arguments")
                            |_,[] ->error (e.loc,"too many arguments given to "^name)
                            |a::q1,(pn,pt)::q2 -> let ta=type_expr t_return env a in 
                                                  assert_compatible pt ta.ttype a.loc;
                                                  ta::(expl_param q1 q2)

                    in let l=expl_param pl s.tsfields in 
                    {ttype=Tstruct name; tdesc=TEcalls (name,l)}

                  with Not_found -> (
                        try let fs = Smap.find name funs in

                        let l = List.map (type_expr t_return env) pl in
                        (* texpr list *)

                        let fcompatible aux f = (* teste si le type de f est compatible
                                                   avec celui de la fonction appelée *)
                            if (try List.fold_left2
                                    (fun b x (_,y) -> b && (compatible x.ttype y))
                                    true l f.tfarg
                                with Invalid_argument _ -> false
                            ) then f::aux else aux
                        in

                        let pot_f = List.fold_left fcompatible [] fs in

                        match pot_f with
                            | [] -> error (e.loc,"No function matches the call type")
                            | [g] -> {ttype=g.tfres; tdesc=TEcalls(name,l)}
                            | _ -> error(e.loc,"Ambiguous call to "^name)

                        with Not_found -> error (e.loc,"Unbound function "^name)
                  )

  | _ -> {ttype=Tnothing;tdesc=TEbool true } (* provoquera une erreur de compatibilité
  pour nous indiquer que quelque chose n'est pas encore implémenté *)

and type_block t_return env b =(*renvoie le couple type du bloc, listes des expressions dans le nouvel ast*)
      match b with
        | [] -> Tnothing,[]
        | [e] -> let t=type_expr t_return env e in t.ttype,[t]
        | e::q ->
                let e1=type_expr t_return env e and t,q1=type_block t_return env q in 
                t,e1::q1


and check_returns t b = () (* vérifie que les return d'un bloc sont bien de type t *)

and check_func (vars,funs,structs,fields_of_structs) f = 
    
    let rec add_params vars = function
        | [] -> vars
        | (pname,ptype)::q -> Smap.add pname {t=ptype} (add_params vars q)
    in

    let env_par=add_params Smap.empty f.tfarg in (* on commence par ajouter les
                                                    paramètres à l'environnement *)
    let envf = loc_env env_par f.tfinstr f.tfloc in (* variables locales de f *)

    let t,bp = type_block f.tfres (vars,Smap.union fc envf env_par,funs,structs,fields_of_structs) f.tfinstr in

    let bl={ttype=t; tdesc=TEblock (bp,envf)} in 

    let tres = f.tfres in
    assert_compatible tres bl.ttype f.tfloc;

    (* à remplacer par Type_error (loc,given,expected) une fois qu'on aura un
     * string_of_type *)

    {tfargr= f.tfarg; tfresr= f.tfres; tfinstrr= bl}

(*and check_expr env e = 
  let t=type_expr env e in 
  if not (compatible t Tany) then failwith "et voilà Ocaml, argument utilisé, t'es content ?"
  else ()*)

let second_search ast env = (*On type le corps des fonctions et des expressions et on
                               vérifie qu'ils conviennent avec l'environnement global *)
    let vars,funs,structs,fields=env in 

    let check_decl env d = match d with
        | Func f -> Tf(f.fname,f.fpar)
        | Expr e -> Te(type_expr Tany (vars,Smap.empty,funs,structs,fields) e)
        | Struct s -> Ts s

   in let l=List.map (check_decl env) ast
  in l,Smap.map (fun l -> List.map (check_func env) l) funs,structs

let typing f=
  let env=first_search f in 
  
  second_search f env
  (*Printf.printf "done\n";;*)
