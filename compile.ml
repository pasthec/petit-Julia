open Ast
open X86_64
open Format
open Typer


module Smap=Typer.Smap (*Map.Make(struct type t=string let compare=compare end) *)

module Tmap=Map.Make(struct type t=typ let compare=compare end)

let t_env = ref (Tmap.add Tnothing 0 (Tmap.add Tbool 1 (Tmap.add Tint64 2 (Tmap.add Tstring 3 (Tmap.add Tany 4 (Tmap.empty))))))

(*
let j_call = ref 0 (* référence du nombre d'appels de fonction en tout, le j-ème
                      appel compilant des function_j_i pour 0<=i<n où n est le nombre
                      de fonctions candidates à l'appel selon le type des arguments *)
*)
(* finalement c'est aussi simple d'utiliser instr_id *)

let functions = ref nop

let shift_level env =
    Smap.map (fun (o,l)-> (o,l+1)) env


let pushn n = subq (imm (8*n)) !%rsp 

let popn n = addq (imm (8*n)) !%rsp

let rec alloc_var n =
    match n with 
    | 0 -> nop
    | _ -> pushq (imm 0) ++ pushq (imm 0) ++ (alloc_var (n-1))

let string_map = ref Smap.empty and string_id = ref 0 
let instr_id = ref 0 

let alloc loc_var o=
    (*dictionnaire des variables locales, renvoie un ofs par rapport à rbp pour chacune des variables*)
    let ofs= ref o in 
    Smap.map (fun t-> ofs:= !ofs -16; (!ofs,0)) loc_var

let union_alloc sup_vars loc_vars ofs=
    (*décale les ofsets de sup_vars de ofs, et ajoute les variables locales spécifiques*)

    let vars_loc= alloc loc_vars ofs in 
    Smap.union Typer.fc sup_vars vars_loc 

let compile_binop op =
    (*code assembleur de l'opération binaire e1 op e2 où la valeur de 1 est dans rax et celle de e2 dans rbx
    met le résultat dans rax*)
    match op with 
    | Ar(Plus) -> addq !%rbx !%rax 
    | Ar(Minus) -> subq !%rbx !%rax
    | Ar(Times) -> imulq !%rbx !%rax
    | Ar(Div) -> cqto ++ idivq !%rbx
    | Ar(Mod) -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
    | Ar(Exp) -> call "fast_exp"
    | Bop(And) -> andq !%rbx !%rax 
    | Bop(Or) -> orq !%rbx !%rax 
    | Eq(Equal) -> call "comp_equal"
    | Eq(Different) -> call "comp_different"
    | Comp(Infeq) -> call "comp_infeq"
    | Comp(Inf) -> call "comp_inf"
    | Comp(Supeq) -> call "comp_supeq"
    | Comp(Sup) -> call "comp_sup"

let get_vars =
    (*level est dans rdx, rbp de ce level est dans rcx, laisse le rbp final dans rdx *)
    label "get_var" ++
    cmpq (imm 0) !%rdx ++
    je "return_var" ++
    movq (ind rcx) !%rcx ++
    subq (imm 1) !%rdx ++
    jmp "get_var" ++
    label "return_var" ++
    ret 

let rec concat_depile l= 
    (*concatène les codes de la liste l en dépilant les valeurs des expressions intermédiaires*)
    match l with 
    | [] -> nop 
    | [a] -> a 
    | a::q -> a ++ addq (imm 16) !%rsp ++ (concat_depile q)

let rec build_f_env args env i = match args with
    | [] -> env
    | (x,_)::q -> build_f_env q (Smap.add x (16+16*i,0) env) (i+1)

let rec compile_expr loc_env funs e =
    (*string_set est un couple de références la table de hachage des strings déjà
     * existants et du prochain identifiant à attribuer*)
    match e.tdesc with 
    | Tprint l -> let lc= 
                    List.map (fun e -> (compile_expr loc_env funs e) ++
                    (call "print") ++ (addq (imm 16) !%rsp)) l in 
                    List.fold_left (++) nop lc ++
                    pushq (imm 0) ++
                    pushq (imm 0) (*l'expression print l vaut nothing*)
    | TEint i -> movq (imm64 i) !%rax ++ pushq !%rax ++
                let ti = Tmap.find Tint64 !t_env in
                pushq (imm (2*ti))
    | TEbool b -> let i=(if b then 1 else 0) in 
                pushq (imm i) ++
                let tb = Tmap.find Tbool !t_env in
                pushq (imm (2*tb))

    | TEstring s -> begin 
                    let i= (match Smap.mem s ( !string_map) with 
                            | true -> Smap.find s ( !string_map)
                            | _ -> string_map :=  Smap.add s ( !string_id) ( !string_map);
                                    incr string_id;
                                    ( !string_id) -1)
                    
                    in leaq (lab (".str_"^string_of_int(i))) rax ++
                    pushq !%rax ++
                    let ts = Tmap.find Tstring !t_env  in
                    pushq (imm (2*ts)) 
                    end 

    | TEbinop(op,e1,e2) -> compile_expr loc_env funs e2 ++
                        compile_expr loc_env funs e1 ++
                        (*à ce stade, e1 puis e2 sont en sommet de pile*)
                        addq (imm 8) !%rsp ++
                        (*on ignore le type de e1, à préciser à l'avenir*)
                        popq rax ++ (*valeur de e1 dans rax*)
                        addq (imm 8) !%rsp ++
                        popq rbx ++
                        compile_binop op ++
                        (*à ce stade, la valeur de e1 op e2 est dans rax*)
                        pushq !%rax ++
                        (match op with 
                        | Ar(_) -> let ti = Tmap.find Tint64 !t_env in 
                                   pushq (imm (2*ti))
                                   (*la valeur est un entier*)
                        | _ -> let tb = Tmap.find Tbool !t_env in pushq (imm (2*tb)))
                                    (*la valeur est un booléen*)
    
    | TEnot(e1) -> compile_expr loc_env funs e1 ++
                    addq (imm 8) !%rsp ++
                    popq rbx ++
                    movq (imm 2) !%rax ++
                    subq !%rbx !%rax ++
                    (*1-e1, le not(1) faisait -2 (bit à bit en complément à 2 ?)*)
                    pushq !%rax ++
                    let tb = Tmap.find Tbool !t_env in
                    pushq (imm (2*tb))

    | TEminus(e1) -> compile_expr loc_env funs e1 ++
                    addq (imm 8) !%rsp ++
                    popq rax ++
                    negq !%rax ++
                    pushq !%rax ++
                    let ti = Tmap.find Tint64 !t_env in
                    pushq (imm (2*ti))
    
    | TEaffect(e1, e2) -> begin match e1.tdesc with
                          | TEvar(x) -> compile_expr loc_env funs e2 ++
                                        popq rax ++ popq rbx ++
                                        begin try 
                                            let (o,l) = Smap.find x loc_env in 
                                            movq (imm l) !%rdx ++
                                            movq !%rbp !%rcx ++
                                            call "get_var" ++
                                            movq !%rax (ind ~ofs:o rcx) ++
                                            movq !%rbx (ind ~ofs:(o+8) rcx)
                                        with Not_found -> 
                                            movq !%rax (ind ("_t_"^x)) ++
                                            movq !%rbx (ind ("_v_"^x))
                                       end ++
                                       pushq !%rbx ++ 
                                       pushq !%rax (*l'expression x=a vaut a*)
                          
                          | _ ->       pushq (imm 0) ++ pushq (imm 0)
                          end
                          
    | TEvar(x) -> begin try 
                    let (o,l) = Smap.find x loc_env in 
                    movq (imm l) !%rdx ++
                    movq !%rbp !%rcx ++
                    call "get_var" ++
                    movq (ind ~ofs:(o+8) rcx) !%rax ++
                    pushq !%rax ++
                    movq (ind ~ofs:o rcx) !%rax ++
                    pushq !%rax 
                with Not_found -> 

                    movq (ind ("_v_"^x)) !%rax ++ pushq !%rax ++
                    movq (ind ("_t_"^x)) !%rax ++ pushq !%rax
                end 

    | TEblock (b,_) -> let l=List.map (compile_expr loc_env funs) b in 
                      concat_depile l 

    | TEwhile(c,e1) -> let i = !instr_id in 
                    incr instr_id ;
                    let loc_spec=(
                         match e1.tdesc with 
                        | TEblock(_,m) -> m
                        | _ -> failwith "impossible matching case" )
                    in
                    
                    let sup = shift_level loc_env in

                    let loc =union_alloc sup loc_spec 0
                    in 
                    

                  

                    label ("instr_"^string_of_int(i)) ++
                    
                    compile_expr loc_env funs c ++ (*on compile la condition, si elle est fausse on saute en fin de boucle*)
                    popq rax ++
                    popq rbx ++
                    cmpq (imm 0) !%rbx ++
                    je ("end_instr_"^string_of_int(i)) ++
                    pushq !%rbp ++
                    movq !%rsp !%rbp ++ 

                    alloc_var (Smap.cardinal loc_spec) ++
                    (*on alloue la place nécessaire pour les variables locales*)
                    compile_expr loc funs e1 ++
                     
                    movq !%rbp !%rsp ++
                    popq rbp ++ (*on désalloue toutes les variables locales de la boucle*)
                    jmp ("instr_"^string_of_int(i))++
                    label ("end_instr_"^string_of_int(i)) ++

                    pushq (imm 0) ++ pushq (imm 0) 

    | TEfor(x,e1,e2,e3) -> let i = !instr_id in 
                        incr instr_id ; 
                        let loc_spec=(
                        match e3.tdesc with 
                        | TEblock(_,m) -> m
                        | _ -> failwith "impossible matching case" )
                        in 
                        
                        let sup = shift_level loc_env in

                        let loc =Smap.add x (24,0) (union_alloc sup loc_spec 0) in


                        (*on alloue la place nécessaire pour les variables locales en initialisant à nothing*)

            
                        compile_expr loc_env funs e1 ++ (*e1 se trouve à la place de la valeur x*)

                        compile_expr loc_env funs e2 ++
                        (*on compile e2 une fois*)
                        label ("instr_"^string_of_int(i)) ++
                        pushq !%rbp ++
                        movq !%rsp !%rbp ++

                        alloc_var (Smap.cardinal loc_spec) ++ 
                        movq (ind ~ofs:16 rbp) !%rbx ++ (*valeur de e2*)
                        cmpq (ind ~ofs:32 rbp) !%rbx ++ (*valeur de x*)
                        (*si x>e2, la boucle se termine*)
                        js ("end_instr_"^string_of_int(i)) ++

                        
                        compile_expr loc funs e3 ++
                        addq (imm 16) !%rsp ++
                        movq (ind ~ofs:32 rbp) !%rax ++
                        addq (imm 1) !%rax ++
                        movq !%rax (ind ~ofs:32 rbp) ++ (*incrémentation de x*)

                        movq !%rbp !%rsp ++
                        popq rbp ++ (*on désalloue toutes les variables locales de la boucle*)
                        jmp ("instr_"^string_of_int(i)) ++
                        label ("end_instr_"^string_of_int(i)) ++
                        movq !%rbp !%rsp ++ 
                        popq rbp ++
                        addq (imm 32) !%rsp ++ (*on désalloue la place pour x et e2*)
                        pushq (imm 0) ++ pushq (imm 0)

    | TIfElse(s,a,b) -> let i= !instr_id in
                        incr instr_id;
                        compile_expr loc_env funs s ++
                        popq rbx ++ popq rax ++
                        cmpq (imm 2) !%rbx ++
                        jne "expected_bool" ++
                        testq !%rax !%rax ++
                        je ("instr_"^string_of_int(i+1)) ++   
                        compile_expr loc_env funs a ++
                        jmp ("end_instr_"^string_of_int(i)) ++
                        label ("instr_"^string_of_int(i+1)) ++
                        compile_expr loc_env funs b ++
                        jmp ("end_instr_"^string_of_int(i)) ++
                        label ("end_instr_"^string_of_int(i))


    | TEcallf(f,lpot,e_args) -> let j = !instr_id in incr instr_id;
                              (* on commence par compiler les arguments *)
                              let args_compiled =
                              List.map (compile_expr loc_env funs) e_args in
                              List.fold_left (++) nop args_compiled ++

                              (* puis on construit l'environnement local de la fonction
                               * et on compile chaque variante dans l'environnement
                               * actuel *)


                             (List.iter (compile_f funs loc_env f j) lpot; nop)

                              (* enfin on explore l'arbre des possibilités selon le
                               * type des arguments compilés, et on appelle la bonne
                               * fonction selon les cas *)

                              ++ call ("function_"^string_of_int(j)^"_0") ++
                              
                              (* on désaloue les arguments *)
                              addq (imm (16*(List.length e_args))) !%rsp ++

                              (* la fonction renvoie (val,type) dans (rax,rbx) qu'on 
                               * rempile donc pour que l'appel retourne ces valeurs *)
                              pushq !%rax ++ pushq !%rbx

    | _ -> pushq (imm 0) ++ (*toutes les expressions non implémentées equivaudront à un double nothing
                            sur la pile pour le moment (pour que tout soit de taille 2)*)
        pushq (imm 0)


and compile_f funs env f j i =

    let lf = Smap.find f funs in
    let f_i = List.nth lf i in
    let instrs = f_i.tfinstrr in

    let loc_spec=(
    match instrs.tdesc with 
      | TEblock(_,m) -> m
      | _ -> failwith "impossible matching case" )
    in 

    let sup = shift_level env in
    let loc = union_alloc sup loc_spec 0 in

    let args = f_i.tfargr in
    let f_env = build_f_env args loc 0 in

    functions := label ("function_"^string_of_int(j)^"_"^string_of_int(i)) ++
                 pushq !%rbp ++
                 movq !%rsp !%rbp ++
                 alloc_var (Smap.cardinal loc_spec) ++
                 compile_expr f_env funs instrs ++
                 popq rbx ++
                 popq rax ++
                 movq !%rbp !%rsp ++
                 popq rbp ++
                 ret ++
                 !functions

let compile_instr funs decl = 
    begin match decl with
    | Tf _ -> nop
    | Ts _ -> nop
    | Te e -> compile_expr Smap.empty funs e ++ 
            addq (imm 16) !%rsp (*on dépile la valeur de l'expression*)
    end

let fast_exp =
    (* fait l'exponentiation rapide de %rax^%rbx si %rbx est positif *)
    label "fast_exp" ++

    testq !%rbx !%rbx ++
    js "negative_exp" ++
        
    movq !%rax !%rdi ++
    call "pos_exp" ++
    ret ++


    label "pos_exp" ++

    movq (imm 1) !%rax ++
    testq !%rbx !%rbx ++ (* si b==0, on renvoie 1 *)
    jne "exp" ++
    ret ++

    label "exp" ++
    pushq !%rbx ++
    shrq (imm 1) !%rbx ++ (* on appelle récursivement exp(a,b/2) *)
    call "pos_exp" ++
    imulq !%rax !%rax ++
    popq rbx ++
    andq (imm 1) !%rbx ++ (* si b impair on multiplie par a *)
    jne "imp_exp" ++
    ret ++

    label "imp_exp" ++
    imulq !%rdi !%rax ++
    ret ++
    

    label "negative_exp" ++
    leaq (lab ".negative_exp") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    movq (imm 1) !%rax ++
    ret

let print_f =
    (*fonction qui affiche la valeur en sommet de pile et la dépile
    Teste le type de la valeur, et appelle la fonction d'affichage dédiée*)
    label "print" ++
    
    (*ici on devra tester la valeur dans rdi, pour l'instant osef j'affiche des entiers*)
    pushq !%rbp ++
    movq !%rsp !% rbp ++
       
    movq (ind ~ofs:16 rbp) !%rsi ++ (*premier composant : le tag*)
    movq (ind ~ofs:24 rbp) !%rdi ++ (*deuxième composant : le truc à afficher*)

    let ti = Tmap.find Tint64 !t_env in 
    cmpq (imm (2*ti)) !%rsi ++
    je "print_int" ++

    let tb = Tmap.find Tbool !t_env in
    cmpq (imm (2*tb)) !%rsi ++
    je "print_bool" ++

    let ts = Tmap.find Tstring !t_env in
    cmpq (imm (2*ts)) !%rsi ++
    je "print_string" ++

    leaq (lab ".implementation_error") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    movq (imm 1) !%rax ++
    ret ++

    (*dans les faits, ici il faudra faire un print pour chaque structure
     * -> càd écrire "l'identifiant de la structure"(les valeurs des champs) *)

    label "end_print" ++
    movq !%rbp !%rsp ++
    popq rbp ++
    movq (imm 0) !%rax ++
    ret 

let print_int = 
    label "print_int" ++
    
   
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_int") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++

    jmp "end_print"

let print_bool =
    label "print_bool" ++
    cmpq (imm 0) !%rdi ++
    je ".Lfalse" ++
    leaq (lab ".btrue") rdi ++
    jmp ".Lprint" ++
    label ".Lfalse" ++
    leaq (lab ".bfalse") rdi ++
    label ".Lprint" ++
    movq (imm 0) !%rax ++
    call "printf" ++

    jmp "end_print"

let print_string =
    label "print_string" ++
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_string") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++

    jmp "end_print"

let print_data =  (*penser à virer les \n pour ne pas faire doublon avec println quand on aura les strings*)
    label ".Sprint_int" ++
    string "%lld" ++
    label ".Sprint_string" ++
    string "%s" ++
    label ".btrue" ++
    string "true" ++
    label ".bfalse" ++
    string "false" ++
    label ".implementation_error" ++
    string "not yet implemented\n"  ++
    label ".negative_exp" ++
    string "Exponents should be nonnegative\n" ++
    label ".expected_bool" ++
    string "ERROR: TypeError: non-boolean used in boolean context\n"

let print_string_label s i =
    label (".str_"^string_of_int(i)) ++
    string s

let print_string_labels string_set =
    let f s i c=
        print_string_label s i ++ c 
    in Smap.fold f string_set nop  

let comparisons = 
    label "comp_equal" ++ 
    cmpq !%rbx !%rax ++
    je "comp_true" ++
    jmp "comp_false" ++

    label "comp_different" ++ 
    cmpq !%rbx !%rax ++
    jne "comp_true" ++
    jmp "comp_false" ++

    label "comp_infeq" ++ 
    cmpq !%rbx !%rax ++
    jle "comp_true" ++
    jmp "comp_false" ++

    label "comp_inf" ++ 
    cmpq !%rbx !%rax ++
    jl "comp_true" ++
    jmp "comp_false" ++

    label "comp_supeq" ++ 
    cmpq !%rbx !%rax ++
    jge "comp_true" ++
    jmp "comp_false" ++

    label "comp_sup" ++ 
    cmpq !%rbx !%rax ++
    jg "comp_true" ++
    jmp "comp_false" ++

    label "comp_true" ++ 
    movq (imm 1) !%rax ++
    ret ++

    label "comp_false" ++
    movq (imm 0)  !%rax ++
    ret 

let errors =
    label "expected_bool" ++
    leaq (lab ".expected_bool") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    movq (imm 1) !%rax ++
    ret

    
(*
let rec create_f_env args = Smap.empty

let rec compile_f f l i = begin match l with
    | [] -> nop
    | f_i::q -> label ("function_"^f^(string_of_int(i))) ++
                compile_expr (create_f_env f_i.tfargr) f_i.tfinstrr ++
                ret ++
                compile_f f q (i+1)
    end
*)

let compile (decls, funs, structsi,vars) ofile =

 
    let code = List.map (compile_instr funs) decls in
    let code = List.fold_right (++) code nop in
    let variables = Smap.fold (fun x _ acc -> label ("_v_"^x) ++ (dquad [0]) ++
                    label ("_t_"^x) ++ (dquad [1]) ++ acc) vars nop in
    (*let functions = Smap.fold (fun f l acc -> compile_f f l 0 ++ acc ) funs nop in*)
    let d = print_data in 
    let s = print_string_labels ( !string_map) in 
    let pgm =
        { text =
            globl "main" ++ label "main" ++
            code ++
            movq (imm 0) !%rax ++ (* exit *)
            ret ++
            fast_exp ++
            print_f ++
            print_int ++
            print_bool ++
            print_string ++
            comparisons ++
            get_vars ++
            errors ++
            !functions ++
            ins ""; (* on saute une ligne pour aérer *)
          data =
              variables ++ d ++ s
        }
    in
    let f = open_out ofile in
    let fmt = formatter_of_out_channel f in
    X86_64.print_program fmt pgm;
    fprintf fmt "@?"; (* flush du buffer *)
    close_out f
