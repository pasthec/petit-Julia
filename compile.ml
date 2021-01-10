open Ast
open X86_64
open Format
open Typer


module Smap=Typer.Smap (*Map.Make(struct type t=string let compare=compare end) *)

let shift_level env =
    Smap.map (fun (o,l)-> (o,l+1)) env


let alloc loc_var o=
    (*dictionnaire des variables locales, renvoie un ofs par rapport à rbp pour chacune des variables*)
    let ofs= ref o in 
    Smap.map (fun t-> ofs:= !ofs -16; (!ofs,0)) loc_var

let pushn n = subq (imm (8*n)) !%rsp 

let popn n = addq (imm (8*n)) !%rsp

let rec alloc_var n =
    match n with 
    | 0 -> nop
    | _ -> pushq (imm 0) ++ pushq (imm 0) ++ (alloc_var (n-1))



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

let rec compile_expr string_set loc_env instr_id e =
    (*string_set est un couple de références la table de hachage des strings déjà existants 
    et du prochain identifiant à attribuer*)
    match e.tdesc with 
    | Tprint l -> let lc= 
                    List.map (fun e -> (compile_expr string_set loc_env instr_id e) ++ (call "print") ++ (addq (imm 16) !%rsp)) l in 
                    List.fold_left (++) nop lc ++
                    pushq (imm 0) ++
                    pushq (imm 0) (*l'expression print l vaut nothing*)
    | TEint i -> movq (imm64 i) !%rax ++ pushq !%rax ++
                pushq (imm 1)
    | TEbool b -> let i=(if b then 1 else 0) in 
                pushq (imm i) ++
                pushq (imm 2)

    | TEstring s -> begin 
                    let set,next= string_set in 
                    let i= (match Smap.mem s ( !set) with 
                            | true -> Smap.find s (!set)
                            | _ -> set :=  Smap.add s ( !next) ( !set);
                                    incr next;
                                    ( !next) -1)
                    
                    in leaq (lab (".str_"^string_of_int(i))) rax ++
                    pushq !%rax ++
                    pushq (imm 3) 
                    end 

    | TEbinop(op,e1,e2) -> compile_expr string_set loc_env instr_id e2 ++
                        compile_expr string_set loc_env instr_id e1 ++ (*à ce stade, e1 puis e2 sont en sommet de pile*)
                        addq (imm 8) !%rsp ++ (*on ignore le type de e1, à préciser à l'avenir*)
                        popq rax ++ (*valeur de e1 dans rax*)
                        addq (imm 8) !%rsp ++
                        popq rbx ++
                        compile_binop op ++ (*à ce stade, la valeur de e1 op e2 est dans rax*)
                        pushq !%rax ++
                        (match op with 
                        | Ar(_) -> pushq (imm 1) (*la valeur est un entier*)
                        | _ -> pushq (imm 2)) (*la valeur est un booléen*)
    
    | TEnot(e1) -> compile_expr string_set loc_env instr_id e1 ++
                    addq (imm 8) !%rsp ++
                    popq rbx ++
                    movq (imm 1) !%rax ++
                    subq !%rbx !%rax ++ (*1-e1, le not(1) faisait -2 (bit à bit en complément à 2 ?)*)
                    pushq !%rax ++
                    pushq (imm 2)

    | TEminus(e1) -> compile_expr string_set loc_env instr_id e1 ++
                    addq (imm 8) !%rsp ++
                    popq rax ++
                    negq !%rax ++
                    pushq !%rax ++
                    pushq (imm 1)
    
    | TEaffect(e1, e2) -> begin match e1.tdesc with
                          | TEvar(x) -> compile_expr string_set loc_env instr_id e2 ++
                                        popq rax ++ popq rbx ++
                                        (try 
                                        let (o,l) = Smap.find x loc_env in 
                                        movq (imm l) !%rdx ++
                                        movq !%rbp !%rcx ++
                                        call "get_var" ++
                                        movq !%rax (ind ~ofs:o rcx) ++
                                        movq !%rbx (ind ~ofs:(o+8) rcx)
                                        with Not_found -> 
                                       movq !%rax (ind ("_t_"^x)) ++
                                       movq !%rbx (ind ("_v_"^x)) ) ++

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

    | TEblock (b,_) -> let l=List.map (compile_expr string_set loc_env instr_id) b in 
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
                    
                    pushq !%rbp ++
                    movq !%rsp !%rbp ++ 

                    alloc_var (Smap.cardinal loc_spec) ++ (*on alloue la place nécessaire pour les variables locales*)

                    label ("instr_"^string_of_int(i)) ++
                    compile_expr string_set sup instr_id c ++
                    popq rax ++
                    popq rbx ++
                    cmpq (imm 0) !%rbx ++
                    je ("end_instr_"^string_of_int(i)) ++
                    compile_expr string_set loc instr_id e1 ++
                    addq (imm 16) !%rsp ++
                    jmp ("instr_"^string_of_int(i))++
                    label ("end_instr_"^string_of_int(i)) ++
                    movq !%rbp !%rsp ++
                    popq rbp ++
                    pushq (imm 0) ++ pushq (imm 0) 

    | TEfor(x,e1,e2,e3) -> let i = !instr_id in 
                        incr instr_id ; 
                        let loc_spec=(
                        match e3.tdesc with 
                        | TEblock(_,m) -> union_alloc loc_env m (-16)
                        | _ -> failwith "impossible matching case" )
                        in 
                        
                        let sup = shift_level loc_env in

                        let loc =Smap.add x (-16,0) (union_alloc sup loc_spec (-16)) in 
                        
                        pushq !%rbp ++
                        movq !%rsp !%rbp ++

                        alloc_var (1+(Smap.cardinal loc_spec)) ++ 
                        (*on alloue la place nécessaire pour les variables locales en initialisant à nothing*)

            
                        compile_expr string_set sup instr_id e1 ++

                        popq rax ++ popq rbx ++

                        movq !%rbx (ind ~ofs:(-8) rbp) ++
                        movq !%rax (ind ~ofs:(-16) rbp) ++ (*x prend la valeur e1*)

                        compile_expr string_set sup instr_id e2 ++
                        (*on compile e2 une fois*)
                        label ("instr_"^string_of_int(i)) ++
                        popq rax ++ popq rbx ++
                        cmpq (ind ~ofs:(-8) rbp) !%rbx ++
                        (*si x>e2, la boucle se termine*)
                        js ("end_instr_"^string_of_int(i)) ++
                        pushq !%rbx ++ pushq !%rax ++
                        (*on repush la valeur de e2*)
                        compile_expr string_set loc instr_id e3 ++
                        addq (imm 16) !%rsp ++
                        movq (ind ~ofs:(-8) rbp) !%rax ++
                        addq (imm 1) !%rax ++
                        movq !%rax (ind ~ofs:(-8) rbp) ++
                        jmp ("instr_"^string_of_int(i)) ++
                        label ("end_instr_"^string_of_int(i)) ++
                        movq !%rbp !%rsp ++ 
                        popq rbp ++
                        pushq (imm 0) ++ pushq (imm 0)

    | TIfElse(s,a,b) -> let i= !instr_id in
                        compile_expr string_set loc_env instr_id s ++
                        popq rbx ++ popq rax ++
                        cmpq (imm 2) !%rbx ++
                        jne "expected_bool" ++
                        testq !%rax !%rax ++
                        je ("instr_"^string_of_int(i+1)) ++                       
                        compile_expr string_set loc_env (incr instr_id; instr_id) a ++
                        jmp ("end_instr_"^string_of_int(i)) ++
                        label ("instr_"^string_of_int(i+1)) ++
                        compile_expr string_set loc_env (incr instr_id; instr_id) b ++
                        jmp ("end_instr_"^string_of_int(i)) ++
                        label ("end_instr_"^string_of_int(i))


    | _ -> pushq (imm 0) ++ (*toutes les expressions non implémentées equivaudront à un double nothing
                            sur la pile pour le moment (pour que tout soit de taille 2)*)
        pushq (imm 0)


let compile_instr string_set instr_id decl = 
    match decl with
    | Tf _ -> nop
    | Ts _ -> nop
    | Te e -> compile_expr string_set Smap.empty instr_id e ++ 
            addq (imm 16) !%rsp (*on dépile la valeur de l'expression*)

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

    cmpq (imm 1) !%rsi ++
    je "print_int" ++

    cmpq (imm 2) !%rsi ++
    je "print_bool" ++

    cmpq (imm 3) !%rsi ++
    je "print_string" ++

    leaq (lab ".implementation_error") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++


    (*dans les faits, ici il faudra faire un print pour chaque structure*)

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

let compile (decls, funs, structsi,vars) ofile =
    let set = ref (Smap.empty) and next = ref 0 in 
    let string_set = set,next in 
    let instr_id = ref 0 in 
    let code = List.map (compile_instr string_set instr_id) decls in
    let code = List.fold_right (++) code nop in
    let variables = Smap.fold (fun x _ acc -> label ("_v_"^x) ++ (dquad [0]) ++
                    label ("_t_"^x) ++ (dquad [0]) ++ acc) vars nop in
    let d = print_data in 
    let s = print_string_labels ( !set) in 
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
