open Ast
open X86_64
open Format
open Typer

module Smap=Typer.Smap 

module Tmap=Map.Make(struct type t=typ let compare=compare end)

let t_env = ref (Tmap.add Tnothing 0 (Tmap.add Tbool 1 (Tmap.add Tint64 2 (Tmap.add Tstring 3 (Tmap.add Tany 4 (Tmap.empty))))))

let id_of_type = function
    | Tnothing -> 0
    | Tbool -> 1
    | Tint64 -> 2
    | Tstring -> 3
    | Tany -> 4
    | Tstruct _ -> 5
let id_structs = ref (Smap.empty)

let id_of_struct s = 
    Smap.find s ( !id_structs) 

let f_implems = Hashtbl.create 2048

let gstructs = ref (Smap.empty)

let gfields = ref (Smap.add "_" ("_",Tnothing,0) Smap.empty)

let troiz (a,b,c) = c

let shift_level env =
    Smap.map (fun (o,l)-> (o,l+1)) env


let pushn n = subq (imm (8*n)) !%rsp 

let popn n = addq (imm (8*n)) !%rsp

let rec alloc_var n =
    match n with 
    | 0 -> nop
    | _ -> pushq (imm 0) ++ pushq (imm 1) ++ (alloc_var (n-1))

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

let compile_arith_op op =
    (*code assembleur de l'opération binaire e1 op e2 où la valeur de 1 est dans rax et celle de e2 dans rbx
    met le résultat dans rax*)
    match op with 
    | Plus -> addq !%rbx !%rax 
    | Minus -> subq !%rbx !%rax
    | Times -> imulq !%rbx !%rax
    | Div -> cqto ++ idivq !%rbx
    | Mod -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
    | Exp -> call "fast_exp"

let compile_eq_op op =
    match op with 
    | Equal -> call "comp_equal"
    | Different -> call "comp_different"


let compile_comp_op op=
    match op with 
    | Infeq -> call "comp_infeq"
    | Inf -> call "comp_inf"
    | Supeq -> call "comp_supeq"
    | Sup -> call "comp_sup"
    

let get_vars =
    (*level est dans rdx, rbp de ce level est dans rcx, laisse le rbp final dans rcx *)
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
    | [] -> pushq (imm 0) ++ pushq (imm 0)
    | [a] -> a 
    | a::q -> a ++ addq (imm 16) !%rsp ++ (concat_depile q)

let rec build_f_env args env i = match args with
    | [] -> env
    | (x,_)::q -> build_f_env q (Smap.add x (16+16*i,0) env) (i+1)

let rec compile_expr loc_env funs ret_depth e=
    (*string_set est un couple de références la table de hachage des strings déjà
     * existants et du prochain identifiant à attribuer*)
    match e.tdesc with 
    | Tprint l -> let lc= 
                    List.map (fun e -> (compile_expr loc_env funs ret_depth e) ++
                    (call "print") ++ (addq (imm 16) !%rsp)) l in 
                    List.fold_left (++) nop lc ++
                    pushq (imm 0) ++
                    pushq (imm 0) (*l'expression print l vaut nothing*)
    | TEint i -> movq (imm64 i) !%rax ++ pushq !%rax ++
                let ti = id_of_type Tint64 in
                pushq (imm (2*ti))
    | TEbool b -> let i=(if b then 1 else 0) in 
                pushq (imm i) ++
                let tb = id_of_type Tbool in
                pushq (imm (2*tb))

    | TEstring s -> begin 
                    let i= (match Smap.mem s ( !string_map) with 
                            | true -> Smap.find s ( !string_map)
                            | _ -> string_map :=  Smap.add s ( !string_id) ( !string_map);
                                    incr string_id;
                                    ( !string_id) -1)
                    
                    in leaq (lab (".str_"^string_of_int(i))) rax ++
                    pushq !%rax ++
                    let ts = id_of_type Tstring in
                    pushq (imm (2*ts)) 
                    end 

    
    | TEbinop(Bop(And),e1,e2) ->let i= !instr_id in 
                                incr instr_id; (*évaluation paresseuse*)
                                let tb=id_of_type Tbool in
                                compile_expr loc_env funs ret_depth e1 ++
                                popq rax ++
                                cmpq (imm (2*tb)) !%rax ++ (* on vérifie à la fois qu'on a un booléen et qu'il est défini*)
                                jne "type_error" ++
                                popq rbx ++
                                cmpq (imm 0) !%rbx ++
                                je ("instr_false_"^string_of_int(i)) ++
                                compile_expr loc_env funs ret_depth e2 ++
                                popq rax ++
                                cmpq (imm (2*tb)) !%rax ++ 
                                jne "type_error" ++
                                popq rbx ++
                                cmpq (imm 0) !%rbx ++
                                je ("instr_false_"^string_of_int(i)) ++
                                movq (imm 1) !%rbx ++
                                jmp ("end_instr_"^string_of_int(i)) ++
                                label ("instr_false_"^string_of_int(i)) ++
                                movq (imm 0) !%rbx ++
                                label ("end_instr_"^string_of_int(i)) ++
                                pushq !%rbx ++
                                pushq (imm (2*tb))

    | TEbinop(Bop(Or),e1,e2) -> let i= !instr_id in 
                                incr instr_id;
                                let tb=id_of_type Tbool in
                                compile_expr loc_env funs ret_depth e1 ++
                                popq rax ++
                                cmpq (imm (2*tb)) !%rax ++ 
                                jne "type_error" ++
                                popq rbx ++
                                cmpq (imm 1) !%rbx ++
                                je ("instr_true_"^string_of_int(i)) ++
                                compile_expr loc_env funs ret_depth e2 ++
                                popq rax ++
                                cmpq (imm (2*tb)) !%rax ++ 
                                jne "type_error" ++
                                popq rbx ++
                                cmpq (imm 1) !%rbx ++
                                je ("instr_true_"^string_of_int(i)) ++
                                movq (imm 0) !%rbx ++
                                jmp ("end_instr_"^string_of_int(i)) ++
                                label ("instr_true_"^string_of_int(i)) ++
                                movq (imm 1) !%rbx ++
                                label ("end_instr_"^string_of_int(i)) ++
                                pushq !%rbx ++
                                pushq (imm (2*tb))
    
    | TEbinop(Ar(op),e1,e2) -> let ti = id_of_type Tint64 in 
        
                        compile_expr loc_env funs ret_depth e2 ++
                        compile_expr loc_env funs ret_depth e1 ++
                        (*à ce stade, e1 puis e2 sont en sommet de pile*)
                        popq rax ++
                        cmpq (imm (2*ti)) !%rax ++
                        jne "type_error" ++
                        (*on vérifie qu'on a bien un entier*)
                        popq rax ++ (*valeur de e1 dans rax*)
                        popq rbx ++
                        cmpq (imm (2*ti)) !%rbx ++
                        jne "type_error" ++
                        popq rbx ++
                        compile_arith_op op ++
                        (*à ce stade, la valeur de e1 op e2 est dans rax*)
                        pushq !%rax ++
                        pushq (imm (2*ti))
                        
    | TEbinop(Comp(op),e1,e2) -> compile_expr loc_env funs ret_depth e2 ++
                                compile_expr loc_env funs ret_depth e1 ++
                                popq rcx ++
                                popq rax ++
                                popq rdx ++
                                popq rbx ++
                                compile_comp_op op ++
                                pushq !%rax ++
                                let tb = id_of_type Tbool in pushq (imm (2*tb))

    | TEbinop(Eq(op),e1,e2) -> let tb = id_of_type Tbool in
                                compile_expr loc_env funs ret_depth e2 ++
                                compile_expr loc_env funs ret_depth e1 ++
                                addq (imm 8) !%rsp ++
                                popq rax ++
                                addq (imm 8) !%rsp ++
                                popq rbx ++
                                movq (imm (2*tb)) !%rcx ++ (*permet de passer le test de type de comparaison*)
                                movq (imm (2*tb)) !%rdx ++
                                compile_eq_op op  ++
                                pushq !%rax ++
                                 pushq (imm (2*tb))

    | TEnot(e1) -> compile_expr loc_env funs ret_depth e1 ++
                    let tb = id_of_type Tbool in 
                    popq rax ++
                    cmpq (imm (2*tb)) !%rax ++
                    jne "type_error" ++
                    popq rbx ++
                    movq (imm 1) !%rax ++
                    subq !%rbx !%rax ++
                    (*1-e1*)
                    pushq !%rax ++
                    pushq (imm (2*tb))

    | TEminus(e1) -> compile_expr loc_env funs ret_depth e1 ++
                    let ti = id_of_type Tint64 in
                    popq rax ++
                    cmpq (imm (2*ti)) !%rax ++
                    jne "type_error" ++
                    popq rax ++
                    negq !%rax ++
                    pushq !%rax ++
                    pushq (imm (2*ti))
    
    | TEaffect(e1, e2) ->
                            begin match e1.tdesc with
                          | TEvar(x) -> compile_expr loc_env funs ret_depth e2 ++
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
                                       end 

 
                          | TEarg(e3,x) -> 
                                        compile_expr loc_env funs ret_depth e2 ++
                                        compile_expr loc_env funs ret_depth e3 ++
                                        popq rcx ++ popq rdx ++
                                        popq rax ++ popq rbx ++
                                        cmpq (imm (2*(id_of_type (Tstruct("_"))))) !%rcx ++
                                        jne "type_error" ++

                                        let (s,t,num)=Smap.find x ( !gfields) in 
                                        let i = id_of_struct s in 
                                        cmpq (imm i) (ind rdx) ++
                                        jne "type_error" ++

                                        (if t=Tany then nop
                                        else let j=id_of_type t in (*si le champ attendu n'est pas de type Tany,
                                        on vérifie qu'on a bien le bon type*)
                                        cmpq (imm (2*j)) !%rax ++
                                        jne "type_error") ++ 

                                        movq !%rax (ind ~ofs:(2*(num-1)*8+8) rdx)  ++
                                        movq !%rbx (ind ~ofs:(2*(num-1)*8+16) rdx)

                          | _ -> failwith "impossible matching case" 
                          
                          end ++
                          pushq !%rbx ++ 
                          pushq !%rax  (*l'expression x=a vaut a*)
                          
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


    | TEarg(e1,x) -> compile_expr loc_env funs ret_depth e1 ++
                    popq rax ++ 
                    popq rbx ++
                    cmpq (imm (2*(id_of_type (Tstruct("_"))))) !%rax ++
                    jne "type_error" ++
                    movq !%rbx !%rcx ++
                    let (s,t,num)=Smap.find x ( !gfields) in 
                    let i = id_of_struct s in 
                    
                    cmpq (imm i) (ind rcx) ++
                    jne "type_error" ++ (*mauvaise structure*)

                    
                    movq (ind ~ofs:(2*(num-1)*8+8) rcx) !%rax ++
                    movq (ind ~ofs:(2*(num-1)*8+16) rcx) !%rbx ++
                    pushq !%rbx ++
                    pushq !%rax 

    | TEblock (b,_) -> let l=List.map (compile_expr loc_env funs ret_depth) b in 
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
                    
                    compile_expr loc_env funs ret_depth c ++ (*on compile la condition, si elle est fausse on saute en fin de boucle*)
                    popq rax ++
                    popq rbx ++

                    cmpq (imm (2*(id_of_type Tbool))) !%rax ++ (*on vérifie que la condition est un booléen*)
                    jne "type_error" ++

                    cmpq (imm 0) !%rbx ++
                    je ("end_instr_"^string_of_int(i)) ++
                    pushq !%rbp ++
                    movq !%rsp !%rbp ++ 

                    alloc_var (Smap.cardinal loc_spec) ++
                    (*on alloue la place nécessaire pour les variables locales*)
                    compile_expr loc funs (ret_depth+1) e1 ++
                     
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

            
                        compile_expr loc_env funs ret_depth e1 ++ (*e1 se trouve à la place de la valeur x*)
                        cmpq (imm (2*(id_of_type Tint64))) (ind rsp) ++
                        jne "type_error"++ (*on vérifie que les bornes sont entières*)

                        compile_expr loc_env funs ret_depth e2 ++
                        cmpq (imm (2*(id_of_type Tint64))) (ind rsp) ++
                        jne "type_error"++ 
                        (*on compile e2 une fois*)
                        label ("instr_"^string_of_int(i)) ++
                        pushq !%rbp ++
                        movq !%rsp !%rbp ++

                        alloc_var (Smap.cardinal loc_spec) ++ 
                        movq (ind ~ofs:16 rbp) !%rbx ++ (*valeur de e2*)
                        cmpq (ind ~ofs:32 rbp) !%rbx ++ (*valeur de x*)
                        (*si x>e2, la boucle se termine*)
                        js ("end_instr_"^string_of_int(i)) ++

                        
                        compile_expr loc funs (ret_depth+1) e3 ++
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
                        let tb = id_of_type Tbool in
                        incr instr_id;
                        incr instr_id;
                        compile_expr loc_env funs ret_depth s ++
                        popq rbx ++ popq rax ++
                        cmpq (imm (2*tb)) !%rbx ++
                        jne "type_error" ++
                        testq !%rax !%rax ++
                        je ("instr_"^string_of_int(i+1)) ++   
                        compile_expr loc_env funs (ret_depth) a ++
                        jmp ("end_instr_"^string_of_int(i)) ++
                        label ("instr_"^string_of_int(i+1)) ++
                        compile_expr loc_env funs (ret_depth) b ++
                        jmp ("end_instr_"^string_of_int(i)) ++
                        label ("end_instr_"^string_of_int(i))


    | TEcallf(f,lpot,e_args,icall) -> 
                              (* comme le dispatch n'a pas été encore implémenté, on utilise la première
                                 méthode potentielle *)

                              let j = List.hd lpot in

                              (* on commence par compiler les arguments *)
                              let args_compiled =
                              List.map (compile_expr loc_env funs ret_depth) e_args in
                              List.fold_right (++) (List.rev args_compiled) nop ++

                              (* puis on construit l'environnement local de la fonction
                               * et on compile chaque variante dans l'environnement
                               * actuel *)


                              begin
                                  if not (Hashtbl.mem f_implems icall) then(
                                    Hashtbl.add f_implems icall [];
                                    Hashtbl.replace f_implems icall
                                    (List.map (compile_f funs loc_env f icall) lpot);
                                  );
                              nop
                              end

                              (* enfin on explore l'arbre des possibilités selon le
                               * type des arguments compilés, et on appelle la bonne
                               * fonction selon les cas *)


                             ++ call ("function_"^string_of_int(icall)^"_"^(string_of_int(j))) ++

                              (* on désalloue les arguments *)
                              addq (imm (16*(List.length e_args))) !%rsp ++

                              
                              (* on teste que le type de retour est compatible avec celui annoncé *)


                              (
                                  let lf = Smap.find f funs in
                                  let f_j = List.nth lf j in
                                  let t_r = f_j.tfresr in
                                  if t_r <> Tany then (
                                      movq !%rbx !%rcx ++
                                      cmpq  (imm (2*(id_of_type t_r))) !%rcx ++
                                      jne "type_error"
                                  )
                                  else
                                      nop
                              )
                              ++

                              (* la fonction renvoie (val,type) dans (rax,rbx) qu'on 
                               * rempile donc pour que l'appel retourne ces valeurs *)
                              pushq !%rax ++ pushq !%rbx

    | TEreturn(Some e) -> compile_expr loc_env funs ret_depth e ++
                          popq rbx ++ popq rax ++
                          

                          movq (imm ret_depth) !%rdx ++
                          movq !%rbp !%rcx ++
                          call "get_var" ++
                          
                          movq !%rcx !%rsp ++
                          popq rbp ++
                          ret

    | TEreturn(None) ->   movq (imm ret_depth) !%rdx ++
                          movq !%rbp !%rcx ++
                          call "get_var" ++
                          movq (imm 0) !%rax ++
                          movq (imm 0) !%rbx ++
                          movq !%rcx !%rsp ++
                          popq rbp ++
                          ret

    | TEcalls(s,e) ->     pushq !%r14 ++ (*on sauvegarde r14 pour éviter qu'il soit écrasé par une autre déclaration
                            parmi les champs*)
                          movq (imm (8*(1+2*(List.length e)))) !%rdi ++
                          call "malloc" ++
                          movq !%rax !%r14 ++
                          movq (imm (id_of_struct s)) (ind r14) ++
                          let fields_compiled = List.map2
                          (fun ei (x,tx) ->
                              let num=troiz (Smap.find x ( !gfields)) in 
                                compile_expr loc_env funs ret_depth ei ++
                              popq rax ++ popq rbx ++
                              (if tx=Tany then nop
                              else let i=id_of_type tx in (*si le type attendu pour ce champ n'est pas any, on
                                    vérifie qu'on a bien le bon type*)
                              cmpq (imm (2*i)) !%rax ++
                              jne "type_error") ++
                              movq !%rax (ind ~ofs:(2*(num-1)*8+8) r14)  ++
                              movq !%rbx (ind ~ofs:(2*(num-1)*8+16) r14)  
                              
                          )
                          e (Smap.find s !gstructs).tsfields in
                          List.fold_left (++) nop fields_compiled ++
                          popq rax ++
                          pushq !%r14 ++
                          pushq (imm (2*(id_of_type (Tstruct(s)))) ) ++
                          movq !%rax !%r14



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

    label ("function_"^string_of_int(j)^"_"^string_of_int(i)) ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    alloc_var (Smap.cardinal loc_spec) ++
    compile_expr f_env funs 0 instrs ++
    popq rbx ++
    popq rax ++
    movq !%rbp !%rsp ++
    popq rbp ++
    ret 

let compile_instr funs decl = 
    begin match decl with
    | Tf _ -> nop
    | Ts _ -> nop
    | Te e -> compile_expr Smap.empty funs 0 e ++ 
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
    movq !%r15 !%rsp ++
    ret

let print_f =
    (*fonction qui affiche la valeur en sommet de pile et la dépile
    Teste le type de la valeur, et appelle la fonction d'affichage dédiée*)
    label "print" ++
    
    pushq !%rbp ++
    movq !%rsp !% rbp ++
       
    movq (ind ~ofs:16 rbp) !%rsi ++ (*premier composant : le tag*)
    movq (ind ~ofs:24 rbp) !%rdi ++ (*deuxième composant : le truc à afficher*)

    let ti = id_of_type Tint64 in 
    cmpq (imm (2*ti)) !%rsi ++
    je "print_int" ++

    let tb = id_of_type Tbool in
    cmpq (imm (2*tb)) !%rsi ++
    je "print_bool" ++

    let ts = id_of_type Tstring in
    cmpq (imm (2*ts)) !%rsi ++
    je "print_string" ++

    let tn = id_of_type Tnothing in
    cmpq (imm (2*tn)) !%rsi ++
    je "print_nothing" ++

    andq (imm 1) !%rsi ++
    testq !%rsi !%rsi ++
    jmp "type_error" ++

    leaq (lab ".implementation_error") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    movq (imm 1) !%rax ++
    movq !%r15 !%rsp ++
    ret ++


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

let print_nothing =
    label "print_nothing" ++
    leaq (lab ".s_nothing") rsi ++
    leaq (lab ".Sprint_string") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++

    jmp "end_print"

let print_data =  
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
    label ".bad_type" ++
    string "ERROR: Bad Type or Variable not Defined \n" ++
    label ".s_nothing" ++
    string "nothing\n"

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
    jmp "comp_type_1" ++

    label "comp_false" ++
    movq (imm 0)  !%rax ++

    label "comp_type_1" ++
    let ti = id_of_type Tint64 in 
    cmpq (imm (2*ti)) !%rcx ++
    je "comp_type_2" ++ (*on a un entier, on teste le deuxième type*)
    let tb = id_of_type Tbool in 
    cmpq (imm (2*tb)) !%rcx ++ (*pas d'entier, on teste si booléen*)
    jne "type_error" ++ (*ni entier ni booléen*)

    label "comp_type_2" ++
    cmpq (imm (2*ti)) !%rdx ++
    je "end_comp" ++
    cmpq (imm (2*tb)) !%rdx ++
    jne "type_error" ++

    label "end_comp" ++ (*on a le bon type, et le résultat du test est dans rax*)
    ret 

let errors =
    label "type_error" ++ 
    leaq (lab ".bad_type") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++ 
    movq !%r15 !%rsp ++
    movq (imm 1) !%rax ++
    ret 
    


let compile (decls, funs, structs, vars, fields) ofile =

    gstructs := structs;
    gfields := fields;
    let i = ref 0 in
    id_structs := Smap.map (fun t -> incr i; !i) structs; 
    let code = List.map (compile_instr funs) decls in
    let code = List.fold_right (++) code nop in
    let variables = Smap.fold (fun x _ acc -> label ("_v_"^x) ++ (dquad [0]) ++
                    label ("_t_"^x) ++ (dquad [1]) ++ acc) vars nop in
    let d = print_data in 
    let s = print_string_labels ( !string_map) in 
    
    let functions = Hashtbl.fold
                    (fun _ l_implems acc -> (List.fold_right (++) l_implems nop) ++ acc)
                    f_implems nop in

    let pgm =
        { text =
            globl "main" ++ label "main" ++
            movq !%rsp !%r15 ++
            movq (imm 0) (lab "_t_nothing") ++
            code ++
            movq (imm 0) !%rax ++ (* exit *)
            ret ++
            fast_exp ++
            print_f ++
            print_int ++
            print_bool ++
            print_string ++
            print_nothing ++
            comparisons ++
            get_vars ++
            errors ++
            functions ++
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
