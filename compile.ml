open Ast
open X86_64
open Format
open Typer

let compile_binop op =
    (*code assembleur de l'opération binaire e1 op e2 où la valeur de 1 est dans rax et celle de e2 dans rbx*)
    match op with 
    | Ar(Plus) -> addq !%rbx !%rax 
    | Ar(Minus) -> subq !%rbx !%rax
    | Ar(Times) -> imulq !%rbx !%rax
    | Ar(Div) -> cqto ++ idivq !%rbx
    | Ar(Mod) -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
    | Ar(Exp) -> call "fast_exp"
    | _ -> failwith "not yet implemented"


let rec compile_expr e =
    match e.tdesc with 
    | Tprint l -> let lc= List.map (fun e -> (compile_expr e) ++ (call "print") ++ (addq (imm 16) !%rsp)) l in 
                    List.fold_left (++) nop lc
    | TEint i -> pushq (imm64 i) ++
                pushq (imm 1)
    | TEbool b -> let i=(if b then 1 else 0) in 
                pushq (imm i) ++
                pushq (imm 2)

    (*| TEstring s -> *)

    | TEbinop(op,e1,e2) -> compile_expr e2 ++
                        compile_expr e1 ++ (*à ce stade, e1 puis e2 sont en sommet de pile*)
                        addq (imm 8) !%rsp ++ (*on ignore le type de e1, à préciser à l'avenir*)
                        popq rax ++ (*valeur de e1 dans rax*)
                        addq (imm 8) !%rsp ++
                        popq rbx ++
                        compile_binop op ++ (*à ce stade, la valeur de e1 op e2 est dans rax*)
                        pushq !%rax ++
                        (match op with 
                        | Ar(_) -> pushq (imm 1) (*la valeur est un entier*)
                        | _ -> pushq (imm 2)) (*la valeur est un booléen*)
                        
    
    | _ -> pushq (imm 0) ++ (*toutes les expressions non implémentées equivaudront à un double nothing
                            sur la pile pour le moment (pour que tout soit de taille 2)*)
        pushq (imm 0)


let compile_instr decl = 
    match decl with
    | Tf _ -> nop
    | Ts _ -> nop
    | Te e -> compile_expr e 

let fast_exp =
    (* fait l'exponentiation rapide de %rax^%rbx si %rbx est positif *)
    label "fast_exp" ++

    testq !%rbx !%rbx ++
    js "negative_exp" ++
        
    (* exponentiation rapide à coder ici*)

    ret

let negative_exp =
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

    (*cmpq (imm 3) !%rsi ++
    je "print_string"*)

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

(*let print_string =
    label "print_string" ++
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_string") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++

    jmp "end_print"*)

let print_data =  (*penser à virer les \n pour ne pas faire doublon avec println quand on aura les strings*)
    label ".Sprint_int" ++
    string "%d\n" ++
    (*label ".Sprint_string" ++
    string "%s" ++*)
    label ".btrue" ++
    string "true\n" ++
    label ".bfalse" ++
    string "false\n" ++
    label ".implementation_error" ++
    string "not yet implemented\n"  ++
    label ".negative_exp" ++
    string "Exponents should be nonnegative\n"

let compile (decls, funs, structsi,vars) ofile =
    let code = List.map compile_instr decls in
    let code = List.fold_right (++) code nop in
    let d = print_data in 
    let pgm =
        { text =
            globl "main" ++ label "main" ++
            code ++
            movq (imm 0) !%rax ++ (* exit *)
            ret ++
            fast_exp ++
            negative_exp ++
            print_f ++
            print_int ++
            print_bool ++
            ins ""; (* on saute une ligne pour aérer *)
          data =
              (*Smap.fold (fun x _ l -> label x ++ (dquad [0]) ++ l) vars*) d
        }
    in
    let f = open_out ofile in
    let fmt = formatter_of_out_channel f in
    X86_64.print_program fmt pgm;
    fprintf fmt "@?"; (* flush du buffer *)
    close_out f
