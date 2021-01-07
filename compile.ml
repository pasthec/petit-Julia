open Ast
open X86_64
open Format
open Typer

let rec compile_expr e =
    match e.tdesc with 
    | Tprint l -> let lc= List.map (fun e -> (compile_expr e) ++ (call "print") ++ (addq (imm 8) !%rsp)) l in 
                    List.fold_left (++) nop lc
    | TEint i -> movq (imm64 i) !%rax ++
                pushq !%rax (*++
                pushq (imm 1)*) (*pour dire que c'est un entier*)
    | _ -> nop 


let compile_instr decl = 
    match decl with
    | Tf _ -> nop
    | Ts _ -> nop
    | Te e -> compile_expr e 

let print_f =
    (*fonction qui affiche la valeur en sommet de pile et la dépile
    Teste le type de la valeur, et appelle la fonction d'affichage dédiée*)
    label "print" ++
    (*popq rdi ++*)
    (*ici on devra tester la valeur dans rdi, pour l'instant osef j'affiche des entiers*)
    pushq !%rbp ++
    movq !%rsp !% rbp ++
       
    movq (ind ~ofs:16 rbp) !%rdi ++

    call "print_int" ++
    movq !%rbp !%rsp ++
    popq rbp ++
    movq (imm 0) !%rax ++
    ret 

let print_int = 
    label "print_int" ++
    (*subq (imm 8) !%rsp ++ (* alignement jsp pourquoi, ça marche ni avec ni sans*)*)
   
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_int") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++

    (*addq (imm 8) !%rsp ++*)
    ret

let print_data = 
    label ".Sprint_int" ++
    string "%d\n" ++
    label ".Sprint_string" ++
    string "%s" ++
    label ".btrue" ++
    string "true" ++
    label ".bfalse" ++
    string "false" 

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
            print_f ++
            print_int ++
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
