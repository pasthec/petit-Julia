open Ast
open X86_64
open Format
open Typer

let compile_instr decl = nop

let print_int = nop

let compile (decls, funs, structsi,vars) ofile =
    let code = List.map compile_instr decls in
    let code = List.fold_right (++) code nop in
    let pgm =
        { text =
            globl "main" ++ label "main" ++
            code ++
            print_int ++
            movq (imm 0) !%rax ++ ret ++ (ins ""); (* on saute une ligne pour aérer *)
          data =
              Smap.fold (fun x _ l -> label x ++ (dquad [0]) ++ l) vars nop
        }
    in
    let f = open_out ofile in
    let fmt = formatter_of_out_channel f in
    X86_64.print_program fmt pgm;
    fprintf fmt "@?"; (* flush du buffer *)
    close_out f
