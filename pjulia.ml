open Ast

let lb=Lexing.from_channel stdin
let ast= Parser.file Lexer.token lb

let binop_rep = [ Ar(Plus), "+";
                  Ar(Minus), "-";
                  Ar(Times), "*";
                  Ar(Exp), "^";
                  Bop(And), "&&";
                  Bop(Or), "||";
                  Comp(Equal), "==";
                  Comp(Different), "!=";
                  Comp(Infeq), "<=";
                  Comp(Inf), "<";
                  Comp(Supeq), ">=";
                  Comp(Sup), ">" ]

let rec pretty_printer e = 

    match e with
    | Eint i -> Printf.printf "%d" i
    | Estring s -> Printf.printf "%s" s
    | Ebool b -> Printf.printf "%b" b
    | Evar x -> Printf.printf "%s" x
    | Ebinop(op,e1,e2) -> Printf.printf "(";
                        pretty_printer e1;
                        Printf.printf ")";
                         Printf.printf " %s " (List.assoc op binop_rep);
                         Printf.printf "(";
                         pretty_printer e2 ;
                         Printf.printf ")"
    | Enot e -> Printf.printf "!("; pretty_printer e; Printf.printf ")"
    | Eminus e -> Printf.printf "- "; pretty_printer e
    
    
let () = List.iter (fun d -> pretty_printer d; print_newline ()) ast



