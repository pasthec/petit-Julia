open Ast
open Format

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

let rec pprint fmt e = 
    begin match e with
    | Eint i -> fprintf fmt "%d" i
    | Estring s -> fprintf fmt "%s" s
    | Ebool b -> fprintf fmt "%b" b
    | Evar x -> fprintf fmt "%s" x
    | Ebinop(op,e1,e2) -> fprintf fmt "(@[%a %s@ %a@])" pprint e1
                                  (List.assoc op binop_rep) pprint e2
    | Enot e -> fprintf fmt "!%a" pprint e
    | Eminus e -> fprintf fmt "-%a" pprint e
    | IfElse(e,b1,b2) -> fprintf fmt "if %a {\n%a}\n else {\n%a}" pprint e
                                      pprintl b1 pprintl b2
    end 
and pprintl fmt = function
    | [] -> fprintf fmt ""
    | e::q -> fprintf fmt "%a\n%a" pprint e pprintl q

let () = List.iter (fun d -> printf "%a" pprint d; print_newline ()) ast



