open Ast
open Format

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
    | Ebinop(op,e1,e2) -> fprintf fmt "(@[%a %s@ %a@])" pprint e1.desc
                                  (List.assoc op binop_rep) pprint e2.desc
    | Enot e -> fprintf fmt "!%a" pprint e.desc
    | Eminus e -> fprintf fmt "-%a" pprint e.desc
    | IfElse(e,b1,b2) -> fprintf fmt "if %a {\n%a}\n else {\n%a}" pprint e.desc
                                      pprintl b1 pprintl b2
    |_ -> failwith "pattern matching not exhaustive"
    end 
and pprintl fmt = function
    | [] -> fprintf fmt ""
    | e::q -> fprintf fmt "%a\n%a" pprint e.desc pprintl q
    
    
let print_file ast = List.iter (fun d -> printf "%a" pprint d.desc; print_newline ()) ast