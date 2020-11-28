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
    | Eint i -> fprintf fmt "%Ld" i
    | Estring s -> fprintf fmt "%s" s
    | Ebool b -> fprintf fmt "%b" b
    | Evar x -> fprintf fmt "%s" x
    | Ebinop(op,e1,e2) -> fprintf fmt "(@[%a %s@ %a@])" pprint e1.desc
                                  (List.assoc op binop_rep) pprint e2.desc
    | Enot e -> fprintf fmt "!%a" pprint e.desc
    | Eminus e -> fprintf fmt "-%a" pprint e.desc
    | IfElse(e,b1,b2) -> fprintf fmt "if %a then {\n%a}\n else {\n%a}" pprint e.desc
                                      pprintl ("\n",b1) pprintl ("\n",b2)
    | Efun f -> fprintf fmt "function %s(%a)\n@[%a@]end" f.fname pprintparam
                                                (", ",f.fpar) pprintl ("\n",f.finstr)
    | Estruct s -> fprintf fmt "struct %s\n@[%a]@end" s.sname pprintparam
                                                                      ("\n",s.spar)
    | Ecall (f,args) -> fprintf fmt "%s(%a)" f pprintl (",",args)
    | Earg(e,x) -> fprintf fmt "%a.%s" pprint e.desc x
    | Eaffect(e1,e2) -> fprintf fmt "%a=%a" pprint e1.desc pprint e2.desc
    | Ereturn e -> fprintf fmt "return ?"
    | Efor(i,e1,e2,b) -> fprintf fmt "for %s=%a:%a\n@[%a@]" i pprint e1.desc
                            pprint e2.desc pprintl ("\n",b)
    | Ewhile(e,b) -> fprintf fmt "while %a do\n@[%a@]" pprint e.desc pprintl ("\n",b)
    | Eblock b -> fprintf fmt "%a" pprintl ("\n",b)
    end 
and pprintl fmt (sep,l) = begin match l with
    | [] -> fprintf fmt ""
    | e::q -> fprintf fmt "%a%s%a" pprint e.desc sep pprintl (sep,q)
    end
and pprintparam fmt (sep,p) = begin match p with
    | [] -> fprintf fmt ""
    | x::q -> fprintf fmt "%s%s%a" x.pname sep pprintparam (sep,q)
    end    
let print_file ast = List.iter (fun d -> printf "%a" pprint d.desc; print_newline ()) ast
