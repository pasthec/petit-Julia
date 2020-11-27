open Ast

open Format
open Lexing
open Parser

let usage = "usage: petit-julia [options] file.jl"

let parse_only = ref false

let type_only = ref false 

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
    end 
and pprintl fmt = function
    | [] -> fprintf fmt ""
    | e::q -> fprintf fmt "%a\n%a" pprint e.desc pprintl q

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, " stop after typing"]

let file = (*chaîne de caractère avec le nom du fichier passé en argument, vérifie qu'on a bien un .jl*)
  (* (ou éventuellement pour le moment stdin) *)
  let f = ref "stdin" in
  let set_file s =
    match s with
    |"stdin"-> ()
    |_-> if not (Filename.check_suffix s ".jl") then
                raise (Arg.Bad "no .jl extension");
            f := s
  in
  Arg.parse spec set_file usage;
  !f;;

let report (b,e) = (*affiche la première ligne de l'erreur, avec la localisation*)
(*b et e sont de type Lexing.position et représentent le début et la fin de la portion qui produit une erreur*)
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = (if file = "stdin" then stdin else open_in file ) in
  let lb = Lexing.from_channel c in
  try
    let ast = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    List.iter (fun d -> printf "%a" pprint d.desc; print_newline ()) ast
    (*Printer.print_file f*)
    (*let f_typ=Typer.typing f in
    if !type_only then exit 0;
    puis production de code*)
  with
    | Lexer.Lexing_error s -> (*erreur lexicale*)
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error | Assert_failure _ -> (*erreur syntaxique*)
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | Typer.Type_error (loc,given,expected) ->(*mauvais type*)
  report loc;
	eprintf "this expression has type %s but was expected to have type %s" given expected;
  exit 1
    |Typer.Unbound_value (loc,s)->(*variable non déclarée, c'est le typeur qui s'en aperçoit*)
  report loc;
  eprintf "unbound value %s" s;
  exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e); (*erreur inattendue du compilateur, inch'allah ça sert à rien*)
	exit 2
