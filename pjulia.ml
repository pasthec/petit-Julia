open Ast

open Format
open Lexing
open Parser

let usage = "usage: petit-julia [options] file.jl"

let parse_only = ref false

let type_only = ref false 

let pretty_print = ref false

let ofile = ref ""

let set_file f s = f := s

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, " stop after typing";
    "--pretty-print", Arg.Set pretty_print, " print the ast produced after parsing";
    "-o", Arg.String (set_file ofile), "<file> to specify the output file"]

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

    if !pretty_print then Printer.print_file ast;


    if !parse_only then exit 0;

    let ast = Typer.typing ast in 
    if !type_only then exit 0;

    (*puis production de code*)

    if !ofile="" then (
        if file="stdin" then ofile:="out.s"
        else ofile := Filename.chop_suffix file ".jl" ^ ".s"
    );

    Compile.compile ast !ofile

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
	eprintf "this expression has type %s but was expected to have type %s@." given expected;
  exit 1
    |Typer.Error (loc,s)->(*autre erreur détectée par le typeur*)
  report loc;
  eprintf "Error: %s@." s;
  exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e); (*erreur inattendue du compilateur*)
	exit 2
