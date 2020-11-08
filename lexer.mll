{
  open Lexing
  open Parser
  
  (*
  let keyword=["else",ELSE;"elseif",ELSEIF;"end",END;"false",FALSE;"for",FOR;"function",FUNCTION;"if",IF;
  "mutable",MUTABLE;"return",RETURN;"struct",STRUCT;"true",TRUE;"while",WHILE]
	let h=Hastbl.create 32 
	List.iter (fun (s,id)-> Hashtbl.add h s id) keyword
	let id_or_kwd s= try Hashtbl.find h s with Not_found -> IDENT s
  *)
(*let before_auto_semicolon = ident | jint | int_ident | rpar_ident | jstring
														| "true" | "false" | "return" | ')' | "end"*)
  let before_auto_semicolon =ref false 
}

let digit = ['0'-'9']
let alpha = [ 'a'-'z' 'A'-'Z' '_' ]
let ident = alpha ( alpha | digit )*
let jint = digit+
let jchar = [ ' '-'!' '#'-'[' ']'-'~' ] | "\\\\" | "\\\"" | "\\n" | "\\t"
let jstring =   '"' jchar* '"'
let int_ident = jint ident
let ident_lpar = ident '('
let int_lpar = jint '('
let rpar_ident = ')' ident


 
 
 
 
rule token=parse 
    | "#" { comment lexbuf }
    | " " | "\t" { token lexbuf }
    | "\n" { new_line lexbuf;if !before_auto_semicolon then 
                              begin before_auto_semicolon:=false;SEMICOLON end 
                              else token lexbuf }
    | eof { EOF }
    | jint as s {before_auto_semicolon:=true; JINT (int_of_string s)}
    | "+" {PLUS}

and comment=parse 
    | "\n" {new_line lexbuf;token lexbuf}
    | eof {EOF}
    | _ {token lexbuf} 
{
let lexer_test()=
  let lb=Lexing.from_channel stdin in
  let t=ref (token lb) in
    while !t<>EOF do
      (match !t with
      |JINT i->Printf.printf "%i" i
      |PLUS ->Printf.printf "+"
      |SEMICOLON->Printf.printf ";"
      |_->failwith "pattern matching exhaustive");
  t:=token lb done;
  Printf.printf "\n";;

(*lexer_test()*)


}
