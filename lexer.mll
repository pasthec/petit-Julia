{
  open Lexing
  open Parser
  
let keyword_before=["true",TRUE;"false",FALSE;"return",RETURN;"end",END] (*mots clés devant ; auto*)

let keyword_not_before=["else",ELSE;"elseif",ELSEIF;"for",FOR;"function",FUNCTION;"if",IF;
  "mutable",MUTABLE;"struct",STRUCT;"while",WHILE] (*mots clés pas devant ; auto*)
let h1=Hashtbl.create 32 ;;
let h2=Hashtbl.create 32 ;;
List.iter (fun (s,id)-> Hashtbl.add h1 s id) keyword_before;;
List.iter (fun (s,id)-> Hashtbl.add h2 s id) keyword_not_before;;

let id_or_kwd s= 
  (* renvoie un couple constitué du token et d'un booléen indiquant si on précède ou non un point-virgule*)
    try Hashtbl.find h1 s, true 
    with Not_found ->(try Hashtbl.find h2 s, false
                         with Not_found -> IDENT s,true  )
  
  

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
    | "\n" { new_line lexbuf;
    	     if !before_auto_semicolon then begin
	     	   before_auto_semicolon:=false;
	           SEMICOLON
	                    end 
             else token lexbuf }
    | eof { EOF }
    | jint as s {before_auto_semicolon:=true; JINT (int_of_string s)}
    | ident as s {let t,b=id_or_kwd s in
                    before_auto_semicolon:=b; t}
    | (jint as s) (ident as u) {before_auto_semicolon:=true; 
                            INT_IDENT (int_of_string s,u)}
    | jint as s '(' {INT_LPAR (int_of_string s)}
    | ')' (ident as s) {before_auto_semicolon:=true;
                        RPAR_IDENT s}
    | "(" {LPAR}
    | ")" {before_auto_semicolon:=true; RPAR} 
    | "+" {PLUS}
    | "*" {TIMES}
    | "-" {MINUS}
    | ";" {SEMICOLON}
    | "&&" {AND}
    | "||" {OR}
    | "!" {NOT}

    |_ {failwith "unauthorized character or token not yet implemented"}

and comment=parse 
    | "\n" {new_line lexbuf;if !before_auto_semicolon then 
                              begin before_auto_semicolon:=false;SEMICOLON end 
                              else token lexbuf }
                          
    | eof {EOF}
    | _ {comment lexbuf} 
{
}
