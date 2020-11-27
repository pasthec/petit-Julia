{
  open Lexing
  open Parser


exception Lexing_error of string
  
let keyword_before=["true",TRUE;"false",FALSE;"return",RETURN;"end",END]
(*mots clés devant ; auto *)

let keyword_not_before = [
        "else",ELSE;
        "elseif",ELSEIF;
        "for",FOR;
        "function",FUNCTION;
        "if",IF;
        "mutable",MUTABLE;
        "struct",STRUCT;
        "while",WHILE
] (*mots clés pas devant ; auto *)

let h1=Hashtbl.create 32 ;;
let h2=Hashtbl.create 32 ;;
List.iter (fun (s,id)-> Hashtbl.add h1 s id) keyword_before;;
List.iter (fun (s,id)-> Hashtbl.add h2 s id) keyword_not_before;;

let id_or_kwd s= 
(* renvoie un couple constitué du token et d'un booléen indiquant si on précède ou
 * non un point-virgule*)
    try Hashtbl.find h1 s, true 
    with Not_found ->(try Hashtbl.find h2 s, false
                         with Not_found -> IDENT s,true  )
  
(*let before_auto_semicolon = ident | jint | int_ident | rpar_ident | jstring
			      | "true" | "false" | "return" | ')' | "end"*)
  let before_auto_semicolon =ref false 


let error s=
  raise (Lexing_error s )

let assert_not_kwd s= (*sert pour s'assurer qu'un par-ident ou un int-indent ne se fait pas avec un keyword
  c'est bien une errreur de syntaxe, mais qu'on détecte dès l'analyse lexicale 
  et qu'il serait difficile de propager*)
  if (Hashtbl.mem h1 s) || (Hashtbl.mem h2 s) then
    raise Parser.Error
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
             else token lexbuf
           }
    | eof { EOF }
    | jint as s { before_auto_semicolon:=true;
                  JINT (int_of_string s) }
    | ident as s { let t,b=id_or_kwd s in
                    before_auto_semicolon:=b; t}
    | (jint as s) (ident as u) { before_auto_semicolon := true; 
                                 assert_not_kwd u;
                                  INT_IDENT (int_of_string s,u) }
    | jint as s '(' {INT_LPAR (int_of_string s)}
    | ')' (ident as s) { before_auto_semicolon:=true;
                         assert_not_kwd s;
                          RPAR_IDENT s }
    | (ident as s) '(' {before_auto_semicolon:=false;
                        assert_not_kwd s;
                        IDENT_LPAR s}

    | "\"" (jchar* as s) "\"" {before_auto_semicolon:=true;
                                    JSTRING s}

    | "(" {before_auto_semicolon:=false; LPAR}
    | ")" {before_auto_semicolon:=true; RPAR} 
    | "+" {before_auto_semicolon:=false; PLUS}
    | "*" {before_auto_semicolon:=false; TIMES}
    | "/" {before_auto_semicolon:=false; DIV}
    | "\\" {before_auto_semicolon:=false; IDIV}
    | "-" {before_auto_semicolon:=false; MINUS}
    | ";" {before_auto_semicolon:=false; SEMICOLON}
    | "&&" {before_auto_semicolon:=false; AND}
    | "||" {before_auto_semicolon:=false; OR}
    | "!" {before_auto_semicolon:=false; NOT}
    | "^" {before_auto_semicolon:=false; EXP}
    | "," {before_auto_semicolon:=false; COMMA}
    | ":" {before_auto_semicolon:=false; COLON}
    | "::" {before_auto_semicolon:=false; DOUBLE_COLON}
    | "." {before_auto_semicolon:=false; DOT}
    | "==" {before_auto_semicolon:=false; BEQUAL}
    | "!=" {before_auto_semicolon:=false; DIFFERENT}
    | "=" {before_auto_semicolon:=false; EQUAL}
    | "<=" {before_auto_semicolon:=false; INFEQ}
    | "<" {before_auto_semicolon:=false; INF}
    | ">=" {before_auto_semicolon:=false; SUPEQ}
    | ">" {before_auto_semicolon:=false; SUP}
    | "%" {before_auto_semicolon:=false; MOD}

    | _ as c {error ("unauthorized character"^(String.make 1 c))}

and comment=parse 
    | "\n" {new_line lexbuf;if !before_auto_semicolon then 
                              begin before_auto_semicolon:=false;SEMICOLON end 
                              else token lexbuf }
                          
    | eof {EOF}
    | _ {comment lexbuf} 
{
}
