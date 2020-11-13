let ()=let lb=Lexing.from_channel stdin in
  let t=ref (Lexer.token lb) in
    while !t<>EOF do
      (match !t with
      |JINT i->Printf.printf "%i" i
      |IDENT s-> Printf.printf "%s" s
      |INT_IDENT (n,s)-> Printf.printf "%i" n;
                        Printf.printf "*";
                        Printf.printf "%s" s
      |PLUS ->Printf.printf "+"
      |SEMICOLON->Printf.printf ";"
      |_->failwith "pattern matching exhaustive");
  t:=Lexer.token lb done;
  Printf.printf "\n";;