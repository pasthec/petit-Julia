let lb=Lexing.from_channel stdin in
  let t=ref (Lexer.token lb) in
    while !t<>EOF do
      (match !t with
      |JINT i->Printf.printf "%i" i
      |PLUS ->Printf.printf "+"
      |SEMICOLON->Printf.printf ";"
      |_->failwith "pattern matching exhaustive");
  t:=Lexer.token lb done;
  Printf.printf "\n";;
