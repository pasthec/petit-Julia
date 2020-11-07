let lb=Lexin.from_channel stdin
let f=Parser.file Lexer.token lb

let ()=Printf.printf "%i" f
