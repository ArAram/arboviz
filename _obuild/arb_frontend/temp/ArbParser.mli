type token =
  | IDENT of (string)
  | COMMA
  | RBRACKET
  | LBRACKET
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.t
