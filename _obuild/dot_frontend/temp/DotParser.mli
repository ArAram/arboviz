type token =
  | IDENT of (string)
  | STR of (string)
  | DIGRAPH
  | LABEL
  | EQUAL
  | COMMA
  | SEMICOLON
  | RBRACKET
  | LBRACKET
  | RBRACE
  | LBRACE
  | ARROW
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.t
