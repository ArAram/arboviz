# 1 "src/frontends/arb/ArbLexer.mll"
 
open ArbParser

# 6 "_obuild/arb_frontend/temp/ArbLexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\249\255\250\255\002\000\252\255\253\255\254\255\078\000\
    ";
  Lexing.lex_backtrk = 
   "\004\000\255\255\255\255\004\000\255\255\255\255\255\255\000\000\
    ";
  Lexing.lex_default = 
   "\255\255\000\000\000\000\255\255\000\000\000\000\000\000\255\255\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\002\000\003\000\000\000\002\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\006\000\000\000\005\000\000\000\000\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\000\000\000\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\000\000\007\000\000\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\003\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\255\255\255\255\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\255\255\255\255\255\255\255\255\007\000\255\255\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 13 "src/frontends/arb/ArbLexer.mll"
           s
# 125 "_obuild/arb_frontend/temp/ArbLexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 13 "src/frontends/arb/ArbLexer.mll"
             ( IDENT (s) )
# 129 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | 1 ->
# 14 "src/frontends/arb/ArbLexer.mll"
           ( LBRACKET )
# 134 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | 2 ->
# 15 "src/frontends/arb/ArbLexer.mll"
           ( RBRACKET )
# 139 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | 3 ->
# 16 "src/frontends/arb/ArbLexer.mll"
        ( COMMA )
# 144 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | 4 ->
# 17 "src/frontends/arb/ArbLexer.mll"
        ( token lexbuf )
# 149 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | 5 ->
# 18 "src/frontends/arb/ArbLexer.mll"
          ( token lexbuf )
# 154 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | 6 ->
# 19 "src/frontends/arb/ArbLexer.mll"
      ( EOF )
# 159 "_obuild/arb_frontend/temp/ArbLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

# 21 "src/frontends/arb/ArbLexer.mll"
 

# 169 "_obuild/arb_frontend/temp/ArbLexer.ml"
