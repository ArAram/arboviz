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

open Parsing;;
let _ = parse_error;;
# 2 "src/frontends/dot/DotParser.mly"
open Tree
open UtilFrontend
open Hashtbl

let get_lbl = function
  | Some s -> s
  | None -> ""
      
let tbl = Hashtbl.create 100000

let add_arrow_tbl k v =
  if Hashtbl.mem tbl k then
    let lbl, sons = Hashtbl.find tbl k in
    Hashtbl.replace tbl k (lbl, v :: sons)
  else
    Hashtbl.add tbl k ("", [v])

let add_node_tbl k lbl =
  if Hashtbl.mem tbl k then
    let lbl, sons = Hashtbl.find tbl k in
    Hashtbl.replace tbl k (lbl, sons)
  else
    Hashtbl.add tbl k (lbl, [])
  
module StringSet = Set.Make(
                       struct
                         type t = string
                         let compare = compare
                       end)

let npop n l =
  let rec aux n l acc =
    if n = 0 then
      (l, acc)
    else
      aux (n-1) (List.tl l) ((List.hd l) :: acc)
  in
  aux n l []
                           
let make_tree nodes sons =
  let roots_ids = StringSet.elements (StringSet.diff nodes sons) in
  if (List.length roots_ids) != 1 then
    failwith "This is not a tree !"
  else
    begin
      let root_id = List.hd roots_ids in
      let root = Hashtbl.find tbl root_id in
      let lbl, sons = root in
      let tree_list = ref [(lbl, List.length sons)] in
      let sons_ids = ref sons in
      while !sons_ids != [] do
        let son_id = List.hd !sons_ids in
        let son = Hashtbl.find tbl son_id in
        let lbl, sons_son = son in
        tree_list := (lbl, List.length sons_son) :: !tree_list;
        sons_ids := sons_son @ (List.tl !sons_ids)
      done;

      let to_build = ref [] in
      while !tree_list != [] do
        let (lbl, arity) = List.hd !tree_list in
        let new_builds, sons = npop arity !to_build in
        to_build := (make_node lbl sons) :: new_builds;
        tree_list := List.tl !tree_list
      done;

      List.hd !to_build
    end
      
      
      
                           
# 92 "src/frontends/dot/DotParser.ml"
let yytransl_const = [|
  259 (* DIGRAPH *);
  260 (* LABEL *);
  261 (* EQUAL *);
  262 (* COMMA *);
  263 (* SEMICOLON *);
  264 (* RBRACKET *);
  265 (* LBRACKET *);
  266 (* RBRACE *);
  267 (* LBRACE *);
  268 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\004\000\
\004\000\004\000\004\000\005\000\005\000\003\000\006\000\006\000\
\006\000\006\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\006\000\005\000\003\000\001\000\001\000\002\000\002\000\002\000\
\003\000\001\000\002\000\004\000\005\000\003\000\001\000\002\000\
\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
\000\000\000\000\000\000\006\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\003\000\002\000\001\000\000\000\
\000\000\014\000\000\000\000\000\016\000\012\000\000\000\021\000\
\020\000\019\000\017\000\018\000\013\000"

let yydgoto = "\002\000\
\004\000\010\000\017\000\011\000\012\000\025\000\026\000"

let yysindex = "\008\000\
\012\255\000\000\006\255\000\000\255\254\022\255\022\255\027\255\
\028\255\021\255\022\255\022\255\025\255\000\000\007\255\015\255\
\031\255\022\255\040\000\000\000\000\000\041\000\037\255\038\255\
\036\255\026\255\017\255\000\000\000\000\000\000\000\000\001\255\
\043\255\000\000\007\255\007\255\000\000\000\000\039\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\255\
\000\000\000\000\040\255\041\255\000\000\000\000\000\000\000\000\
\019\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\247\255\000\000\000\000\234\255\000\000"

let yytablesize = 52
let yytable = "\018\000\
\013\000\040\000\041\000\037\000\020\000\021\000\005\000\023\000\
\001\000\007\000\024\000\029\000\043\000\044\000\003\000\027\000\
\006\000\039\000\010\000\011\000\010\000\011\000\008\000\038\000\
\009\000\015\000\023\000\010\000\011\000\024\000\019\000\035\000\
\036\000\014\000\022\000\015\000\015\000\028\000\016\000\030\000\
\031\000\032\000\033\000\034\000\042\000\045\000\000\000\000\000\
\000\000\004\000\005\000\015\000"

let yycheck = "\009\000\
\007\000\001\001\002\001\026\000\011\000\012\000\001\001\001\001\
\001\000\011\001\004\001\018\000\035\000\036\000\003\001\001\001\
\011\001\027\000\001\001\001\001\003\001\003\001\001\001\007\001\
\003\001\009\001\001\001\010\001\010\001\004\001\010\001\006\001\
\007\001\007\001\010\001\009\001\009\001\007\001\012\001\000\000\
\000\000\005\001\005\001\008\001\002\001\007\001\255\255\255\255\
\255\255\010\001\010\001\008\001"

let yynames_const = "\
  DIGRAPH\000\
  LABEL\000\
  EQUAL\000\
  COMMA\000\
  SEMICOLON\000\
  RBRACKET\000\
  LBRACKET\000\
  RBRACE\000\
  LBRACE\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'graph) in
    Obj.repr(
# 87 "src/frontends/dot/DotParser.mly"
                                           (
                let nodes, sons = _4 in
                make_tree nodes sons
              )
# 201 "src/frontends/dot/DotParser.ml"
               : Tree.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'graph) in
    Obj.repr(
# 91 "src/frontends/dot/DotParser.mly"
                                     (
                let nodes, sons = _3 in
                make_tree nodes sons
              )
# 211 "src/frontends/dot/DotParser.ml"
               : Tree.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'attrs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph) in
    Obj.repr(
# 97 "src/frontends/dot/DotParser.mly"
                          ( _3 )
# 219 "src/frontends/dot/DotParser.ml"
               : 'graph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node_decl) in
    Obj.repr(
# 98 "src/frontends/dot/DotParser.mly"
                ( let n = _1 in
                  ((StringSet.add n StringSet.empty), StringSet.empty)
                )
# 228 "src/frontends/dot/DotParser.ml"
               : 'graph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_decl) in
    Obj.repr(
# 101 "src/frontends/dot/DotParser.mly"
                 ( let p,s = _1 in
                   ((StringSet.add s (StringSet.add p StringSet.empty)),
                    (StringSet.add s StringSet.empty))
                 )
# 238 "src/frontends/dot/DotParser.ml"
               : 'graph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'graph) in
    Obj.repr(
# 105 "src/frontends/dot/DotParser.mly"
                      ( let n = _1 in
                        let nodes, sons = _2 in
                        ((StringSet.add n nodes), sons)
                       )
# 249 "src/frontends/dot/DotParser.ml"
               : 'graph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arrow_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'graph) in
    Obj.repr(
# 109 "src/frontends/dot/DotParser.mly"
                       ( let p,s = _1 in
                         let nodes, sons = _2 in
                         ((StringSet.add s (StringSet.add p nodes)),
                          (StringSet.add s sons))
                       )
# 261 "src/frontends/dot/DotParser.ml"
               : 'graph))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 116 "src/frontends/dot/DotParser.mly"
                      ( add_node_tbl _1 ""; _1 )
# 268 "src/frontends/dot/DotParser.ml"
               : 'node_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'attrs) in
    Obj.repr(
# 117 "src/frontends/dot/DotParser.mly"
                            ( add_node_tbl _1 _2; _1 )
# 276 "src/frontends/dot/DotParser.ml"
               : 'node_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "src/frontends/dot/DotParser.mly"
            ( add_node_tbl _1 ""; _1 )
# 283 "src/frontends/dot/DotParser.ml"
               : 'node_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attrs) in
    Obj.repr(
# 119 "src/frontends/dot/DotParser.mly"
                  ( add_node_tbl _1 _2; _1 )
# 291 "src/frontends/dot/DotParser.ml"
               : 'node_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "src/frontends/dot/DotParser.mly"
                                  ( add_arrow_tbl _1 _3; (_1, _3) )
# 299 "src/frontends/dot/DotParser.ml"
               : 'arrow_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'attrs) in
    Obj.repr(
# 124 "src/frontends/dot/DotParser.mly"
                                        ( add_arrow_tbl _1 _3; (_1, _3) )
# 308 "src/frontends/dot/DotParser.ml"
               : 'arrow_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'attr_list) in
    Obj.repr(
# 127 "src/frontends/dot/DotParser.mly"
                                  (
                 try
                   List.find (fun x -> if x = "" then false else true) (List.map get_lbl _2)
                 with
                   Not_found -> ""
               )
# 320 "src/frontends/dot/DotParser.ml"
               : 'attrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'attr) in
    Obj.repr(
# 135 "src/frontends/dot/DotParser.mly"
           ( [_1] )
# 327 "src/frontends/dot/DotParser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'attr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 136 "src/frontends/dot/DotParser.mly"
                     ( _1 :: _2 )
# 335 "src/frontends/dot/DotParser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'attr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 137 "src/frontends/dot/DotParser.mly"
                           ( _1 :: _3)
# 343 "src/frontends/dot/DotParser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'attr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 138 "src/frontends/dot/DotParser.mly"
                               ( _1 :: _3 )
# 351 "src/frontends/dot/DotParser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 141 "src/frontends/dot/DotParser.mly"
                      ( Some _3 )
# 358 "src/frontends/dot/DotParser.ml"
               : 'attr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "src/frontends/dot/DotParser.mly"
                      ( None )
# 366 "src/frontends/dot/DotParser.ml"
               : 'attr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 143 "src/frontends/dot/DotParser.mly"
                        ( None )
# 374 "src/frontends/dot/DotParser.ml"
               : 'attr))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.t)
