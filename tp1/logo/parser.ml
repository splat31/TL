type token =
  | EOF
  | FORWARD
  | BACKWARD
  | PRINT
  | RIGHT
  | LEFT
  | HOME
  | LPAR
  | RPAR
  | PLUS
  | MINUS
  | TIMES
  | MOD
  | DIV
  | POW
  | MAKE
  | INT of (int)
  | NAME of (string)
  | REF of (string)
  | PRINTS of (string)

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

	open Ast
	open Common
	open Printf

	module StringSet = Set.Make(String)

	(** Map of variable name over variable index. *)
	let var_map = ref StringMap.empty

	(** Get the index of variable.
		@param name	Name of variable.
		@return		Variable index, -1 if the variable does not exists. *)
	let get_var name =
		try
			StringMap.find name !var_map
		with Not_found ->
			-1

	(** Create a new variable.
		@param name		Name of variable.
		@return			Variable index. *)
	let make_var name =
		let index = Comp.alloc_var () in
		var_map := StringMap.add name index !var_map;
		index

	(** Local map. *)
	let local_set = ref StringSet.empty

	(** Record a variable as local.
		@param name		Variable name. *)
	let set_local name =
		local_set := StringSet.add name !local_set

	(** Test if a variable is local.
		@param name		Variable name.
		@return			True if the variable is local, false else. *)
	let is_local name =
		StringSet.mem name !local_set

	(** Map of the function. Provides the label of the function.*)
	let fun_map = ref StringMap.empty

	(** Get the label of a function.
		@param name		Function name. *)
	let get_fun name =
		try
			StringMap.find name !fun_map
		with Not_found ->
			-1

	(** Create a function.
		@param name		Name of function.
		@param ast		AST of the function.
		@return			Label of function. *)
	let make_fun name ast =
		let lab = Comp.new_label () in
		fun_map := StringMap.add name lab !fun_map;
		Comp.add_fun name lab ast

	(** Raise an error with the provided message.
		@param msg	Message to display. *)
	let error msg =
		raise (SyntaxError msg)

	(** Declare the passed parameters and assign them good offsets.
		@param params	Parameters to declare. *)
	let declare_params params =
		ignore (List.fold_left
			(fun index name ->
				var_map := StringMap.add name index !var_map;
				set_local name;
				index - 1
			)
			(-3)
			(List.rev params))

	(** Release the parameters.
		@param params	Parameters to declare. *)
	let release_params params =
		List.iter
			(fun name -> var_map := StringMap.remove name !var_map)
			params

# 112 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* FORWARD *);
  258 (* BACKWARD *);
  259 (* PRINT *);
  260 (* RIGHT *);
  261 (* LEFT *);
  262 (* HOME *);
  263 (* LPAR *);
  264 (* RPAR *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* MOD *);
  269 (* DIV *);
  270 (* POW *);
  271 (* MAKE *);
    0|]

let yytransl_block = [|
  272 (* INT *);
  273 (* NAME *);
  274 (* REF *);
  275 (* PRINTS *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\005\000\005\000\005\000\
\006\000\006\000\007\000\007\000\007\000\007\000\008\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\001\000\000\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\001\000\003\000\003\000\003\000\001\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\011\000\026\000\001\000\000\000\004\000\000\000\023\000\
\024\000\000\000\014\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\015\000\016\000\019\000\020\000\
\021\000\017\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\018\000\019\000\020\000\021\000"

let yysindex = "\255\255\
\006\255\000\000\255\254\255\254\255\254\255\254\255\254\000\000\
\253\254\000\000\000\000\000\000\006\255\000\000\255\254\000\000\
\000\000\028\255\000\000\016\255\000\000\028\255\028\255\028\255\
\028\255\255\254\000\000\023\255\255\254\255\254\255\254\255\254\
\255\254\255\254\028\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\020\000\000\000\001\000\000\000\026\000\045\000\051\000\
\070\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\076\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\027\000\254\255\245\255\000\000\003\000"

let yytablesize = 351
let yytable = "\001\000\
\018\000\022\000\023\000\024\000\025\000\015\000\003\000\004\000\
\005\000\006\000\007\000\008\000\028\000\026\000\016\000\002\000\
\017\000\037\000\038\000\006\000\009\000\003\000\042\000\035\000\
\010\000\007\000\031\000\032\000\033\000\034\000\036\000\029\000\
\030\000\039\000\040\000\041\000\029\000\030\000\000\000\027\000\
\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\018\000\018\000\018\000\018\000\018\000\000\000\
\018\000\018\000\018\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\018\000\006\000\006\000\006\000\006\000\
\006\000\006\000\007\000\007\000\007\000\007\000\007\000\007\000\
\000\000\000\000\006\000\000\000\000\000\000\000\006\000\000\000\
\007\000\000\000\000\000\000\000\007\000\010\000\010\000\010\000\
\010\000\010\000\010\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000\000\000\010\000\000\000\000\000\000\000\010\000\
\000\000\008\000\000\000\000\000\000\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000\000\000\009\000\000\000\000\000\000\000\
\009\000\000\000\013\000\000\000\000\000\000\000\013\000"

let yycheck = "\001\000\
\000\000\004\000\005\000\006\000\007\000\007\001\001\001\002\001\
\003\001\004\001\005\001\006\001\015\000\017\001\016\001\000\000\
\018\001\029\000\030\000\000\000\015\001\000\000\034\000\026\000\
\019\001\000\000\011\001\012\001\013\001\014\001\008\001\009\001\
\010\001\031\000\032\000\033\000\009\001\010\001\255\255\013\000\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\015\001\
\255\255\255\255\255\255\019\001\001\001\002\001\003\001\004\001\
\005\001\006\001\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\255\255\015\001\255\255\255\255\255\255\019\001\255\255\
\015\001\255\255\255\255\255\255\019\001\001\001\002\001\003\001\
\004\001\005\001\006\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\015\001\255\255\255\255\255\255\019\001\
\255\255\015\001\255\255\255\255\255\255\019\001\001\001\002\001\
\003\001\004\001\005\001\006\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\255\255\015\001\255\255\255\255\255\255\
\019\001\255\255\015\001\255\255\255\255\255\255\019\001"

let yynames_const = "\
  EOF\000\
  FORWARD\000\
  BACKWARD\000\
  PRINT\000\
  RIGHT\000\
  LEFT\000\
  HOME\000\
  LPAR\000\
  RPAR\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  MOD\000\
  DIV\000\
  POW\000\
  MAKE\000\
  "

let yynames_block = "\
  INT\000\
  NAME\000\
  REF\000\
  PRINTS\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'opt_cmd_seq) in
    Obj.repr(
# 136 "parser.mly"
  ( _1 )
# 307 "parser.ml"
               : Ast.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
  ( NOP )
# 313 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd_seq) in
    Obj.repr(
# 143 "parser.mly"
  ( _1 )
# 320 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 148 "parser.mly"
  ( _1 )
# 327 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 150 "parser.mly"
  ( SEQ(_1, _2) )
# 335 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
  ( SYSCALL (Logo.cFORWARD, [_2]) )
# 342 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
  ( NOP )
# 349 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
  ( SYSCALL (Logo.cRIGHT, [_2]) )
# 356 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
  ( NOP )
# 363 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
  ( PRINT _2 )
# 370 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
  ( PRINTS _1 )
# 377 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "parser.mly"
  ( NOP )
# 383 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "parser.mly"
  ( NOP )
# 391 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 176 "parser.mly"
        ( _1 )
# 398 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 178 "parser.mly"
        ( NONE )
# 406 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 180 "parser.mly"
  ( NONE )
# 414 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 185 "parser.mly"
  ( NONE )
# 422 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 187 "parser.mly"
  ( NONE )
# 429 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 192 "parser.mly"
  ( NONE )
# 437 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 194 "parser.mly"
  ( NONE )
# 445 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 196 "parser.mly"
  ( NONE )
# 453 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 198 "parser.mly"
  ( NONE)
# 460 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 203 "parser.mly"
        ( CST _1 )
# 467 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 205 "parser.mly"
        ( NONE )
# 474 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 207 "parser.mly"
        ( NONE )
# 481 "parser.ml"
               : 'expr4))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.command)
