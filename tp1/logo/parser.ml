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
  | LEFTC
  | RIGHTC
  | EQUAL
  | DIFF
  | LESS
  | LESSEQ
  | GREATER
  | GREATEREQ
  | PLUS
  | MINUS
  | TIMES
  | MOD
  | DIV
  | POW
  | MAKE
  | REPEAT
  | IF
  | IFELSE
  | INT of (
# 137 "parser.mly"
       int
# 33 "parser.ml"
)
  | NAME of (
# 138 "parser.mly"
       string
# 38 "parser.ml"
)
  | REF of (
# 139 "parser.mly"
        string
# 43 "parser.ml"
)
  | PRINTS of (
# 140 "parser.mly"
        string
# 48 "parser.ml"
)

open Parsing
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

# 139 "parser.ml"
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
  265 (* LEFTC *);
  266 (* RIGHTC *);
  267 (* EQUAL *);
  268 (* DIFF *);
  269 (* LESS *);
  270 (* LESSEQ *);
  271 (* GREATER *);
  272 (* GREATEREQ *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* TIMES *);
  276 (* MOD *);
  277 (* DIV *);
  278 (* POW *);
  279 (* MAKE *);
  280 (* REPEAT *);
  281 (* IF *);
  282 (* IFELSE *);
    0|]

let yytransl_block = [|
  283 (* INT *);
  284 (* NAME *);
  285 (* REF *);
  286 (* PRINTS *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\005\000\007\000\007\000\008\000\008\000\008\000\
\008\000\009\000\009\000\009\000\006\000\006\000\006\000\006\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\000\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\001\000\005\000\005\000\008\000\003\000\
\001\000\003\000\003\000\003\000\001\000\003\000\003\000\003\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\011\000\035\000\001\000\000\000\
\004\000\000\000\026\000\027\000\000\000\017\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\018\000\019\000\022\000\023\000\
\024\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\014\000\000\000\000\000\000\000\
\015\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\031\000\032\000\022\000\023\000\
\024\000"

let yysindex = "\012\000\
\114\255\000\000\008\255\008\255\008\255\008\255\008\255\000\000\
\244\254\008\255\008\255\008\255\000\000\000\000\000\000\114\255\
\000\000\008\255\000\000\000\000\241\254\000\000\103\255\000\000\
\241\254\241\254\241\254\241\254\008\255\030\255\134\255\014\255\
\015\255\000\000\028\255\008\255\008\255\008\255\008\255\008\255\
\008\255\241\254\114\255\008\255\008\255\008\255\008\255\008\255\
\008\255\114\255\114\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\255\241\254\241\254\241\254\241\254\241\254\
\241\254\064\255\074\255\000\000\000\000\017\255\114\255\104\255\
\000\000"

let yyrindex = "\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\000\
\000\000\000\000\000\000\000\000\031\000\000\000\001\000\000\000\
\041\000\071\000\081\000\111\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\121\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\255\033\255\040\255\045\255\046\255\
\049\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\217\255\240\255\015\000\060\000\016\000\000\000\
\053\000"

let yytablesize = 407
let yytable = "\034\000\
\021\000\036\000\037\000\059\000\003\000\004\000\005\000\006\000\
\007\000\008\000\066\000\067\000\001\000\068\000\018\000\029\000\
\002\000\021\000\025\000\026\000\027\000\028\000\050\000\051\000\
\030\000\071\000\009\000\010\000\011\000\012\000\006\000\072\000\
\035\000\013\000\019\000\052\000\020\000\003\000\043\000\029\000\
\007\000\030\000\034\000\042\000\036\000\037\000\036\000\037\000\
\031\000\034\000\034\000\053\000\054\000\032\000\033\000\034\000\
\058\000\034\000\060\000\061\000\062\000\063\000\064\000\065\000\
\003\000\004\000\005\000\006\000\007\000\008\000\010\000\033\000\
\000\000\069\000\003\000\004\000\005\000\006\000\007\000\008\000\
\008\000\000\000\000\000\070\000\000\000\000\000\009\000\010\000\
\011\000\012\000\055\000\056\000\057\000\013\000\000\000\000\000\
\009\000\010\000\011\000\012\000\000\000\000\000\000\000\013\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\000\000\
\000\000\073\000\003\000\004\000\005\000\006\000\007\000\008\000\
\016\000\038\000\039\000\040\000\041\000\000\000\009\000\010\000\
\011\000\012\000\000\000\000\000\000\000\013\000\000\000\000\000\
\009\000\010\000\011\000\012\000\000\000\000\000\000\000\013\000\
\044\000\045\000\046\000\047\000\048\000\049\000\036\000\037\000\
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
\000\000\021\000\021\000\021\000\021\000\021\000\021\000\000\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\021\000\021\000\000\000\000\000\000\000\021\000\006\000\
\006\000\006\000\006\000\006\000\006\000\000\000\000\000\000\000\
\006\000\007\000\007\000\007\000\007\000\007\000\007\000\000\000\
\000\000\000\000\007\000\000\000\000\000\006\000\006\000\006\000\
\006\000\000\000\000\000\000\000\006\000\000\000\000\000\007\000\
\007\000\007\000\007\000\000\000\000\000\000\000\007\000\010\000\
\010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
\010\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000\
\000\000\000\000\008\000\000\000\000\000\010\000\010\000\010\000\
\010\000\000\000\000\000\000\000\010\000\000\000\000\000\008\000\
\008\000\008\000\008\000\000\000\000\000\000\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\000\000\000\000\000\000\
\009\000\016\000\016\000\016\000\016\000\016\000\016\000\000\000\
\000\000\000\000\016\000\000\000\000\000\009\000\009\000\009\000\
\009\000\000\000\000\000\000\000\009\000\000\000\000\000\016\000\
\016\000\016\000\016\000\000\000\000\000\000\000\016\000"

let yycheck = "\016\000\
\000\000\017\001\018\001\043\000\001\001\002\001\003\001\004\001\
\005\001\006\001\050\000\051\000\001\000\010\001\007\001\028\001\
\000\000\003\000\004\000\005\000\006\000\007\000\009\001\009\001\
\010\000\009\001\023\001\024\001\025\001\026\001\000\000\071\000\
\018\000\030\001\027\001\008\001\029\001\000\000\009\001\009\001\
\000\000\009\001\059\000\029\000\017\001\018\001\017\001\018\001\
\009\001\066\000\067\000\036\000\037\000\009\001\009\001\072\000\
\041\000\009\001\044\000\045\000\046\000\047\000\048\000\049\000\
\001\001\002\001\003\001\004\001\005\001\006\001\000\000\012\000\
\255\255\010\001\001\001\002\001\003\001\004\001\005\001\006\001\
\000\000\255\255\255\255\010\001\255\255\255\255\023\001\024\001\
\025\001\026\001\038\000\039\000\040\000\030\001\255\255\255\255\
\023\001\024\001\025\001\026\001\255\255\255\255\255\255\030\001\
\001\001\002\001\003\001\004\001\005\001\006\001\000\000\255\255\
\255\255\010\001\001\001\002\001\003\001\004\001\005\001\006\001\
\000\000\019\001\020\001\021\001\022\001\255\255\023\001\024\001\
\025\001\026\001\255\255\255\255\255\255\030\001\255\255\255\255\
\023\001\024\001\025\001\026\001\255\255\255\255\255\255\030\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
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
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\030\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\010\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\255\255\255\255\023\001\024\001\025\001\
\026\001\255\255\255\255\255\255\030\001\255\255\255\255\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\030\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\010\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\255\255\255\255\023\001\024\001\025\001\
\026\001\255\255\255\255\255\255\030\001\255\255\255\255\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\030\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\010\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\255\255\255\255\023\001\024\001\025\001\
\026\001\255\255\255\255\255\255\030\001\255\255\255\255\023\001\
\024\001\025\001\026\001\255\255\255\255\255\255\030\001"

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
  LEFTC\000\
  RIGHTC\000\
  EQUAL\000\
  DIFF\000\
  LESS\000\
  LESSEQ\000\
  GREATER\000\
  GREATEREQ\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  MOD\000\
  DIV\000\
  POW\000\
  MAKE\000\
  REPEAT\000\
  IF\000\
  IFELSE\000\
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
# 149 "parser.mly"
  ( _1 )
# 386 "parser.ml"
               : Ast.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser.mly"
  ( NOP )
# 392 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd_seq) in
    Obj.repr(
# 156 "parser.mly"
  ( _1 )
# 399 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 161 "parser.mly"
  ( _1 )
# 406 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 163 "parser.mly"
  ( SEQ(_1, _2) )
# 414 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
  ( SYSCALL (Logo.cFORWARD, [_2]) )
# 421 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
  ( SYSCALL (Logo.cBACKWARD, [_2]) )
# 428 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
  ( SYSCALL (Logo.cRIGHT, [_2]) )
# 435 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
  ( SYSCALL (Logo.cLEFT, [_2]) )
# 442 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
  ( PRINT _2 )
# 449 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 178 "parser.mly"
  ( PRINTS _1 )
# 456 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "parser.mly"
  ( SYSCALL (Logo.cHOME, []))
# 462 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 182 "parser.mly"
  ( REPEAT (_2, _4) )
# 470 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 184 "parser.mly"
  ( IF (_2, _4, NOP) )
# 478 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'cmd_seq) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 186 "parser.mly"
  ( IF (_2, _4, _7) )
# 487 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
  ( 
			let var = get_var _2 in
			let var = if var = -1 then make_var _2 else var in
			MAKE(var,_3)
		)
# 499 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 197 "parser.mly"
        ( _1 )
# 506 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 199 "parser.mly"
        ( BINOP (OP_ADD, _1, _3) )
# 514 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 201 "parser.mly"
        ( BINOP (OP_SUB, _1, _3) )
# 522 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 206 "parser.mly"
        ( BINOP (OP_POW, _1, _3) )
# 530 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 208 "parser.mly"
        ( _1 )
# 537 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 213 "parser.mly"
        ( BINOP (OP_MUL, _1, _3) )
# 545 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 215 "parser.mly"
        ( BINOP (OP_MOD, _1, _3) )
# 553 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 217 "parser.mly"
        ( BINOP (OP_DIV, _1, _3) )
# 561 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 219 "parser.mly"
        ( _1 )
# 568 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 224 "parser.mly"
        ( CST _1 )
# 575 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 226 "parser.mly"
        (
            let var = get_var _1 in
            if var = -1 then
                error ("La variable " ^ _1 ^ " n'est pas défini")
            else
                VAR var
        )
# 588 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 234 "parser.mly"
        ( _2 )
# 595 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 239 "parser.mly"
  ( COMP (COMP_EQ, _1, _3))
# 603 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 241 "parser.mly"
  ( COMP (COMP_NE, _1, _3) )
# 611 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 243 "parser.mly"
  ( COMP (COMP_LT, _1, _3) )
# 619 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 245 "parser.mly"
  ( COMP (COMP_LE, _1, _3) )
# 627 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 247 "parser.mly"
  ( COMP (COMP_GT, _1, _3) )
# 635 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 249 "parser.mly"
  ( COMP (COMP_GE, _1, _3) )
# 643 "parser.ml"
               : 'cond))
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
