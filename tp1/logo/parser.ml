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
  | TO
  | END
  | REPEAT
  | IF
  | IFELSE
  | INT of (int)
  | NAME of (string)
  | REF of (string)
  | IDSUBP of (string)
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

# 126 "parser.ml"
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
  280 (* TO *);
  281 (* END *);
  282 (* REPEAT *);
  283 (* IF *);
  284 (* IFELSE *);
    0|]

let yytransl_block = [|
  285 (* INT *);
  286 (* NAME *);
  287 (* REF *);
  288 (* IDSUBP *);
  289 (* PRINTS *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\007\000\007\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\009\000\009\000\
\009\000\012\000\012\000\013\000\013\000\013\000\013\000\014\000\
\014\000\014\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\000\000\001\000\001\000\002\000\001\000\004\000\002\000\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\001\000\
\001\000\005\000\005\000\008\000\003\000\002\000\001\000\003\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000\002\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\041\000\016\000\045\000\
\001\000\000\000\004\000\006\000\000\000\032\000\033\000\000\000\
\023\000\000\000\031\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\024\000\025\000\
\028\000\029\000\030\000\026\000\044\000\007\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\019\000\000\000\000\000\000\000\020\000"

let yydgoto = "\002\000\
\016\000\017\000\018\000\019\000\050\000\034\000\051\000\049\000\
\036\000\037\000\039\000\025\000\026\000\027\000"

let yysindex = "\255\255\
\067\255\000\000\254\254\254\254\254\254\254\254\254\254\000\000\
\228\254\239\254\254\254\254\254\254\254\000\000\000\000\000\000\
\000\000\067\255\000\000\000\000\254\254\000\000\000\000\001\255\
\000\000\019\255\000\000\001\255\001\255\001\255\001\255\254\254\
\000\000\201\255\008\255\224\255\007\255\012\255\254\254\000\000\
\005\255\254\254\254\254\254\254\254\254\254\254\254\254\001\255\
\253\254\000\000\102\255\201\255\254\254\254\254\254\254\254\254\
\254\254\254\254\201\255\201\255\001\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\113\255\
\001\255\001\255\001\255\001\255\001\255\001\255\146\255\157\255\
\000\000\000\000\021\255\201\255\190\255\000\000"

let yyrindex = "\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\000\000\000\000\000\000\000\000\000\000\000\045\000\
\000\000\001\000\000\000\078\000\089\000\122\000\133\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\166\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\177\000\
\210\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\255\037\255\038\255\042\255\043\255\045\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\037\000\002\000\000\000\208\255\000\000\
\003\000\049\000\000\000\246\255\000\000\004\000"

let yytablesize = 499
let yytable = "\001\000\
\027\000\032\000\020\000\072\000\021\000\024\000\028\000\029\000\
\030\000\031\000\079\000\080\000\062\000\035\000\033\000\059\000\
\052\000\042\000\043\000\020\000\060\000\042\000\043\000\041\000\
\042\000\043\000\022\000\069\000\023\000\084\000\002\000\063\000\
\064\000\042\000\048\000\085\000\068\000\044\000\045\000\046\000\
\047\000\061\000\003\000\035\000\011\000\036\000\037\000\065\000\
\066\000\067\000\038\000\039\000\071\000\040\000\040\000\073\000\
\074\000\075\000\076\000\077\000\078\000\038\000\000\000\000\000\
\000\000\000\000\000\000\003\000\004\000\005\000\006\000\007\000\
\008\000\071\000\000\000\000\000\000\000\012\000\000\000\000\000\
\071\000\071\000\000\000\000\000\000\000\000\000\071\000\000\000\
\015\000\009\000\010\000\000\000\011\000\012\000\013\000\000\000\
\000\000\000\000\014\000\015\000\000\000\000\000\003\000\004\000\
\005\000\006\000\007\000\008\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\007\000\008\000\000\000\
\000\000\013\000\081\000\000\000\009\000\000\000\070\000\011\000\
\012\000\013\000\000\000\000\000\014\000\014\000\015\000\009\000\
\000\000\000\000\011\000\012\000\013\000\000\000\000\000\000\000\
\014\000\015\000\003\000\004\000\005\000\006\000\007\000\008\000\
\000\000\000\000\000\000\082\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\000\000\000\000\022\000\083\000\000\000\
\009\000\000\000\000\000\011\000\012\000\013\000\000\000\000\000\
\021\000\014\000\015\000\009\000\000\000\000\000\011\000\012\000\
\013\000\000\000\000\000\000\000\014\000\015\000\003\000\004\000\
\005\000\006\000\007\000\008\000\000\000\000\000\000\000\086\000\
\000\000\003\000\004\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\011\000\
\012\000\013\000\000\000\000\000\000\000\014\000\015\000\009\000\
\000\000\000\000\011\000\012\000\013\000\000\000\000\000\000\000\
\014\000\015\000\053\000\054\000\055\000\056\000\057\000\058\000\
\042\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\000\000\000\000\000\000\000\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\000\000\027\000\
\027\000\027\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\000\000\000\000\042\000\000\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\000\000\000\000\011\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\000\000\
\042\000\042\000\042\000\011\000\011\000\011\000\011\000\011\000\
\011\000\000\000\000\000\000\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\012\000\000\000\000\000\000\000\012\000\
\000\000\015\000\015\000\015\000\015\000\015\000\015\000\000\000\
\000\000\000\000\015\000\000\000\012\000\012\000\012\000\012\000\
\012\000\012\000\000\000\000\000\000\000\012\000\012\000\015\000\
\015\000\015\000\015\000\015\000\015\000\000\000\000\000\000\000\
\015\000\015\000\013\000\013\000\013\000\013\000\013\000\013\000\
\000\000\000\000\000\000\013\000\000\000\014\000\014\000\014\000\
\014\000\014\000\014\000\000\000\000\000\000\000\014\000\000\000\
\013\000\013\000\013\000\013\000\013\000\013\000\000\000\000\000\
\000\000\013\000\013\000\014\000\014\000\014\000\014\000\014\000\
\014\000\000\000\000\000\000\000\014\000\014\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\000\000\000\000\022\000\
\000\000\021\000\021\000\021\000\021\000\021\000\021\000\000\000\
\000\000\000\000\021\000\000\000\022\000\022\000\022\000\022\000\
\022\000\022\000\000\000\000\000\000\000\022\000\022\000\021\000\
\021\000\021\000\021\000\021\000\021\000\000\000\000\000\000\000\
\021\000\021\000\008\000\008\000\008\000\008\000\008\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\008\000\008\000\008\000\000\000\000\000\
\000\000\008\000\008\000"

let yycheck = "\001\000\
\000\000\030\001\001\000\052\000\007\001\003\000\004\000\005\000\
\006\000\007\000\059\000\060\000\008\001\011\000\032\001\009\001\
\009\001\017\001\018\001\018\000\009\001\017\001\018\001\021\000\
\017\001\018\001\029\001\031\001\031\001\009\001\000\000\042\000\
\043\000\000\000\032\000\084\000\047\000\019\001\020\001\021\001\
\022\001\039\000\000\000\009\001\000\000\009\001\009\001\044\000\
\045\000\046\000\009\001\009\001\051\000\009\001\018\000\053\000\
\054\000\055\000\056\000\057\000\058\000\013\000\255\255\255\255\
\255\255\255\255\255\255\001\001\002\001\003\001\004\001\005\001\
\006\001\072\000\255\255\255\255\255\255\000\000\255\255\255\255\
\079\000\080\000\255\255\255\255\255\255\255\255\085\000\255\255\
\000\000\023\001\024\001\255\255\026\001\027\001\028\001\255\255\
\255\255\255\255\032\001\033\001\255\255\255\255\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\000\000\010\001\255\255\023\001\255\255\025\001\026\001\
\027\001\028\001\255\255\255\255\000\000\032\001\033\001\023\001\
\255\255\255\255\026\001\027\001\028\001\255\255\255\255\255\255\
\032\001\033\001\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\255\255\255\255\010\001\255\255\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\000\000\010\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\255\255\255\255\
\000\000\032\001\033\001\023\001\255\255\255\255\026\001\027\001\
\028\001\255\255\255\255\255\255\032\001\033\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\010\001\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\023\001\255\255\255\255\026\001\
\027\001\028\001\255\255\255\255\255\255\032\001\033\001\023\001\
\255\255\255\255\026\001\027\001\028\001\255\255\255\255\255\255\
\032\001\033\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\255\255\031\001\
\032\001\033\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\255\255\255\255\010\001\255\255\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\255\255\010\001\255\255\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\255\255\
\031\001\032\001\033\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\032\001\033\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\010\001\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\255\255\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\255\255\032\001\033\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\255\255\255\255\
\032\001\033\001\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\255\255\255\255\010\001\255\255\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\255\255\010\001\255\255\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\255\255\032\001\033\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\032\001\033\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\010\001\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\255\255\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\255\255\032\001\033\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\255\255\255\255\
\032\001\033\001\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\255\255\255\255\
\255\255\032\001\033\001"

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
  TO\000\
  END\000\
  REPEAT\000\
  IF\000\
  IFELSE\000\
  "

let yynames_block = "\
  INT\000\
  NAME\000\
  REF\000\
  IDSUBP\000\
  PRINTS\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'opt_cmd_seq) in
    Obj.repr(
# 152 "parser.mly"
        ( _1 )
# 408 "parser.ml"
               : Ast.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
        ( NOP )
# 414 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_prog_seq) in
    Obj.repr(
# 159 "parser.mly"
        ( _1 )
# 421 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_prog) in
    Obj.repr(
# 164 "parser.mly"
        ( _1 )
# 428 "parser.ml"
               : 'sub_prog_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sub_prog_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub_prog) in
    Obj.repr(
# 166 "parser.mly"
        ( SEQ(_1, _2) )
# 436 "parser.ml"
               : 'sub_prog_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 171 "parser.mly"
        ( _1 )
# 443 "parser.ml"
               : 'sub_prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'subprog_header) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 173 "parser.mly"
    (
      let (name, params) = _2 in
      let body = _3 in
      let _ = make_fun name body in
      release_params params;
      NOP
    )
# 457 "parser.ml"
               : 'sub_prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 184 "parser.mly"
    (
      if get_fun _1 <> -1 then
        error ("Le sous-programme " ^ _1 ^ " est deja defini");
      declare_params _2;
      (_1, _2)
    )
# 470 "parser.ml"
               : 'subprog_header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 193 "parser.mly"
  ( _1 )
# 477 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 195 "parser.mly"
  ( SEQ(_1, _2) )
# 485 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
  ( SYSCALL (Logo.cFORWARD, [_2]) )
# 492 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
  ( SYSCALL (Logo.cBACKWARD, [_2]) )
# 499 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 204 "parser.mly"
  ( SYSCALL (Logo.cRIGHT, [_2]) )
# 506 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 206 "parser.mly"
  ( SYSCALL (Logo.cLEFT, [_2]) )
# 513 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 208 "parser.mly"
  ( PRINT _2 )
# 520 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 210 "parser.mly"
  ( PRINTS _1 )
# 527 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 212 "parser.mly"
  ( SYSCALL (Logo.cHOME, []))
# 533 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 214 "parser.mly"
  ( REPEAT (_2, _4) )
# 541 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 216 "parser.mly"
  ( IF (_2, _4, NOP) )
# 549 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'cmd_seq) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 218 "parser.mly"
  ( IF (_2, _4, _7) )
# 558 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 220 "parser.mly"
  ( 
			let var = get_var _2 in
			let var = if var = -1 then make_var _2 else var in
			MAKE(var,_3)
		)
# 570 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 226 "parser.mly"
    (
      let label = get_fun _1 in
        if label = -1 then
            error ("sous programme non def")
        else
            CALL_CMD (_1, label, _2)
    )
# 584 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 237 "parser.mly"
        ( _1 )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 239 "parser.mly"
        ( BINOP (OP_ADD, _1, _3) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 241 "parser.mly"
        ( BINOP (OP_SUB, _1, _3) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 246 "parser.mly"
        ( BINOP (OP_POW, _1, _3) )
# 615 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 248 "parser.mly"
        ( _1 )
# 622 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 253 "parser.mly"
        ( BINOP (OP_MUL, _1, _3) )
# 630 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 255 "parser.mly"
        ( BINOP (OP_MOD, _1, _3) )
# 638 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 257 "parser.mly"
        ( BINOP (OP_DIV, _1, _3) )
# 646 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 259 "parser.mly"
        ( _1 )
# 653 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 264 "parser.mly"
        ( CST _1 )
# 660 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 266 "parser.mly"
        (
			let var = get_var _1 in
			if var = -1 then
			error ("La variable " ^ _1 ^ " n'est pas définie");
			if is_local _1 then
			LOCAL_VAR var
			else
			VAR var
        )
# 675 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 276 "parser.mly"
        ( _2 )
# 682 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 281 "parser.mly"
  ( COMP (COMP_EQ, _1, _3))
# 690 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 283 "parser.mly"
  ( COMP (COMP_NE, _1, _3) )
# 698 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 285 "parser.mly"
  ( COMP (COMP_LT, _1, _3) )
# 706 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 287 "parser.mly"
  ( COMP (COMP_LE, _1, _3) )
# 714 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 289 "parser.mly"
  ( COMP (COMP_GT, _1, _3) )
# 722 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 291 "parser.mly"
  ( COMP (COMP_GE, _1, _3) )
# 730 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    Obj.repr(
# 295 "parser.mly"
        ( [] )
# 736 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 297 "parser.mly"
        ( _1 @ [_2] )
# 744 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 301 "parser.mly"
        ( [] )
# 750 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 303 "parser.mly"
        ( _1 @ [_2] )
# 758 "parser.ml"
               : 'params))
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
