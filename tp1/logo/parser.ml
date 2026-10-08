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
  | INT of (
# 139 "parser.mly"
       int
# 35 "parser.ml"
)
  | NAME of (
# 140 "parser.mly"
       string
# 40 "parser.ml"
)
  | REF of (
# 141 "parser.mly"
        string
# 45 "parser.ml"
)
  | IDSUBP of (
# 142 "parser.mly"
        string
# 50 "parser.ml"
)
  | PRINTS of (
# 143 "parser.mly"
        string
# 55 "parser.ml"
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

# 146 "parser.ml"
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
\006\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\007\000\007\000\007\000\
\009\000\009\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\000\000\001\000\001\000\002\000\001\000\004\000\001\000\
\002\000\002\000\002\000\002\000\002\000\002\000\001\000\001\000\
\005\000\005\000\008\000\003\000\001\000\001\000\003\000\003\000\
\003\000\001\000\003\000\003\000\003\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\000\000\000\000\000\000\000\000\000\000\021\000\015\000\040\000\
\001\000\000\000\004\000\006\000\000\000\031\000\032\000\000\000\
\022\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\023\000\024\000\027\000\028\000\029\000\025\000\
\007\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\018\000\000\000\000\000\000\000\
\019\000"

let yydgoto = "\002\000\
\016\000\017\000\018\000\019\000\047\000\048\000\035\000\036\000\
\025\000\026\000\027\000"

let yysindex = "\014\000\
\036\255\000\000\252\254\252\254\252\254\252\254\252\254\000\000\
\242\254\253\254\252\254\252\254\252\254\000\000\000\000\000\000\
\000\000\036\255\000\000\000\000\252\254\000\000\000\000\243\254\
\000\000\029\255\000\000\243\254\243\254\243\254\243\254\252\254\
\179\255\013\255\202\255\017\255\019\255\000\000\250\254\252\254\
\252\254\252\254\252\254\252\254\252\254\243\254\000\000\079\255\
\179\255\252\254\252\254\252\254\252\254\252\254\252\254\179\255\
\179\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\091\255\243\254\243\254\243\254\243\254\243\254\
\243\254\124\255\135\255\000\000\000\000\023\255\179\255\168\255\
\000\000"

let yyrindex = "\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\034\000\
\000\000\001\000\000\000\045\000\078\000\089\000\122\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\133\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\255\037\255\043\255\052\255\056\255\
\058\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\057\000\255\255\220\255\003\000\063\000\
\234\255\000\000\028\000"

let yytablesize = 422
let yytable = "\020\000\
\026\000\058\000\021\000\040\000\041\000\024\000\028\000\029\000\
\030\000\031\000\040\000\041\000\067\000\034\000\001\000\032\000\
\020\000\059\000\060\000\074\000\075\000\049\000\064\000\039\000\
\022\000\056\000\023\000\057\000\033\000\040\000\041\000\079\000\
\002\000\010\000\046\000\003\000\003\000\004\000\005\000\006\000\
\007\000\008\000\080\000\034\000\011\000\035\000\066\000\042\000\
\043\000\044\000\045\000\036\000\068\000\069\000\070\000\071\000\
\072\000\073\000\009\000\010\000\037\000\011\000\012\000\013\000\
\038\000\066\000\039\000\014\000\015\000\061\000\062\000\063\000\
\066\000\066\000\038\000\037\000\000\000\014\000\066\000\003\000\
\004\000\005\000\006\000\007\000\008\000\000\000\000\000\000\000\
\012\000\000\000\000\000\003\000\004\000\005\000\006\000\007\000\
\008\000\000\000\000\000\000\000\076\000\009\000\000\000\065\000\
\011\000\012\000\013\000\000\000\000\000\000\000\014\000\015\000\
\000\000\009\000\000\000\000\000\011\000\012\000\013\000\000\000\
\000\000\013\000\014\000\015\000\003\000\004\000\005\000\006\000\
\007\000\008\000\000\000\000\000\020\000\077\000\000\000\003\000\
\004\000\005\000\006\000\007\000\008\000\000\000\000\000\000\000\
\078\000\000\000\009\000\000\000\000\000\011\000\012\000\013\000\
\000\000\000\000\000\000\014\000\015\000\009\000\000\000\000\000\
\011\000\012\000\013\000\000\000\000\000\000\000\014\000\015\000\
\003\000\004\000\005\000\006\000\007\000\008\000\000\000\000\000\
\000\000\081\000\000\000\003\000\004\000\005\000\006\000\007\000\
\008\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\011\000\012\000\013\000\000\000\000\000\000\000\014\000\
\015\000\009\000\000\000\000\000\011\000\012\000\013\000\000\000\
\000\000\000\000\014\000\015\000\050\000\051\000\052\000\053\000\
\054\000\055\000\040\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\026\000\026\000\026\000\026\000\026\000\000\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\000\000\000\000\000\000\000\000\026\000\
\026\000\026\000\026\000\026\000\026\000\000\000\000\000\000\000\
\026\000\026\000\010\000\010\000\010\000\010\000\010\000\010\000\
\000\000\000\000\000\000\010\000\000\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\000\000\000\000\011\000\000\000\
\010\000\010\000\010\000\010\000\010\000\010\000\000\000\000\000\
\000\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\000\000\000\000\000\000\011\000\011\000\014\000\014\000\
\014\000\014\000\014\000\014\000\000\000\000\000\000\000\014\000\
\000\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000\
\000\000\000\000\012\000\000\000\014\000\014\000\014\000\014\000\
\014\000\014\000\000\000\000\000\000\000\014\000\014\000\012\000\
\012\000\012\000\012\000\012\000\012\000\000\000\000\000\000\000\
\012\000\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
\000\000\000\000\000\000\013\000\000\000\020\000\020\000\020\000\
\020\000\020\000\020\000\000\000\000\000\000\000\020\000\000\000\
\013\000\013\000\013\000\013\000\013\000\013\000\000\000\000\000\
\000\000\013\000\013\000\020\000\020\000\020\000\020\000\020\000\
\020\000\000\000\000\000\000\000\020\000\020\000"

let yycheck = "\001\000\
\000\000\008\001\007\001\017\001\018\001\003\000\004\000\005\000\
\006\000\007\000\017\001\018\001\049\000\011\000\001\000\030\001\
\018\000\040\000\041\000\056\000\057\000\009\001\045\000\021\000\
\029\001\009\001\031\001\009\001\032\001\017\001\018\001\009\001\
\000\000\000\000\032\000\000\000\001\001\002\001\003\001\004\001\
\005\001\006\001\079\000\009\001\000\000\009\001\048\000\019\001\
\020\001\021\001\022\001\009\001\050\000\051\000\052\000\053\000\
\054\000\055\000\023\001\024\001\009\001\026\001\027\001\028\001\
\009\001\067\000\009\001\032\001\033\001\042\000\043\000\044\000\
\074\000\075\000\018\000\013\000\255\255\000\000\080\000\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\000\000\255\255\255\255\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\255\255\010\001\023\001\255\255\025\001\
\026\001\027\001\028\001\255\255\255\255\255\255\032\001\033\001\
\255\255\023\001\255\255\255\255\026\001\027\001\028\001\255\255\
\255\255\000\000\032\001\033\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\255\255\000\000\010\001\255\255\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\010\001\255\255\023\001\255\255\255\255\026\001\027\001\028\001\
\255\255\255\255\255\255\032\001\033\001\023\001\255\255\255\255\
\026\001\027\001\028\001\255\255\255\255\255\255\032\001\033\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\255\255\
\255\255\010\001\255\255\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\023\001\255\255\
\255\255\026\001\027\001\028\001\255\255\255\255\255\255\032\001\
\033\001\023\001\255\255\255\255\026\001\027\001\028\001\255\255\
\255\255\255\255\032\001\033\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
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
\255\255\255\255\255\255\010\001\255\255\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\255\255\010\001\255\255\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\255\255\032\001\033\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\032\001\033\001"

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
# 406 "parser.ml"
               : Ast.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
        ( NOP )
# 412 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_prog_seq) in
    Obj.repr(
# 159 "parser.mly"
        ( _1 )
# 419 "parser.ml"
               : 'opt_cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_prog) in
    Obj.repr(
# 164 "parser.mly"
        ( _1 )
# 426 "parser.ml"
               : 'sub_prog_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sub_prog_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub_prog) in
    Obj.repr(
# 166 "parser.mly"
        ( SEQ(_1, _2) )
# 434 "parser.ml"
               : 'sub_prog_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 171 "parser.mly"
        ( _1 )
# 441 "parser.ml"
               : 'sub_prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 173 "parser.mly"
        (
            if get_fun _2 <> -1 then
                error ("Le sous-programme " ^ _2 ^ " est deja defini")
            else
                let _ = make_fun _2 _3 in
                NOP
        )
# 455 "parser.ml"
               : 'sub_prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 184 "parser.mly"
  ( _1 )
# 462 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 186 "parser.mly"
  ( SEQ(_1, _2) )
# 470 "parser.ml"
               : 'cmd_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
  ( SYSCALL (Logo.cFORWARD, [_2]) )
# 477 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
  ( SYSCALL (Logo.cBACKWARD, [_2]) )
# 484 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
  ( SYSCALL (Logo.cRIGHT, [_2]) )
# 491 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "parser.mly"
  ( SYSCALL (Logo.cLEFT, [_2]) )
# 498 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "parser.mly"
  ( PRINT _2 )
# 505 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 201 "parser.mly"
  ( PRINTS _1 )
# 512 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 203 "parser.mly"
  ( SYSCALL (Logo.cHOME, []))
# 518 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 205 "parser.mly"
  ( REPEAT (_2, _4) )
# 526 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 207 "parser.mly"
  ( IF (_2, _4, NOP) )
# 534 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'cmd_seq) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'cmd_seq) in
    Obj.repr(
# 209 "parser.mly"
  ( IF (_2, _4, _7) )
# 543 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 211 "parser.mly"
  ( 
			let var = get_var _2 in
			let var = if var = -1 then make_var _2 else var in
			MAKE(var,_3)
		)
# 555 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 217 "parser.mly"
    (
        if get_fun _1 = -1 then
            error ("sous programme non def")
        else
            CALL_CMD (_1, 0, [])
    )
# 567 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 227 "parser.mly"
        ( _1 )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 229 "parser.mly"
        ( BINOP (OP_ADD, _1, _3) )
# 582 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 231 "parser.mly"
        ( BINOP (OP_SUB, _1, _3) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 236 "parser.mly"
        ( BINOP (OP_POW, _1, _3) )
# 598 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 238 "parser.mly"
        ( _1 )
# 605 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 243 "parser.mly"
        ( BINOP (OP_MUL, _1, _3) )
# 613 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 245 "parser.mly"
        ( BINOP (OP_MOD, _1, _3) )
# 621 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 247 "parser.mly"
        ( BINOP (OP_DIV, _1, _3) )
# 629 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 249 "parser.mly"
        ( _1 )
# 636 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 254 "parser.mly"
        ( CST _1 )
# 643 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 256 "parser.mly"
        (
            let var = get_var _1 in
            if var = -1 then
                error ("La variable " ^ _1 ^ " n'est pas défini")
            else
                VAR var
        )
# 656 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 264 "parser.mly"
        ( _2 )
# 663 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 269 "parser.mly"
  ( COMP (COMP_EQ, _1, _3))
# 671 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 271 "parser.mly"
  ( COMP (COMP_NE, _1, _3) )
# 679 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 273 "parser.mly"
  ( COMP (COMP_LT, _1, _3) )
# 687 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 275 "parser.mly"
  ( COMP (COMP_LE, _1, _3) )
# 695 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 277 "parser.mly"
  ( COMP (COMP_GT, _1, _3) )
# 703 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 279 "parser.mly"
  ( COMP (COMP_GE, _1, _3) )
# 711 "parser.ml"
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
