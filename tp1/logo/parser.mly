/*
 * logo -- logo compiler and VM.
 * Copyright (C) 2026  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

%{

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

%}

%token EOF

%token FORWARD
%token BACKWARD
%token PRINT
%token RIGHT
%token LEFT
%token HOME

%token LPAR
%token RPAR
%token LEFTC
%token RIGHTC

%token EQUAL
%token DIFF
%token LESS
%token LESSEQ
%token GREATER
%token GREATEREQ

%token PLUS
%token MINUS
%token TIMES
%token MOD
%token DIV
%token POW

%token MAKE
%token TO
%token END
%token REPEAT
%token IF
%token IFELSE

%token<int> INT
%token<string> NAME
%token <string> REF
%token <string> IDSUBP
%token <string> PRINTS

%start program
%type<Ast.command> program

%%

program:
    opt_cmd_seq
        { $1 }
;

opt_cmd_seq:
    /* empty */
        { NOP }
|   sub_prog_seq
        { $1 }
;

sub_prog_seq:
    sub_prog
        { $1 }
|   sub_prog_seq sub_prog
        { SEQ($1, $2) }
;

sub_prog:
    cmd
        { $1 }
|   TO IDSUBP cmd_seq END
        {
            if get_fun $2 <> -1 then
                error ("Le sous-programme " ^ $2 ^ " est deja defini")
            else
                let _ = make_fun $2 $3 in
                NOP
        }
;

cmd_seq:
	cmd
		{ $1 }
| cmd_seq cmd
		{ SEQ($1, $2) }
;

cmd:
	FORWARD expr
		{ SYSCALL (Logo.cFORWARD, [$2]) }
| 	BACKWARD expr
		{ SYSCALL (Logo.cBACKWARD, [$2]) }
|	RIGHT expr
		{ SYSCALL (Logo.cRIGHT, [$2]) }
| 	LEFT expr
		{ SYSCALL (Logo.cLEFT, [$2]) }
|	PRINT expr
		{ PRINT $2 }
|	PRINTS
		{ PRINTS $1 }
|	HOME
		{ SYSCALL (Logo.cHOME, [])}
|	REPEAT expr LEFTC cmd_seq RIGHTC
		{ REPEAT ($2, $4) }
|  	IF cond LEFTC cmd_seq RIGHTC
		{ IF ($2, $4, NOP) }
|  	IFELSE cond LEFTC cmd_seq RIGHTC LEFTC cmd_seq RIGHTC
		{ IF ($2, $4, $7) }
|   MAKE NAME expr
		{ 
			let var = get_var $2 in
			let var = if var = -1 then make_var $2 else var in
			MAKE(var,$3)
		}
| IDSUBP
    {
        if get_fun $1 = -1 then
            error ("sous programme non def")
        else
            CALL_CMD ($1, 0, [])
    }
;

expr:
    expr2
        { $1 }
|   expr PLUS expr2
        { BINOP (OP_ADD, $1, $3) }
|   expr MINUS expr2
        { BINOP (OP_SUB, $1, $3) }
;

expr2:
    expr3 POW expr2
        { BINOP (OP_POW, $1, $3) }
|   expr3
        { $1 }
;

expr3: 
    expr3 TIMES expr4    
        { BINOP (OP_MUL, $1, $3) }
|   expr3 MOD expr4
        { BINOP (OP_MOD, $1, $3) }
|   expr3 DIV expr4
        { BINOP (OP_DIV, $1, $3) }
|   expr4
        { $1 }
;

expr4: 
    INT
        { CST $1 }
|   REF
        {
            let var = get_var $1 in
            if var = -1 then
                error ("La variable " ^ $1 ^ " n'est pas défini")
            else
                VAR var
        }
|   LPAR expr RPAR
        { $2 }
;

cond:
	expr EQUAL expr
		{ COMP (COMP_EQ, $1, $3)}
|	expr DIFF expr
		{ COMP (COMP_NE, $1, $3) }
|	expr LESS expr
		{ COMP (COMP_LT, $1, $3) }
|	expr LESSEQ expr
		{ COMP (COMP_LE, $1, $3) }
|	expr GREATER expr
		{ COMP (COMP_GT, $1, $3) }
|	expr GREATEREQ expr
		{ COMP (COMP_GE, $1, $3) }





