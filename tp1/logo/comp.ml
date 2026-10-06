(*
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
 *)

open Ast
open Logo
open Stackinst

(** Array of metas. *)
let meta_arr = Array.make 1024 META_NONE

(** Count of used meta. *)
let meta_cnt = ref 0

(** Count of labels. *)
let label_cnt = ref 0

(** Variable count. *)
let var_cnt = ref 0

(** List of functions. *)
let fun_list: (string * int * Ast.command) list ref = ref []

(** Add a meta-value of type string.
	@param str		String value.
	@retrun			Index of meta. *)
let add_string str =
	let idx = !meta_cnt; in
	incr meta_cnt;
	meta_arr.(idx) <- META_STR str;
	idx

(** Allocate a new anonymous variable.
	@return	Allocated variable number. *)
let alloc_var _ =
	let x = !var_cnt in
	incr var_cnt;
	x

(** Add a new meta.
	@param meta	Meta to store.
	@return		Index of meta. *)
let new_meta meta =
	meta_arr.(!meta_cnt) <- meta;
	incr meta_cnt

(** Create a new label number.
	@return	New label number. *)
let new_label _ =
	let num = !label_cnt in
	incr label_cnt;
	num

(** Add a new function.
	@param name		Function name.
	@param lab		Start label of the function.
	@param ast		AST of the body bof the function. *)
let add_fun name lab ast =
	fun_list := (name, lab, ast)::!fun_list

(** Compile a command.
	@param cmd	Command to compile.
	@return		List of instructions. *)
let rec comp_cmd cmd =
    match cmd with
    | NOP ->
        []
    | SEQ(c1, c2) ->
        (comp_cmd c1) @ (comp_cmd c2)
    | PRINT e ->
        (comp_expr e) @ [INVOKE cPRINT]
    | PRINTS s ->
        [PUSH (add_string s); INVOKE cPRINTS]
    | SYSCALL (cmd, args) ->
        (List.flatten (List.map comp_expr (List.rev args)))
        @ [INVOKE cmd]
    | MAKE(i,e) ->
        (comp_expr e) @ [SET_GLOB i]
    | REPEAT (n, cmd) ->
				let n_var = alloc_var () in
				let i_var = alloc_var () in
				let l_begin = new_label () in
				let l_end = new_label () in
				(comp_expr n)
				@ [SET_GLOB n_var]
				@ [PUSH 0]
				@ [SET_GLOB i_var]
				@ [LABEL l_begin]
				@ [GET_GLOB i_var]
				@ [GET_GLOB n_var]
				@ [GOTO_GE l_end]
				@ (comp_cmd cmd)
				@ [GET_GLOB i_var]
				@ [PUSH 1]
				@ [ADD]
				@ [SET_GLOB i_var]
				@ [GOTO l_begin]
				@ [LABEL l_end]
		| IF (cond, cmd_true, cmd_false) ->
			let l_true = new_label () in
			let l_false = new_label () in
			let l_end = new_label () in

			(comp_cond cond l_true l_false)
			@ [LABEL l_true]
			@ (comp_cmd cmd_true)
			@ [GOTO l_end]
			@ [LABEL l_false]
			@ (comp_cmd cmd_false)
			@ [LABEL l_end]
	
    | _ ->
        failwith "unsupported command!"

	 


(** Compile the provided expression.
	@param expr	Expression to compile.
	@return		Generated instructions. *)
and comp_expr expr =
	match expr with
	| NONE ->
		failwith "attempt to compile NONE expression!"
	| CST n ->
		[PUSH n]
	| VAR i-> 
		[GET_GLOB i]
	| BINOP(opp, expr1, expr2) -> (comp_expr expr2) @ (comp_expr expr1)
		@ (comp_binop opp)
	| _ -> failwith "unsupported expression!"
	 
and comp_binop opp = 
	match opp with
	| OP_ADD -> [ADD]
	| OP_SUB -> [SUB]
	| OP_MUL -> [MUL]
	| OP_DIV -> [DIV]
	| OP_MOD -> [MOD]
	| OP_POW -> [POW]
	| _ -> failwith "unsupported binop"


(** Compile the provided condition.
	@param cond		Condition to translate.
	@param l_true	Label to branch to if condition is true.
	@param l_false	Label to branch to if condition is false.
	@return			Condition quadruplets. *)
and comp_cond cond l_true l_false : Stackinst.inst list =
	match cond with
    | COMP (op, e1, e2) ->
        (comp_expr e2)
        @ (comp_expr e1)
        @ (
            match op with
            | COMP_EQ -> [GOTO_EQ l_true]
            | COMP_NE -> [GOTO_NE l_true]
            | COMP_LT -> [GOTO_LT l_true]
            | COMP_LE -> [GOTO_LE l_true]
            | COMP_GT -> [GOTO_GT l_true]
            | COMP_GE -> [GOTO_GE l_true]
        )
        @ [GOTO l_false]

    | NO_COND ->
        [GOTO l_false]
	| _ -> failwith "unsupported condition!"
	 
