type token =
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | POW
  | PUSH
  | GET
  | SET
  | GOTO
  | GOTO_EQ
  | GOTO_NE
  | GOTO_LT
  | GOTO_LE
  | GOTO_GT
  | GOTO_GE
  | INVOKE
  | CALL
  | RESERVE
  | RETURN
  | RETURN_VOID
  | STOP
  | DEBUG
  | GET_GLOB
  | SET_GLOB
  | BOOL of (bool)
  | INT of (int)
  | FLOAT of (float)
  | LABEL of (string)
  | STR of (string)
  | COMMA
  | SHARP
  | COLON
  | META
  | EOF

val listing :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Stackinst.prog_t
