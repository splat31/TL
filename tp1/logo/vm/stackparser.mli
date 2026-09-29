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
  | BOOL of (
# 140 "stackparser.mly"
        bool
# 31 "stackparser.mli"
)
  | INT of (
# 141 "stackparser.mly"
        int
# 36 "stackparser.mli"
)
  | FLOAT of (
# 142 "stackparser.mly"
        float
# 41 "stackparser.mli"
)
  | LABEL of (
# 143 "stackparser.mly"
        string
# 46 "stackparser.mli"
)
  | STR of (
# 144 "stackparser.mly"
        string
# 51 "stackparser.mli"
)
  | COMMA
  | SHARP
  | COLON
  | META
  | EOF

val listing :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Stackinst.prog_t
