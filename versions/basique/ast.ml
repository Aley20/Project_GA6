type expr =
  | Num of int
  | Var of string
  | Str of string
  | Binop of expr * string * expr

type instr =
  | Print of expr list
  | Assign of string * expr
  | If of expr * string * expr * instr
  | Input of string list
  | End
  | Comment of string
  | Newline
  | Goto of expr

type line = Line of int * instr
type program = line list
