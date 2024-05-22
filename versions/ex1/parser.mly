%{
  open Ast
%}

%token <int> NUM
%token <string> STRING
%token <string> VAR
%token IMPRIME SI ALORS VAVERS ENTREE FIN REM NL
%token PLUS MINUS TIMES DIV EQ LT GT NOT LPAREN RPAREN COMMA
%token EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc LT GT EQ
%right NOT

%start program
%type <Ast.program> program
%type <Ast.line list> lines
%type <Ast.line> line
%type <Ast.instr> instr
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <(string * Ast.expr)> assignment  (* Ajout pour l'extension 1 *)
%type <(string * Ast.expr) list> assignments (* Idem *)
%type <string list> var_list

%%

program:
  | lines EOF { $1 }

lines:
  | line { [$1] }
  | line lines { $1 :: $2 }

line:
  | NUM instr { Line ($1, $2) }

instr:
  | IMPRIME expr_list { Print $2 }
  | SI expr LT expr ALORS instr { If ($2, "<", $4, $6) }
  | SI expr GT expr ALORS instr { If ($2, ">", $4, $6) }
  | SI expr EQ expr ALORS instr { If ($2, "=", $4, $6) }
  | VAVERS expr { Goto $2 }
  | ENTREE var_list { Input $2 }
  | assignments { Assignments $1 }
  | FIN { End }
  | REM STRING { Comment $2 }
  | NL { Newline }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

var_list:
  | VAR { [$1] }
  | VAR COMMA var_list { $1 :: $3 }


(* Ajout pour l'extension 1 *)
assignments:
  | assignment { [$1] } 
  | assignment COMMA assignments { $1 :: $3 } 

assignment:
  | VAR EQ expr { ($1, $3) }

expr:
  | NUM { Num $1 }
  | VAR { Var $1 }
  | STRING { Str $1 }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { Binop ($1, "+", $3) }
  | expr MINUS expr { Binop ($1, "-", $3) }
  | expr TIMES expr { Binop ($1, "*", $3) }
  | expr DIV expr { Binop ($1, "/", $3) }
  | expr LT expr { Binop ($1, "<", $3) }
  | expr GT expr { Binop ($1, ">", $3) }
  | expr EQ expr { Binop ($1, "=", $3) }
  | NOT expr { Binop (Num 0, "-", $2) }
