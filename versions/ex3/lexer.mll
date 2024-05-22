{
  open Parser
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "IMPRIME" { IMPRIME }
  | "SI" { SI }
  | "ALORS" { ALORS }
  | "VAVERS" { VAVERS }
  | "ENTREE" { ENTREE }
  | "FIN" { FIN }
  | "REM" { REM }
  | "NL" { NL }
  | "SOUSROUTINE" { SOUSROUTINE } (* Ajout token SOUSROUTINE pour extension 3*)
  | "RETOURNE" { RETOURNE } (* Ajout token RETOURNE pour extension 3*)
  | ['0'-'9']+ as c { NUM (int_of_string c) }
  | ['A'-'Z'] as s { VAR (String.make 1 s) }
  | "\"" [^ '\"']* "\"" as str { STRING (String.sub str 1 (String.length str - 2)) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '=' { EQ }
  | '<' { LT }
  | '>' { GT }
  | '!' { NOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | eof { EOF }