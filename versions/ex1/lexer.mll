{
  open Parser
}

(* Idem lexer de la version basique *)
rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }  (* Ignore les espaces blancs *)
  | "IMPRIME" { IMPRIME }
  | "SI" { SI }
  | "ALORS" { ALORS }
  | "VAVERS" { VAVERS }
  | "ENTREE" { ENTREE }
  | "FIN" { FIN }
  | "REM" { REM }
  | "NL" { NL }
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