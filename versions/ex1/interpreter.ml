open Ast

(* Fonction qui trie les lignes du programme par numéro de ligne et supprime les doublons *)
let sort_lines program =
  let sorted_program = List.sort (fun (Line (n1, _)) (Line (n2, _)) -> compare n1 n2) program in
  let rec remove_duplicates acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | (Line (n1, _) as l1) :: (Line (n2, _) as l2) :: rest ->
      if n1 = n2 then remove_duplicates acc (l2 :: rest) (* Si on a plusieurs lignes du même numéro , on choisit la dernière*)
      else remove_duplicates (l1 :: acc) (l2 :: rest) (* Sinon, ajout de la ligne à la liste *)
  in
  remove_duplicates [] sorted_program

(* Fonction qui évalue une expression *)
let rec eval_expr env = function
  | Num n -> n
  | Var v -> Hashtbl.find env v
  | Str s -> failwith "Cannot evaluate a string to an integer"
  | Binop (e1, op, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    match op with
    | "+" -> v1 + v2
    | "-" -> v1 - v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2
    | _ -> failwith "Unknown operator"

(* Fonction qui va retourner la ligne du numéro de ligne num *)
let rec find_line program num =
  match program with
  | [] -> None
  | Line (n, instr) :: rest ->
    if n = num then Some (Line (n, instr))
    else find_line rest num

(* Idem avec un numéro supérieur à current_num *)
let rec find_next_line program current_num =
  match program with
  | [] -> None
  | Line (n, instr) :: rest ->
    if n > current_num then Some (Line (n, instr))
    else find_next_line rest current_num

(* Fonction qui retourne un numéro de ligne *)
let get_line_num = function
  | None -> -1
  | Some (Line (n, _)) -> n

(* Fonction qui exécute un programme à partir d'une ligne donnée.*)
let rec execute_program env program current_line =
  match find_line program current_line with
  | None -> ()
  | Some (Line (num, instr)) ->
    execute_instr env program instr num

and execute_instr env program instr current_line =
  match instr with
  | Print exprs -> (* Intruction pour IMPRIME *)
    List.iter (fun e -> match eval_expr env e with
      | exception Failure _ -> (match e with
        | Str s -> print_string s
        | _ -> failwith "Expected string")
      | n -> print_int n; print_string "") exprs;
    let next_line = find_next_line program current_line in
    execute_program env program (get_line_num next_line) (* Passe à la ligne suivante *)
  | Assign (v, e) -> (* Intruction pour les variables (X,Y, I, ...) *)
    Hashtbl.replace env v (eval_expr env e); (* Met à jour la variable dans l'environnement *)
    let next_line = find_next_line program current_line in
    execute_program env program (get_line_num next_line) (* Passe à la ligne suivante *)
  | Assignments assigns ->  (* Instruction pour l'extension 1 *)
    List.iter (fun (v, e) -> Hashtbl.replace env v (eval_expr env e)) assigns;
    let next_line = find_next_line program current_line in
    execute_program env program (get_line_num next_line)
  | If (e1, op, e2, then_instr) -> (* Instruction pour condition SI *)
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let condition = match op with
      | "=" -> v1 = v2
      | "<" -> v1 < v2
      | ">" -> v1 > v2
      | "<>" | "><" -> v1 <> v2
      | _ -> failwith "Unknown operator" in
    if condition then
      execute_instr env program then_instr current_line
    else
      let next_line = find_next_line program current_line in
      execute_program env program (get_line_num next_line)
  | Input vars -> (* Intruction pour ENTREE *)
    List.iter (fun v ->
      let rec read_int_input () =
        Printf.printf "Enter value for %s: " v;
        flush stdout;
        try int_of_string (input_line stdin)
        with Failure _ ->
          print_endline "Invalid input. Please enter a valid integer.";
          read_int_input ()
      in
      let value = read_int_input () in
      Hashtbl.replace env v value
    ) vars;
    let next_line = find_next_line program current_line in
    execute_program env program (get_line_num next_line)
  | End -> () (* Intruction pour FIN *)
  | Comment s -> (* Intruction pour REM *)
    let next_line = find_next_line program current_line in
    execute_program env program (get_line_num next_line)
  | Newline -> (* Intruction pour NL *)
    print_newline ();
    let next_line = find_next_line program current_line in
    execute_program env program (get_line_num next_line)
  | Goto e -> (* Intruction pour VAVERS *)
    let new_line = eval_expr env e in
    execute_program env program new_line (* Saute à la ligne spécifiée *)

and string_of_expr = function
  | Num n -> string_of_int n
  | Var v -> v
  | Str s -> s
  | Binop (e1, op, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ op ^ " " ^ string_of_expr e2 ^ ")"

let run program =
  let sorted_program = sort_lines program in
  let non_comment_program = List.filter (function Line (_, instr) -> not (match instr with Comment _ -> true | _ -> false)) sorted_program in
  if non_comment_program = [] then failwith "Error: Program contains only comments"; (* Si le programme contient que des commentaires : ERREUR*)
  let env = Hashtbl.create 26 in
  List.iter (fun v -> Hashtbl.add env v 0) ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"];
  execute_program env non_comment_program (match non_comment_program with Line (n, _)::_ -> n | [] -> -1)

let main () =
  (* Vérifie que le programme a été appelé avec un argument *)
  if Array.length Sys.argv <> 2 then
    failwith "Usage: interpreter <file>";
  let file = Sys.argv.(1) in

  (* Lit le contenu du fichier *)
  let file_contents = 
    let ic = open_in file in  (* Ouvre le fichier en lecture *)
    let n = in_channel_length ic in  (* Récupère la taille du fichier *)
    let s = really_input_string ic n in  (* Lit tout le contenu du fichier *)
    close_in ic;  (* Ferme le fichier *)
    s  (* Retourne le contenu du fichier *)
  in

  (* Si le programme est vide : ERREUR*)
  if String.trim file_contents = "" then failwith "Error: Program is empty";
  let lexbuf = Lexing.from_string file_contents in
  let program = Parser.program Lexer.token lexbuf in
  run program

let () = main ()
