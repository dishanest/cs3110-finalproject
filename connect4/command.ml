type command = 
  | Insert of int * int
  | Rotate of int
  | Score 
  | Switch
  | Undo
  | Quit

exception Empty

exception Malformed

let parse_insert cmd_lst = 
  match cmd_lst with 
  | c :: v :: [] -> Insert (int_of_string c, int_of_string v) 
  | _ -> raise Malformed

let parse_rotate cmd_lst = 
  match cmd_lst with 
  | num :: [] -> Rotate (int_of_string num)
  | _ -> raise Malformed

(** [parse str] is the command that results from an input string: 
    - Splits words by ' ' and then filters out "". 
    - If [str] begins with "insert", it uses the constructor [Insert] of [t]  
      where [t] is a string list of the words after "insert". 
    - If [str] is "score", it uses the constructor [Score]. 
    - If [str] is "quit", it uses the constructor [Quit]. 
*)
let parse str =
  match str |> String.trim |> String.split_on_char ' '
        |> List.filter (fun x -> x <> "") with
  | [] -> raise Empty
  | h :: t -> if h = "quit" && t = [] then Quit
    else if h = "insert" then parse_insert t
    else if h = "undo" && t = [] then Undo
    else if h = "switch" && t = [] then Switch
    else if h = "rotate" then parse_rotate t
    else if h = "score" && t = [] then Score
    else raise Malformed

let string_of_cmd cmd = 
  match cmd with 
  | Insert (c, v) -> "insert " ^ string_of_int c ^ " " ^ string_of_int v
  | Rotate n -> "rotate " ^ string_of_int n
  | Score -> "score"
  | Switch -> "switch"
  | Undo -> "undo"
  | Quit -> "quit"