type command = 
  | Insert of int * int
  | RInsert of int
  | Rotate of int
  | Score 
  | Switch
  | Undo
  | Quit

exception Empty

exception Malformed

(** [parse_insert cmd_lst] produces either [RInsert] or [Insert] from string 
    list [cmd_lst]. 
    - If [cmd_lst] has one int [c], [parse_insert] is [RInsert c].
    - If [cmd_lst] has two ints [c] and [v], [parse_insert] is [Insert (c, v)]. 
    - Raise: [Malformed] if [cmd_lst] does not match the above cases. *)
let parse_insert cmd_lst = 
  match cmd_lst with 
  | c::[] -> RInsert (int_of_string c)
  | c :: v :: [] -> Insert (int_of_string c, int_of_string v) 
  | _ -> raise Malformed

(** [parse_rotate cmd_lst] produces [Rotate] commands from strings in [cmd_lst].
    - If [cmd_lst] contains an int [n], [parse_rotate] is [Rotate n].
      Raises: [Malformed] if [cmd_lst] does not match the above cases. *)
let parse_rotate cmd_lst = 
  match cmd_lst with 
  | num :: [] -> Rotate (int_of_string num)
  | _ -> raise Malformed

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
  | RInsert c -> "insert " ^ string_of_int c
  | Rotate n -> "rotate " ^ string_of_int n
  | Score -> "score"
  | Switch -> "switch"
  | Undo -> "undo"
  | Quit -> "quit"