(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Insert of object_phrase
  | Rotate of object_phrase
  | Score 
  | Undo
  | Quit

exception Empty

exception Malformed

(** [parse str] is the command that results from an input string: 
    - Splits words by ' ' and then filters out "". 
    - If [str] begins with "insert", it uses the constructor [Insert] of [t]  
      where [t] is a string list of the words after "insert". 
    - If [str] is "score", it uses the constructor [Score]. 
    - If [str] is "quit", it uses the constructor [Quit]. 
*)
let parse str =
  match String.split_on_char ' ' str 
        |> List.filter (fun x -> x <> "") with
  | [] -> raise Empty
  | h::t -> if h="quit" then Quit
    else if h="insert" then Insert t
    else if h="undo" then Undo
    else if h="rotate" then Rotate t
    else if h="score" then Score
    else raise Malformed


