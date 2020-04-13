(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Quit

exception Empty

exception Malformed

(** [parse str] is the command that results from an input string: 
    - Splits words by ' ' and then filters out "". 
    - If [str] begins with "go", it uses the constructor [Go] of [t] where [t] 
      is a string list of the words after "go". 
    - If [str] is "quit", it uses the constructor [Quit]. 
*)
let parse str =
  match begin 
    str |> String.split_on_char ' ' 
    |> List.filter_map (fun elt -> if elt = "" then None else Some elt) 
  end with
  | [] -> raise Empty 
  | h :: t -> 
    if h = "go" && t <> [] then Go t else 
    if h = "quit" && t = [] then Quit else 
      raise Malformed


