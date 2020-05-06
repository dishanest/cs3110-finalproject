open Command
open State
open Random

type response = command

type difficulty = 
  | Easy 
  | Hard

(** [easy_response st] is an unintelligent and random response built using 
    little contextual understanding of the board. Only returns insert. *)
let rec easy_response st : response = 
  self_init ();
  let (num_cols, _) = get_dimensions st in
  let random_col = Random.int num_cols in 
  let random_val = Random.int 10 in
  Insert (random_col, random_val)

let hard_response st = failwith "unimplemented"

let get_response d st = 
  match d with 
  | Easy -> easy_response st
  | Hard -> hard_response st

