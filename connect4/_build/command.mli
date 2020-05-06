(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Insert of int * int
  | Rotate of int
  | Score
  | Switch
  | Undo 
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

val parse : string -> command

val string_of_cmd : command -> string
