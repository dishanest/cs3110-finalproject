(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)

(** Representation type for Command.  *)
type command = 
  | Insert of int * int
  | RInsert of int
  | Rotate of int
  | Score
  | Switch
  | Undo 
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] is the command representation of string [str].
    - [parse "insert 0 0"] ==> [Insert (0, 0)]
    - [parse "rotate 1"] ==> [Rotate 1]
    - [parse "score"] ==> [Score]
    - [parse "switch"] ==> [Switch]
    - [parse "undo"] ==> [Undo]
    - [parse "quit"] ==> [Quit]
      Raises: [Empty] if [str] is the [""] or only whitespace. [Malformed] if 
      [str] does not take one of the forms listed above. 
      Note: [parse] is not case-sensitive. [RInsert] is not to be parsed. *)
val parse : string -> command

(** [string_of_cmd cmd] is the string representation of a command [cmd]. 
    It is essentially the inverse of the parse function.
    - [string_of_cmd (Insert (0, 0))] ==> "insert 0 0"
    - [string_of_cmd (RInsert 0)] ==> "insert 0"
    - [string_of_cmd (Rotate 0)] ==> "rotate 0"
    - [string_of_cmd (Score)] ==> "score"
    - [string_of_cmd (Switch)] ==> "switch"
    - [string_of_cmd (Undo)] ==> "undo"
    - [string_of_cmd (Quit)] ==> "quit" *) 
val string_of_cmd : command -> string
