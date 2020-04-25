(* Han's Comments: 
   - When the game begins, size of the board should be at least 4x4
   - 
*)

(** Representation of the state of the game. 

    This module maintains the data stored in a board, including chip colors,
    where chips are on the board, and whose turn it is. It handles 
    reconfiguration of the board for game commands, processing win conditions, 
    as well as printing of the board. 

*)

(** The colors of the chips and players. *)
type color = Red | Blue

(** A cell within the board with color and coordinate information. *)
type cell

(** An representation of a nxn grid of cells *)
type board

(** Abstract data type of values representing states. *)
type t

exception Error
(** [get_cell_color c] is the color of the chip in cell [c]. *)
val get_cell_color: cell -> color

(** [get_cell_value c] is the integer point-value of cell [c]. *)
val get_cell_value: cell -> int

val get_state_color: t -> string
(** [check_win t] is true if someone has won the game. *)
val check_win: t -> bool

(** [insert col v t] inserts a chip with value [v] in column [col] of the board
    in state [t] where color is of the player whose turn it is in [t]
    Precondition: [col] is a valid column and [v] is an in between 0..9
    Raises: "Invalid col" failure if [col] is less than 1 and "Invalid col" 
    failure if the column is already full*)
val insert: int -> int -> t -> t

(** [tick_turn t] advances to the next color's turn by switching colors.  *)
val tick_turn: t -> t

val new_board: int -> int -> 'a option List.t List.t

(** [new_state c row col] creates a new state with an empty board of size 
    [row] by [col] where the starting player is of color [c]
    Requires: [row] and [col] are larger than 0*)
val new_state: color -> int -> int -> t

(** [score t] is the score of the game in its current state. *)
val score: t -> int

(** [rotate rep st] is the state that contains the board of [st] rotated [rep]
    times. 
    Precondition: [rep] must be positive. *)
val rotate: int -> t -> t

(** [print st] pretty-prints a visual representation of board in state [st] onto
    the command line. *)
val print: t -> unit