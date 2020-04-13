(** Representation of the state of the game. 

    This module maintains the data stored in a board, including chip colors,
    where chips are on the board, and whose turn it is. It handles 
    reconfiguration of the board for game commands, processing win conditions, 
    as well as printing of the board. 

*)

(** The colors of the chips and players. *)
type color

(** A cell within the board with color and coordinate information. *)
type cell

(** An representation of a nxn grid of cells *)
type board

(** Abstract data type of values representing states. *)
type t

(** [print b] pretty-prints a visual representation of board [b] onto the 
    command line *)
val print: board -> unit

(** [get_cell_color c] is Some of the color of the chip in cell [c], and None 
    if the cell is empty. *)
val get_cell_color: cell -> color option

(** [get_cell_value c] is the integer point-value of cell [c]. *)
val get_cell_value: cell -> int

(** [check_win t] is the color of the player that won the game. *)
val check_win: t -> color option

(** [insert x y value b] inserts a chip into the specified cell with the correct
    value. *)
val insert: int -> int -> int -> board -> board

(** [tick_turn t] advances to the next color's turn by switching colors.  *)
val tick_turn: t -> t

(** [score t] is the score of the game in its current state. *)
val score: t -> int
