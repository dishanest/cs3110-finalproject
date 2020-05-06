(** Representation of the state of the game. 

    This module maintains the data stored in a board, including chip colors,
    where chips are on the board, and whose turn it is. It handles 
    reconfiguration of the board for game commands, processing win conditions, 
    as well as printing of the board. 

*)

val invalid_dimensions_err: exn
val invalid_col_err: exn
val full_col_err: exn
val undo_err: exn
val insert_value_err: exn

(** The colors of the chips and players. *)
type color = 
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan

(** The color of the winning player or none if no player wins*)
type win = 
  | NWin
  | Win of color

(** A cell within the board with color and coordinate information. *)
type cell

(** An representation of a nxn grid of cells *)
type board

(** Abstract data type of values representing states. *)
type t

(** [get_cell_color c] is the color of the chip in cell [c]. *)
val get_cell_color: cell -> color

(** [get_cell_value c] is the integer point-value of cell [c]. *)
val get_cell_value: cell -> int

val get_p1_color: t -> color
val get_p2_color: t -> color
val get_current_color: t -> color
val get_dimensions: t -> (int * int)

val string_of_color: color -> string

(** [check_full t] is a bool of whether the board of the state [t] has every
    column completely filled with pieces *)
val check_full: t -> bool 

(** [switch_colors t] is a state with a board identical to that in [t], but 
    which has each piece's color replaced with the color of the other player*)
val switch_colors: t -> t

(** [check_win t n] is [BWin]\[RWin] if blue\red has won the game where the 
    condition is to have int [n] pices of their color in a row where the board 
    checked is that of [t] or [NWin] if neither player wins.
    Requires: [n] is larger than 2 (for a real game) *)
val check_win: t -> int -> win

(** [insert col v t] inserts a chip with value [v] in column [col] of the board
    in state [t] where color is of the player whose turn it is in [t]
    Precondition: [col] is a valid column and [v] is an in between 0..9
    Raises: "Invalid col" failure if [col] is less than 1 and "Invalid col" 
    failure if the column is already full*)
val insert: int -> int -> t -> t

(** [gravity t] Is the state which contains all of the pieces in [t] but whose
    placements are corrected as they would be by gravity (shifted down) *)
val gravity: t -> t

(** [tick_turn t] advances to the next color's turn by switching colors.  *)
val tick_turn: t -> t

(** [undo t] returns the last state before an alteration such as an insertion 
    or a rotation
    Raises: "Could not undo further" failure if undo is called on a new state*)
val undo: t-> t

val new_board: int -> int -> 'a option List.t List.t

(** [new_state (c1, c2) row col] creates a new state with an empty board of size 
    [row] by [col] where the players have colors [c1] and [c2] and the starting 
    player has color [c1]. 
    Requires: [row] and [col] are larger than 4. *)
val new_state: (color * color) -> (int * int) -> t

(** [score st] is the score (s1, s2) of the game in its current state [st], 
    where s1 is player 1's score and s2 is player 2's score. *)
val score: t -> (int * int)

(** [rotate rep st] is the state that contains the board of [st] rotated [rep]
    times. 
    Precondition: [rep] must be positive. *)
val rotate: int -> t -> t

val style_of_color: color -> ANSITerminal.style

(** [print st] pretty-prints a visual representation of board in state [st] onto
    the command line. *)
val print: t -> unit

(** [print_win st win_color] does the same thing as [print] but bolds the winning chips. *)
val print_win: t -> color -> unit