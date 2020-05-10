(** Representation of the state of the game. 

    This module maintains the data stored in a board, including chip colors,
    where chips are on the board, and whose turn it is. It handles 
    reconfiguration of the board for game commands, processing win conditions, 
    as well as printing of the board. *)

(** The exn raised for creating a new board with invalid dimensions. *)
val invalid_dimensions_err: exn
(** The exn raised for attempting to access an invalid column. *)
val invalid_col_err: exn
(** The exn raised for attempting to insert into a full column. *)
val full_col_err: exn
(** The exn raised for attempting to undo without first making a move. *)
val undo_err: exn
(** The exn raised for inserting a value that has alread been used. *)
val insert_value_err: exn

(** The colors of the chips and players. *)
type color = 
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan

(** Information about whether a player has won the game. *)
type win = 
  | NWin
  | Win of color

(** A cell within the board with color and coordinate information. *)
type cell

(** An representation of a n x n grid of cells *)
type board

(** Abstract data type of values representing states. *)
type t

(** [get_cell_color c] is the color of the chip in cell [c]. *)
val get_cell_color: cell -> color

(** [get_cell_value c] is the integer point-value of cell [c]. *)
val get_cell_value: cell -> int

(** [get_board st] is the board contained within state [st]. *)
val get_board: t -> board

(** [get_p1_color st] is the color of player 1 in state [st]. *)
val get_p1_color: t -> color

(** [get_p2_color st] is the color of player 2 in state [st]. *)
val get_p2_color: t -> color

(** [get_current_color st] is the color of the player whose turn it currently is
    in state [st]. *)
val get_current_color: t -> color

(** [get_dimensions st] is the dimensions of the board in state [st]. *)
val get_dimensions: t -> (int * int)

(** [get_gamemode st] is [true] if the Random gamemode is active and [false] if 
    the Normal gamemode is active.  *)
val get_gamemode: t -> bool

(** [string_of_color c] is the string that represents the color [c]. *)
val string_of_color: color -> string

(** [check_full st] is [true] if the board in state [st] has every
    column completely filled with chips. *)
val check_full: t -> bool 

(** [switch_colors st] is identical to [st], but each chip has its color
    switched to the color of the other player. *)
val switch_colors: t -> t

(** [make_assoc c b init_col] is an association list that maps all the cells
    of color [c] in board [b] to their coordinates, beginning at column 
    [init_col]. 
    Requires: [init_col] >= 0. *)
val make_assoc: color -> board -> int -> (int*int) list

(** [check_win st n] is [Win c] if the player with color [c] has won the game. 
    To win the game, a player must connect [n] pieces consecutively in a 
    horizontal, vertical, or diagonal line. If none of these conditions are met,
    [check_win] is [NWin], meaning nobody has won (yet). 
    Note: the game engine itself uses only [n] = 4. 
    Requires: [n] > 0. *)
val check_win: t -> int -> win

(** [check_val_used c v st] is true if the player of color [c] has already 
    inserted a chip of int [v] in state [st]. *)
val check_val_used: color -> int -> t -> bool

(** [get_valid_int c st] is a valid int the player of color [c] can insert*)
val get_valid_int: color -> t -> int

(** [insert col v t] inserts a chip with value [v] in column [col] of the board
    in state [t] where color is of the player whose turn it is in [t]
    Precondition: [col] is a valid column and [v] is an in between 0..9
    Raises: [invalid_col_err] if [col] is less than 1 and [full_col_err] if the 
    column is already full. *)
val insert: int -> int -> t -> t

(** [gravity t] Is the state which contains all of the pieces in [t] but whose
    placements are corrected as they would be by gravity (shifted down) *)
val gravity: t -> t

(** [tick_turn t] advances to the next color's turn by switching colors.  *)
val tick_turn: t -> t

(** [undo t] returns the last state before an alteration such as an insertion 
    or a rotation. 
    Raises: [undo_err] failure if undo is called on a new state*)
val undo: t-> t

(** [new_state (c1, c2) row col] creates a new state with an empty board of size 
    [row] by [col] where the players have colors [c1] and [c2] and the starting 
    player has color [c1]. 
    Requires: [row] and [col] are larger than 4. *)
val new_state: (color * color) -> (int * int) -> bool -> t

(** [score st] is the score [(s1, s2)] of the game in its current state [st], 
    where [s1] is player 1's score and [s2] is player 2's score. 
    Score is calculated by taking the total number of horizontal, vertical, and
    diagonal lines with pieces that add up to exactly 10, then multiplying by 
    10. Then, if a player has connected 4 consecutive chips, that player's score
    is incremented by 20. *)
val score: t -> (int * int)

(** [rotate rep st] is the state that contains the board of [st] rotated [rep]
    times. 
    Requires: [rep] > 0. *)
val rotate: int -> t -> t

(** [style_of_color c] is the ANSITerminal style represented by color [c]. *)
val style_of_color: color -> ANSITerminal.style

(** [print st] pretty-prints a visual representation of board in state [st] onto
    the command line. *)
val print: t -> unit

(** [print_col_nums st] prints the number of each column below it for the board 
    in state [st]*)
val print_col_nums: t -> unit

(** [print_win st win_color] is the same as [print] but emphasizes chips of 
    color [win_color] that have formed the winning line. *)
val print_win: t -> color -> unit