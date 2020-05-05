open List

let invalid_col_err = Failure "Invalid column. "
let full_col_err = Failure "Column is full. "

type color = 
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan

type win = 
  | NWin
  | Win of color

type cell = {
  color: color;
  value: int;
}

type board = cell option list list

type t = {
  (*  num_cols & num_rows are static - represent dimensions of the board *)
  num_rows: int;
  num_cols: int;
  (* player_colors is static - colors that the players chose at the beginning *)
  player_colors: color * color;
  board: board;
  (* color of the player whose turn it currently is. *)
  current_player: color;
  score: int;
  prev_state: t option;
} 

let get_cell_color c = c.color
let get_cell_value c = c.value

let get_p1_color st = 
  let (c1, _) = st.player_colors in c1

let get_p2_color st = 
  let (_, c2) = st.player_colors in c2

let get_current_color st = st.current_player

let string_of_color c = 
  match c with
  | Red -> "Red"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | Blue -> "Blue"
  | Magenta -> "Magenta"
  | Cyan -> "Cyan"

(**[list_assoc color lst row col] is an association list from an int r to an int 
   c where c is [col] and r is the row of all cells of color [color] in [t]
   Requires: row begins at 0*)
let rec list_assoc color lst row col = 
  match lst with
  | Some a::b -> if a.color = color 
    then (row,col)::(list_assoc color b (row+1) col)
    else list_assoc color b (row+1) col
  | None::b -> list_assoc color b (row+1) col
  | [] -> []

(** [make_assoc color board col] is an association list from an int r to an int
    c where r is the row and c is the column of all cells of color [color] in [t]
    Requires: col begins at 0*)
let rec make_assoc color board col= 
  match board with 
  | [] -> []
  | x :: y -> (list_assoc color x 0 col) @ (make_assoc color y (col+1))

(** [check_pattern piece assoc n pattern] is the bool of whether there is a 
    group of int [n] pieces of the same color in a pattern [pattern] in 
    the list [assoc] where one of the pieces in the group is [piece]
    Requires: n is *)
let rec check_pattern piece assoc n pattern = 
  match pattern with
  | (1,0) when n>0 ->  mem (fst piece +(n-1), snd piece) assoc && 
                       check_pattern piece assoc (n-1) pattern
  | (0,1) when n>0 ->  mem (fst piece , snd piece +(n-1)) assoc && 
                       check_pattern piece assoc (n-1) pattern
  | (1,1) when n>0 ->  mem (fst piece +(n-1), snd piece +(n-1)) assoc && 
                       check_pattern piece assoc (n-1) pattern  
  | (1,-1) when n>0 ->  mem (fst piece +(n-1), snd piece -(n-1)) assoc && 
                        check_pattern piece assoc (n-1) pattern   
  | (-1,1) when n>0 ->  mem (fst piece -(n-1), snd piece +(n-1)) assoc && 
                        check_pattern piece assoc (n-1) pattern  
  | (-1,-1) when n>0 ->  mem (fst piece -(n-1), snd piece -(n-1)) assoc && 
                         check_pattern piece assoc (n-1) pattern                                        
  |_ -> true

(** [check_horizontal piece assoc n] is the bool of whether there is a horizontal 
    of int [n] pieces in assoc that contains [piece]*)
let check_horizontal piece assoc n = 
  (* if mem (fst piece +1, snd piece) assoc && 
      mem (fst piece +2, snd piece) assoc && 
      mem (fst piece +3, snd piece) assoc then true else false
  *)
  check_pattern piece assoc n (1,0)

(** [check_vertial piece assoc n] is the bool of whether there is a vertical 
    of int [n] pieces in assoc that contains [piece]*)
let check_vertical piece assoc n = 
  (* if mem (fst piece , snd piece +1) assoc && 
      mem (fst piece , snd piece +2) assoc && 
      mem (fst piece , snd piece +3) assoc then true else false
  *)
  check_pattern piece assoc n (0,1)

(** [check_diagonal piece assoc n] is the bool of whether there is a diagonal 
    of int [n] pieces in assoc that contains [piece]*)
let check_diagonal piece assoc n = 
  (*if (mem (fst piece +1, snd piece +1) assoc && 
      mem (fst piece +2, snd piece +2) assoc && 
      mem (fst piece +3, snd piece +3) assoc)|| 
     (mem (fst piece +1, snd piece -1) assoc && 
    @@ -72,16 +96,21 @@ let rec check_diagonal piece assoc =
     (mem (fst piece -1, snd piece -1) assoc && 
      mem (fst piece -2, snd piece -2) assoc && 
      mem (fst piece -3, snd piece -3) assoc )
    then true else false
  *)
  check_pattern piece assoc n (1,1) ||
  check_pattern piece assoc n (1,-1) ||
  check_pattern piece assoc n (-1,1) ||
  check_pattern piece assoc n (-1,-1)

(** [win_color color] returns the type of win that matches to color [color]*)
let win_color color = Win color

let rec check_win t n = 
  let board = t.board in 
  let (color, color') = t.player_colors in
  let assoc = make_assoc color board 0 in 
  let assoc' = make_assoc color' board 0 in
  let rec check ass = 
    match ass with 
    | a::b -> 
      if check_diagonal a ass n
      || check_vertical a ass n
      || check_horizontal a ass n 
      then true else check b
    | [] -> false in
  if (check assoc) then 
    win_color color
  else if (check assoc') then 
    win_color color'
  else NWin

(** [new_column len] is a string of empty cells of length [len]
    Requires: [len is larger than 0]*)
let rec new_column len =
  if len = 0 then [] else (None)::(new_column (len-1))

(** [new_board col row] creates a new board of size int [row] by int [col]
    Requires: [col] and [row] larger than 0*)
let rec new_board row col = 
  if row = 0 then [] else (new_column col)::(new_board (row-1) col)

let new_state (c1, c2) row col = {
  num_rows = row; 
  num_cols = col;
  player_colors = (c1, c2);
  board = (new_board col row); 
  current_player = c1; 
  score = 0; 
  prev_state = None
}

(** [push color v lst] is the list [lst] with a cell of color [color] and
    value [v]
    Raises: invalid col failure if the column is full*)
let rec push color v lst = 
  let cell = { color = color ; value = v} in
  match lst with
  | None :: (Some a) :: x -> (Some cell) :: (Some a) :: x
  | None :: [] -> (Some cell)::[]
  | Some _ :: _ -> raise full_col_err
  | None :: x -> None :: push color v x
  | [] -> raise full_col_err

let rec insert col v st = 
  let board = st.board in
  let rec add col board = 
    match board with 
    | a :: b -> if col = 0 then (push st.current_player v a) :: b else 
        a :: add (col - 1) b
    | [] -> raise invalid_col_err in { 
    st with 
    board = add col board; 
    prev_state = Some st
  }

(** [skyfall lst] is list where all None elements are moved to the front and 
    all other options are moved to the bottem in the same order*)
let rec skyfall lst acc= 
  match lst with
  | (Some a)::x -> skyfall x ((Some a)::acc)
  | None::x -> None:: skyfall x acc
  | [] -> List.rev acc

let rec gravity st = 
  let board = st.board in 
  let rec gravitation board =
    match board with
    | a::b -> (skyfall a []):: gravitation b
    | [] -> [] in { 
    st with 
    board = gravitation board
  }

let tick_turn t = { 
  t with 
  current_player = let (c1, c2) = t.player_colors in
    if t.current_player = c1 then c2 else c1
}

let score st = 
  failwith "Unimplemented"

(** [rotate st rep] is the state with a board rotated c-clockwise [rep] times. 
    Note: Does not implement chips fallins down. Han just needs this to 
    properly write the [print] function. *)
let rotate rep st = 
  let board = st.board in 
  let rec rot_rec (b:board) (acc:board) = 
    if flatten b = [] then acc else 
      match b with 
      | [] -> acc
      | board -> rot_rec (map tl board) ((board |> map hd) :: acc) in 
  if rep mod 4 = 1 then { 
    st with 
    board = rot_rec board [];
    prev_state = Some st
  } else if rep mod 4 = 2 then {
    st with 
    board = map rev board |> rev;
    prev_state = Some st
  } else if rep mod 4 = 3 then { 
    st with 
    board = [] |> rot_rec board |> map rev |> rev;
    prev_state = Some st 
  } else st

let undo st = 
  match st.prev_state with
  | Some t -> t
  | None -> failwith "Could not undo further"

let style_of_color c = 
  match c with 
  | Red -> ANSITerminal.red
  | Green -> ANSITerminal.green
  | Yellow -> ANSITerminal.yellow
  | Blue -> ANSITerminal.blue
  | Magenta -> ANSITerminal.magenta
  | Cyan -> ANSITerminal.cyan

(** [print_cell] prints the cell according to its color and value. *)
let print_cell c = 
  match c with 
  | None -> print_string "( )"
  | Some cell -> 
    let style = cell |> get_cell_color |> style_of_color in
    "(" ^ (cell |> get_cell_value |> string_of_int) ^ ")" 
    |> ANSITerminal.(print_string [style])

let print st = 
  let board = st |> rotate 3 |> fun x -> x.board |> map rev in
  print_endline "";
  let rec loop st =
    match st with 
    | [] -> ()
    | h :: t -> iter print_cell h; print_string "\n"; loop t
  in loop board 