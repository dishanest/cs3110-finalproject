
open List

type color = Red | Blue

type cell = {
  color:color;
  value:int;
}

type board = cell option list list

exception Error

type t = {
  board: board;
  player: color;
  score: int;
  col: int;
  row: int;
}

let get_cell_color c = 
  c.color

let get_cell_value c = 
  c.value

let get_state_color st = 
  if st.player = Red then "RED"
  else "BLUE"

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

(** [make_assoc color board col] is an association list from an int r to an int c where 
    r is the row and c is the column of all cells of color [color] in [t]
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

let rec check_win t n = 
  let board = t.board in 
  let color = t.player in 
  let assoc = make_assoc color board 0 in 
  let rec check ass = 
    match ass with 
    |a::b -> if check_diagonal a assoc n|| check_vertical a assoc n
                || check_horizontal a assoc n then true else check b
    |[] -> false
  in check assoc

(** [new_column len] is a string of empty cells of length [len]
    Requires: [len is larger than 0]*)
let rec new_column len =
  if len = 0 then [] else (None)::(new_column (len-1))

(** [new_board col row] creates a new board of size int [row] by int [col]
    Requires: [col] and [row] larger than 0*)
let rec new_board row col = 
  if row = 0 then []  else (new_column col)::(new_board (row-1) col)

let new_state c row col = 
  {player = c; score = 0; board = (new_board col row); row = row; col = col}

(** [push color v lst] is the list [lst] with a cell of color [color] and
    value [v]
    Raises: invalid col failure if the column is full*)
let rec push color v lst = 
  let cell = { color = color ; value = v} in
  match lst with
  |None::(Some a)::x -> 
    (Some cell)::(Some a)::x
  |None::[] -> (Some cell)::[]
  |Some _::_ -> failwith "invalid col"
  |None::x -> None :: push color v x
  |[] -> failwith "invalid col"

let rec insert col v st = 
  let board = st.board in
  let rec add col board = 
    match board with 
    |a::b -> if col = 0 then (push st.player v a)::b else 
        a::add (col-1) b
    | [] -> failwith "invalid col" in 
  { st with board = add col board }

let tick_turn t = { t with player = if t.player = Red then Blue else Red }

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
  if rep mod 4 = 1 then { st with board = rot_rec board [] } else 
  if rep mod 4 = 2 then { st with board = map rev board |> rev } else
  if rep mod 4 = 3 then { st with board = [] |> rot_rec board |> map rev |> rev }
  else st

(** [print_cell] prints the cell according to its color and value. *)
let print_cell c = 
  match c with 
  | None -> print_string "( )"
  | Some cell -> 
    let style = match get_cell_color cell with 
      | Red -> ANSITerminal.red
      | Blue -> ANSITerminal.blue in
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