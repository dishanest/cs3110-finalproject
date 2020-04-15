(**  *)

type color = Red | Blue

type cell = 
  {
    color:color;
    value:int;
  }

type board = cell option list list

type t = 
  {
    board: board;
    player: color;
    score: int;
    col: int;
    row: int;
  }

let print b = 
  failwith "Unimplemented"

let get_cell_color c = 
  c.color

let get_cell_value c = 
  c.value

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
  |x::y -> (list_assoc color x 0 col) @ (make_assoc color y (col+1))
  |[] -> []

(** [check_horizontal piece assoc] is the bool of whether there is a horizontal 
    of 4 pieces in assoc that contains [piece]*)
let rec check_horizontal piece assoc = 
  if List.mem (fst piece +1, snd piece) assoc && 
     List.mem (fst piece +2, snd piece) assoc && 
     List.mem (fst piece +3, snd piece) assoc then true else false

(** [check_vertial piece assoc] is the bool of whether there is a vertical 
    of 4 pieces in assoc that contains [piece]*)
let rec check_vertical piece assoc = 
  if List.mem (fst piece , snd piece +1) assoc && 
     List.mem (fst piece , snd piece +2) assoc && 
     List.mem (fst piece , snd piece +3) assoc then true else false

(** [check_diagonal piece assoc] is the bool of whether there is a diagonal 
    of 4 pieces in assoc that contains [piece]*)
let rec check_diagonal piece assoc = 
  if (List.mem (fst piece +1, snd piece +1) assoc && 
      List.mem (fst piece +2, snd piece +2) assoc && 
      List.mem (fst piece +3, snd piece +3) assoc)|| 
     (List.mem (fst piece +1, snd piece -1) assoc && 
      List.mem (fst piece +2, snd piece -2) assoc && 
      List.mem (fst piece +3, snd piece -3) assoc)||
     (List.mem (fst piece -1, snd piece +1) assoc && 
      List.mem (fst piece -2, snd piece +2) assoc && 
      List.mem (fst piece -3, snd piece +3) assoc )|| 
     (List.mem (fst piece -1, snd piece -1) assoc && 
      List.mem (fst piece -2, snd piece -2) assoc && 
      List.mem (fst piece -3, snd piece -3) assoc )
  then true else false

let rec check_win t = 
  let board = t.board in 
  let color = t.player in 
  let assoc = make_assoc color board 0 in 
  let rec check ass = 
    match ass with 
    |a::b -> if check_diagonal a assoc || check_vertical a assoc 
                || check_horizontal a assoc then true else check b
    |[] -> false
  in
  check assoc

(** [new_column len] is a string of empty cells of length [len]
    Requires: [len is larger than 0]*)
let rec new_column len =
  if len = 0 then [] else (None)::(new_column (len-1))

(** [new_board col row] creates a new board of size int [row] by int [col]
    Requires: [col] and [row] larger than 0*)
let rec new_board row col = 
  if row = 0 then []  else (new_column col)::(new_board (row-1) col)

let new_state c row col= 
  {player = c; score = 0; board = (new_board row col); row = row; col = col}

(** [push color v lst] is the list [lst] with a cell of color [color] and
    value [v]
    Raises invalid col failure if the column is full*)
let rec push color v lst = 
  let cell = { color = color ; value = v} in
  match lst with
  |None::(Some a)::x -> 
    (Some cell)::(Some a)::x
  |None::[] -> (Some cell)::[]
  |Some _::_ -> failwith "invalid col"
  |None::x -> None :: push color v x
  |[] -> failwith "invalid col"

let rec insert col v t = 
  let board = t.board in

  let rec add col board = 
    match board with 
    |a::b -> if col = 0 then (push t.player v a)::b else 
        a::add (col-1) b
    | [] -> failwith "invalid col" in 
  { t with board = add col board }

let tick_turn t = 
  if t.player = Red then 
    {
      board =  t.board;
      player = Blue;
      score =  t.score;
      col = t.col;
      row = t.row;
    }
  else 
    {
      board =  t.board;
      player = Red;
      score =  t.score;
      col = t.col;
      row = t.row;
    }

let score board = 
  failwith "Unimplemented"