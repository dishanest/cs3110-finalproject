open List

let invalid_dimensions_err = Failure "Invalid dimensions. "
let invalid_col_err = Failure "Invalid column. "
let full_col_err = Failure "Column is full. "
let undo_err = Failure "Cannot undo further. "
let insert_value_err = Failure "Cannot insert piece of that value. May have already been played. "

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
  (*  dimensions are static - (rows, columns) *)
  dimensions: int * int;
  (* player_colors is static - colors that the players chose at the beginning *)
  player_colors: color * color;
  board: board;
  (* color of the player whose turn it currently is. *)
  current_player: color;
  score: int;
  prev_state: t option;
  (* random is true if gamemode is random *)
  random: bool;
} 

let get_cell_color c = c.color

let get_cell_value c = c.value

let get_p1_color st = 
  let (c1, _) = st.player_colors in c1

let get_p2_color st = 
  let (_, c2) = st.player_colors in c2

let get_dimensions st = st.dimensions

let get_board st = st.board

let get_current_color st = st.current_player

let get_gamemode st = st.random 

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

let rec make_assoc color board col = 
  match board with 
  | [] -> []
  | x :: y -> (list_assoc color x 0 col) @ (make_assoc color y (col+1))

let rec check_full st = 
  let board = st.board in
  let red_list = make_assoc (get_p1_color st) board 0 in
  let blue_list = make_assoc (get_p2_color st) board 0 in
  let total_list = red_list @ blue_list in 
  let fill_size = List.length total_list in 
  let max_size = let (c, r) = st.dimensions in c * r in 
  if fill_size < max_size then false else true

(** *)
let rec switch_row_color lst st= 
  match lst with
  | [] -> []
  | None :: b -> None :: switch_row_color b st
  | Some a ::b -> 
    let col =  if a.color = get_p1_color st then get_p2_color st 
      else get_p1_color st in 
    Some { a with color = col} :: switch_row_color b st

let switch_colors st = 
  let board = st.board in 
  let rec iter board = 
    match board with 
    | a::b -> (switch_row_color a st):: iter b
    | [] -> [] in
  let new_board = iter board in { 
    st with 
    board = new_board; 
    prev_state = Some st
  }

(** [check_pattern piece assoc n pattern] is the bool of whether there is a 
    group of int [n] pieces of the same color in a pattern [pattern] in 
    the list [assoc] where one of the pieces in the group is [piece]
    Requires: n is > 0. *)
let rec check_pattern piece assoc n pattern = 
  if n > 0 then 
    match pattern with
    | (1,0) -> mem (fst piece + (n - 1), snd piece) assoc && 
               check_pattern piece assoc (n - 1) pattern
    | (0,1) -> mem (fst piece , snd piece + (n - 1)) assoc && 
               check_pattern piece assoc (n - 1) pattern
    | (1,1) -> mem (fst piece + (n - 1), snd piece + (n - 1)) assoc && 
               check_pattern piece assoc (n - 1) pattern  
    | (1,-1) -> mem (fst piece + (n - 1), snd piece - (n - 1)) assoc && 
                check_pattern piece assoc (n - 1) pattern 
    | (-1,1) -> mem (fst piece - (n - 1), snd piece + (n - 1)) assoc && 
                check_pattern piece assoc (n - 1) pattern  
    | (-1,-1) -> mem (fst piece - (n - 1), snd piece - (n - 1)) assoc && 
                 check_pattern piece assoc (n - 1) pattern                                        
    | _ -> true
  else true

(** [check_vertical piece assoc n] is the bool of whether there is a horizontal 
    of int [n] pieces in assoc that contains [piece]*)
let check_vertical piece assoc n = 
  (* if mem (fst piece +1, snd piece) assoc && 
      mem (fst piece +2, snd piece) assoc && 
      mem (fst piece +3, snd piece) assoc then true else false
  *)
  check_pattern piece assoc n (0,1)

(** [check_horizontal piece assoc n] is the bool of whether there is a vertical 
    of int [n] pieces in assoc that contains [piece]*)
let check_horizontal piece assoc n = 
  (* if mem (fst piece , snd piece +1) assoc && 
      mem (fst piece , snd piece +2) assoc && 
      mem (fst piece , snd piece +3) assoc then true else false
  *)
  check_pattern piece assoc n (1,0)

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
    | a :: b -> 
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

let new_state (c1, c2) (rows, cols) random = 
  if rows >= 4 && cols >= 4 then {
    dimensions = (rows, cols);
    player_colors = (c1, c2);
    board = (new_board cols rows); 
    current_player = c1; 
    score = 0; 
    prev_state = None;
    random = random;
  } else raise invalid_dimensions_err

(** [push color v lst] is the list [lst] with a cell of color [color] and
    value [v]
    Raises: invalid col failure if the column is full*)
let rec push color v lst = 
  let cell = { color = color ; value = v } in
  match lst with
  | None :: (Some a) :: x -> (Some cell) :: (Some a) :: x
  | None :: [] -> (Some cell)::[]
  | Some _ :: _ -> raise full_col_err
  | None :: x -> None :: push color v x
  | [] -> raise full_col_err

(** [check_val_used c v st] is true if the player of color [c] has already 
    inserted a chip of int [v] in state [st]. *)
let check_val_used c v st = 
  if v = 0 then false 
  else
    let rec check_board b acc = 
      match b with 
      | [] -> acc 
      | h :: t -> 
        let get_value cell_opt = 
          match cell_opt with 
          | Some { color; value } -> 
            if color = c then value else 0
          | None -> 0 in
        let values = map get_value h in
        check_board t (acc || mem v values) in 
    check_board st.board false

let rec get_valid_int c st = 
  let v = Random.int 10 in 
  print_int v; print_string "\n";
  if (check_val_used c v st) then get_valid_int c st 
  else v

let rec insert col v st = 
  if v < 0 || v > 9 then raise insert_value_err 
  else if v <> 0 && check_val_used st.current_player v st 
  then raise insert_value_err 
  else 
    let board = st.board in
    let rec add col board = 
      match board with 
      | a :: b -> if col = 0 then (push st.current_player v a) :: b 
        else a :: add (col - 1) b
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
  | None -> raise undo_err

let style_of_color c = 
  match c with 
  | Red -> ANSITerminal.red
  | Green -> ANSITerminal.green
  | Yellow -> ANSITerminal.yellow
  | Blue -> ANSITerminal.blue
  | Magenta -> ANSITerminal.magenta
  | Cyan -> ANSITerminal.cyan

(** [print_cell] prints the cell according to its color and value. *)
let print_cell styles c = 
  match c with 
  | None -> ANSITerminal.(print_string styles "( )") 
  | Some cell -> 
    let style = cell |> get_cell_color |> style_of_color in
    "(" ^ (cell |> get_cell_value |> string_of_int) ^ ")" 
    |> ANSITerminal.(print_string (styles @ [style]))


let print_col_nums st =
  let cols = snd (st.dimensions) in 
  let rec loop cols acc= 
    if acc > cols -1 then (print_string "";) else
      (print_string (" "^ string_of_int acc ^ " ");
       loop cols (acc+1); )
  in loop cols 0


let print st = 
  let board = st |> rotate 3 |> fun x -> x.board |> map rev in
  print_endline "";
  let rec loop b =
    match b with 
    | [] -> ()
    | h :: t -> iter (print_cell []) h ; print_string "\n"; loop t
  in loop board

(** [winning_pieces st c] is the association list of the coordinates of the 
    winning pieces in [st] of color [c]. 
    Requires: [check_win st] is [true].  *)
let winning_pieces st c = 
  let assoc = make_assoc c st.board 0 in 
  let pcs_of_pat (r, c) (p1, p2) ass = 
    if check_pattern (r, c) ass 4 (p1, p2) then [
      (r, c); 
      (r + p1, c + p2); 
      (r + 2 * p1, c + 2 * p2); 
      (r + 3 * p1, c + 3 * p2);
    ] else [] in 
  let rec check_assoc ass acc = 
    match ass with 
    | [] -> acc 
    | (r, c) :: t -> 
      let pcs = pcs_of_pat (r, c) (0, 1) ass 
                @ pcs_of_pat (r, c) (1, 0) ass 
                @ pcs_of_pat (r, c) (1, 1) ass 
                @ pcs_of_pat (r, c) (1, -1) ass 
                @ pcs_of_pat (r, c) (-1, 1) ass 
                @ pcs_of_pat (r, c) (-1, -1) ass in
      if pcs <> [] then pcs else check_assoc t acc
  in check_assoc assoc []

let print_win st win_color = 
  let pcs = winning_pieces st win_color in 
  let board = st |> rotate 3 |> fun x -> x.board |> map rev in 
  print_endline "";
  let rec print_row row (r, c) = 
    match row with 
    | [] -> ()
    | h :: t -> if mem (r, c) pcs 
      then (print_cell [Bold; Background White] h; print_row t (r, c + 1))
      else (print_cell [] h; print_row t (r, c + 1)) in
  let rec print_board b r = 
    match b with 
    | [] -> () 
    | h :: t -> print_row h (r, 0); print_string "\n"; print_board t (r + 1)
  in print_board board 0

(** [score_cell_helper y lst] returns the [y]th value in [lst] *)
let rec score_cell_helper y lst = 
  match lst with 
  | [] ->  0
  | h::t -> if y = 0 then
      match h with 
      | None -> 0 
      | Some v -> get_cell_value v
    else score_cell_helper (y-1) t

(**[score_cell_value x y board] is the int value of the piece in location 
   ([x],[y]) in [board] *)
let rec score_cell_value x y (board: cell option List.t List.t) : int =
  match board with 
  | [] -> 0
  | h::t -> 
    if x = 0 then score_cell_helper y h
    else score_cell_value (x-1) y t

(**[calculate_score piece board n pattern] is the sum of [n] pieces on the [board]
   starting at [piece] in [pattern] *)
let rec calculate_score piece board n pattern direction = 
  match pattern with 
  | "vertical" -> 
    if n = 0 then 0 
    else 
      let score = score_cell_value (snd piece) (fst piece) board in 
      score + calculate_score (fst piece, snd piece+1) board (n-1) pattern direction
  | "horizontal" -> 
    if n = 0 then 0 
    else 
      let score = score_cell_value (snd piece) (fst piece) board in 
      score + calculate_score (fst piece+1, snd piece) board (n-1) pattern direction
  | "diagonal" -> 
    if n = 0 then 0 
    else
      let score = score_cell_value (snd piece) (fst piece) board in 
      score + calculate_score (fst piece + fst direction, snd piece + snd direction) board (n-1) pattern direction
  | _ -> failwith "invalid pattern"

let rec update_assoc ass cons pattern (direction: int * int) piece = 
  (* print_string "piece: "; print_int (fst piece); print_int (snd piece); print_string "\n"; *)
  match pattern with 
  | "vertical" -> begin
      match ass with 
      | [] -> [] 
      | h::t -> if (fst h) = cons then 
          update_assoc t cons pattern direction piece
        else h::(update_assoc t cons pattern direction piece)
    end
  | "horizontal" -> begin
      match ass with 
      | [] -> []
      | h::t -> if (snd h) = cons then 
          update_assoc t cons pattern direction piece
        else h::(update_assoc t cons pattern direction piece)
    end
  | "diagonal" -> begin 
      (* print_string "direction: "; print_int (fst direction); print_int (snd direction); print_string "\n"; *)
      match ass with 
      | [] -> []
      | h::t -> if fst h = fst piece && snd h = snd piece then 
          update_assoc t cons pattern direction (fst piece - fst direction, snd piece - snd direction)
        else h::(update_assoc t cons pattern direction piece)
    end
  | _ -> failwith "invalid pattern"

let rec print_assoc assoc = 
  match assoc with 
  | [] -> print_string"";
  | h::t -> print_int (fst h); print_string " "; print_int (snd h); print_string "\n";
    print_assoc t

(* let rec diagonal_direction_helper assoc piece= 
   print_int (fst piece); print_int(snd piece);print_string "\n";
   match assoc with 
   | [] -> 0 
   | h::t -> if (fst h=fst piece && snd h=snd piece) then 
      1 + diagonal_direction_helper t ((fst h)+1, (snd h)-1)
    else 
      diagonal_direction_helper t piece

   let diagonal_direction assoc n piece = 
   print_string "n: "; print_int n; print_string "\n";
   print_string "assoc: "; print_assoc assoc;
   if (diagonal_direction_helper assoc piece) = n 
   then "right"
   else "left" *)

let diagonal_score_helper piece assoc n = 
  if check_pattern piece assoc n (1,1) then (1,1)
  else if check_pattern piece assoc n (1,-1) then (1,-1)
  else if check_pattern piece assoc n (-1,1) then (-1,1)
  else if check_pattern piece assoc n (-1,-1) then (-1,-1)
  else (0,0)

let score t = 
  let board = t |> rotate 3 |> fun x -> x.board |> map rev in
  let (color, color') = t.player_colors in
  let assoc = make_assoc color board 0 in 
  let assoc' = make_assoc color' board 0 in 
  let rec vertical_score ass n : int = 
    match ass with 
    | a::b -> 
      if check_vertical a ass n
      then 
        let x = calculate_score a board n "vertical" (0,0) in 
        let new_ass = update_assoc ass (fst a) "vertical" (0,0) (0,0) in
        (* print_int n; print_string "\n"; *)
        if x = 10 
        then 10 + vertical_score new_ass (fst t.dimensions)
        else 
          vertical_score new_ass (fst t.dimensions)
      else vertical_score ass (n-1)
    | [] -> 0 in  
  let rec horizontal_score ass n : int = 
    match ass with 
    | a::b -> 
      if check_horizontal a ass n
      then 
        let x = calculate_score a board n "horizontal" (0,0) in 
        let new_ass = update_assoc ass (snd a) "horizontal" (0,0) (0,0) in
        if x = 10 
        then 10 + horizontal_score new_ass (snd t.dimensions)
        else 
          horizontal_score new_ass (snd t.dimensions)
      else horizontal_score ass (n-1)
    | [] -> 0 in
  let rec diagonal_score ass n : int = 
    if n = 1 then 0 
    else
      match ass with 
      | a::b -> 
        let direction = diagonal_score_helper a ass n in
        if direction = (0,0) then 
          diagonal_score ass (n-1)
        else 
          let x = calculate_score a board n "diagonal" direction in 
          let new_ass = update_assoc ass n "diagonal" direction a in
          (* print_string "new assoc: "; print_assoc new_ass; print_string "\n"; *)
          (* print_int x; print_string "\n"; *)
          if x = 10 
          then 10 + diagonal_score new_ass (snd t.dimensions)
          else 
            diagonal_score new_ass (snd t.dimensions)
      | [] -> 0 in

  let v1 =  vertical_score assoc (fst t.dimensions) in 
  let h1 = horizontal_score assoc (snd t.dimensions) in 
  let v2 =  vertical_score assoc' (fst t.dimensions) in 
  let h2 = horizontal_score assoc' (snd t.dimensions) in 
  let d1 = diagonal_score assoc (fst t.dimensions) in 
  let d2 = diagonal_score assoc' (fst t.dimensions) in
  (v1 + h1 + d1, v2 + h2 + d2)