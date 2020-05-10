open Command
open State
open Random
open List

type response = command

type difficulty = 
  | Easy 
  | Hard

(** [easy_response st] is an unintelligent and random response built using 
    little contextual understanding of the board. Only returns insert. *)
let rec easy_response st : response = 
  self_init ();
  let (num_cols, _) = get_dimensions st in
  let random_col = Random.int num_cols in 
  let random_val = Random.int 10 in
  Insert (random_col, random_val)

(** [chain] is the type of a chain of pieces in a board*)
type chain = |Exists | Blocked | DNE

(** [score_pattern piece assoc1 assoc2 n pattern nog] is a chain that represents
    the position of pieces starting at position [piece] and moving in a 
    direction determined by [pattern] for a maximum length of [nog]. The chain
    returned is [Exists] if there are [nog] pieces in the [pattern] in the list 
    of piece positions [assoc1] without any piece form list [assoc2] at the end,
    the case of which returns [blocked] instead, or [DNE] if the specified
    length of pieces did not exist in the first place
    Requires: [n] is equal to [nog] when run*)
let rec score_pattern piece assoc1 assoc2 n pattern nog= 
  if n > 0 then sc_pat_pos piece assoc1 assoc2 n pattern nog
  else sc_pat_neg piece assoc1 assoc2 n pattern nog

and sc_pat_pos piece assoc1 assoc2 n pattern nog = 
  match pattern with
  | (1,0) -> if mem (fst piece + (n - 1), snd piece) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (-1,0) -> if mem (fst piece - (n - 1), snd piece) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (0,1) -> if mem (fst piece , snd piece + (n - 1)) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (0,-1) -> if mem (fst piece , snd piece - (n - 1)) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (1,1) -> if mem (fst piece + (n - 1), snd piece + (n - 1)) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (1,-1) -> if mem (fst piece + (n - 1), snd piece - (n - 1)) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (-1,1) -> if mem (fst piece - (n - 1), snd piece + (n - 1)) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE
  | (-1,-1) -> if mem (fst piece - (n - 1), snd piece - (n - 1)) assoc1
    then score_pattern piece assoc1 assoc2 (n - 1) pattern nog else DNE                                    
  | _ -> failwith "no pattern"

and sc_pat_neg piece assoc1 assoc2 n pattern nog = 
  match pattern with
  | (1,0) -> if mem (fst piece + nog, snd piece) assoc2 then Blocked else Exists
  | (-1,0) ->if mem (fst piece - nog, snd piece) assoc2 then Blocked else Exists
  | (0,1) -> if mem (fst piece, snd piece + nog) assoc2 then Blocked else Exists
  | (0,-1) -> if mem (fst piece, snd piece-nog) assoc2  then Blocked else Exists
  | (1,1) -> if mem (fst piece + nog, snd piece + nog) assoc2 then Blocked 
    else Exists
  | (1,-1) -> if mem (fst piece + nog, snd piece - nog) assoc2 then Blocked 
    else Exists
  | (-1,1) -> if mem (fst piece - nog, snd piece + nog) assoc2 then Blocked 
    else Exists 
  | (-1,-1) -> if mem (fst piece - nog, snd piece - nog) assoc2 then Blocked 
    else Exists                                       
  | _ -> failwith "no pattern"

(** [score_horizontal a assoc1 assoc2 bool] returns an int for the horizontal 
    score of a board from a board space touple [a] where [bool] is whether it is
    the non-AI player's turn, and [assoc1] is a list of board position touples 
    for current player and [assoc2] is a list of board position touples for the 
    opposing player*)
let score_horizontal a assoc1 assoc2 bool= 
  let four = map (fun x -> if x = DNE then 0 else 10000) [
      score_pattern a assoc1 assoc2 4 (1,0) 4;
      score_pattern a assoc1 assoc2 4 (-1,0) 4
    ] in
  let three = 
    map (fun x -> if x = Exists then (if bool then 2000 else 150) else 0) [
      score_pattern a assoc1 assoc2 3 (1,0) 3;
      score_pattern a assoc1 assoc2 3 (-1,0) 3
    ] in
  let two = map (fun x -> if x = Exists then 30 else 0) [
      score_pattern a assoc1 assoc2 2 (1,0) 2;
      score_pattern a assoc1 assoc2 2 (-1,0) 2
    ] in
  [two; three; four] |> flatten |> fold_left (+) 0 

(** [score_vertical a assoc1 assoc2 bool] returns an int for the vertical score 
    of a board from a board space touple [a] where [bool] is whether it is the 
    non-AI player's turn, and [assoc1] is a list of board position touples for 
    current player and [assoc2] is a list of board position touples for the 
    opposing player*)
let score_vertical a assoc1 assoc2 bool = 
  let four = map (fun x -> if x = DNE then 0 else 100000) [
      score_pattern a assoc1 assoc2 4 (0,1) 4;
      score_pattern a assoc1 assoc2 4 (0,-1) 4
    ] in
  let three = 
    map (fun x -> if x = Exists then (if bool then 2000 else 150) else 0) [
      score_pattern a assoc1 assoc2 3 (0,1) 3;
      score_pattern a assoc1 assoc2 3 (0,-1) 3
    ] in
  let two = map (fun x -> if x = Exists then 30 else 0) [
      score_pattern a assoc1 assoc2 2 (0,1) 2;
      score_pattern a assoc1 assoc2 2 (0,-1) 2
    ] in
  [two; three; four] |> flatten |> fold_left (+) 0

(** [score_diagonal a assoc1 assoc2 bool] returns an int for the diagonal score 
    of a board from a board space touple [a] where [bool] is whether it is the 
    non-AI player's turn, and [assoc1] is a list of board position touples for 
    current player and [assoc2] is a list of board position touples for the 
    opposing player*)
let score_diagonal a assoc1 assoc2 bool = 
  let four = map (fun x -> if x = DNE then 0 else 10000) [
      score_pattern a assoc1 assoc2 4 (1,1) 4;
      score_pattern a assoc1 assoc2 4 (1,-1) 4;
      score_pattern a assoc1 assoc2 4 (-1,1) 4;
      score_pattern a assoc1 assoc2 4 (-1,-1) 4
    ] in
  let three = 
    map (fun x -> if x = Exists then (if bool then 200 else 150) else 0) [
      score_pattern a assoc1 assoc2 3 (1,1) 3;
      score_pattern a assoc1 assoc2 3 (1,-1) 3;
      score_pattern a assoc1 assoc2 3 (-1,1) 3;
      score_pattern a assoc1 assoc2 3 (-1,-1) 3
    ] in
  let two = map (fun x -> if x = Exists then 30 else 0) [
      score_pattern a assoc1 assoc2 2 (1,1) 2;
      score_pattern a assoc1 assoc2 2 (1,-1) 2;
      score_pattern a assoc1 assoc2 2 (-1,1) 2;
      score_pattern a assoc1 assoc2 2 (-1,-1) 2
    ] in 
  [four; three; two] |> flatten |> fold_left (+) 0

(** [ai_score st] gives a numeric score to the favorability of the board in state [st]*)
let ai_score st = 
  let score = 0 in
  let board = get_board st in 
  let good_col = get_current_color st in
  let bad_col = if good_col = get_p1_color st then get_p2_color st 
    else get_p1_color st in
  let good_assoc = make_assoc good_col board 0 in 
  let bad_assoc = make_assoc bad_col board 0 in
  let rec calc assoc score list1 list2 bool = 
    match assoc with
    | a::b -> 
      let horizontal = score_horizontal a list1 list2 in
      let vertical = score_vertical a list1 list2 in
      let diagonal = score_diagonal a list1 list2 in
      calc b (score+horizontal bool+vertical bool+diagonal bool) list1 list2 bool
    |[] -> score
  in calc good_assoc score good_assoc bad_assoc false + length(good_assoc) - 
     calc bad_assoc score bad_assoc good_assoc true

(** [test_list lst] creates a string out of the association list [lst] for testing*)
let rec test_list lst = 
  match lst with
  |(a,b)::c -> " "^ string_of_int a ^ ":" ^ string_of_int (ai_score b)^ test_list c
  | [] -> ""

(** [best_board board_list coords] is the highest-soring board in [board_list] 
    according to [ai_score]. *)
let rec best_board board_list (x, y) = 
  match board_list with
  | (a,b)::c -> 
    if ai_score b > ai_score y then best_board c (a,b)
    else best_board c (x,y)
  | [] -> (x,y)

(** [hard_response st] returns a command based on an evaluation of all possible
    insertion and rotation commands in state [st]*)
(** How AI hard will work: takes in a board state and makes a new temporary 
    board for all possible insertions/rotations. Then a board score function 
    will be called that gives each board after the move a score. Finally, the 
    AI chooses the board which has the highest score to use.*)
let hard_response st = 
  let cols = snd (get_dimensions st) in
  let valid_int = get_valid_int (get_current_color st) st in
  let rec insert_maker cols board_list = 
    if cols < 0 then board_list 
    else try (cols,insert cols valid_int st)::board_list|> insert_maker (cols-1) 
      with Failure a -> insert_maker (cols-1) board_list in
  let board_list = insert_maker (cols-1) [
      (-3, st |> rotate 3 |> gravity);
      (-2, st |> rotate 2 |> gravity);
      (-1, st |> rotate 1 |> gravity);
    ] in
  let (a, _) = 
    (-4, try insert (cols/2) valid_int st with Failure a -> st) 
    |> best_board board_list in 
  match a with
  | -4 -> Insert (cols/2, valid_int) 
  | -1 -> Rotate 1
  | -2 -> Rotate 2 
  | -3 -> Rotate 3
  | _ -> Insert (a, valid_int)
(* print_string begin 
   (-4, try (st |> insert (cols/2) valid_int) 
   with Failure a -> st)::board_list
   |> test_list
   end; *)

let get_response d st = 
  match d with 
  | Easy -> easy_response st
  | Hard -> hard_response st

