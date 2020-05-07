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

type chain = |Exists | Blocked | DNE


let rec score_pattern piece assoc1 assoc2 n pattern nog= 
  if n > 0 then 
    match pattern with
    | (1,0) -> if (mem (fst piece + (n - 1), snd piece) assoc1) = false 
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (-1,0) ->if ( mem (fst piece - (n - 1), snd piece) assoc1) = false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (0,1) -> if (mem (fst piece , snd piece + (n - 1)) assoc1) = false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (0,-1) -> if (mem (fst piece , snd piece - (n - 1)) assoc1) = false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (1,1) -> if(mem (fst piece + (n - 1), snd piece + (n - 1)) assoc1) = false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (1,-1) -> if(mem (fst piece + (n - 1), snd piece - (n - 1)) assoc1)= false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (-1,1) -> if(mem (fst piece - (n - 1), snd piece + (n - 1)) assoc1)= false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog
    | (-1,-1) -> if(mem (fst piece - (n - 1), snd piece - (n - 1)) assoc1)=false
      then DNE else score_pattern piece assoc1 assoc2 (n - 1) pattern nog                                     
    | _ -> failwith "no pattern"
  else 
    match pattern with
    | (1,0) -> if (mem (fst piece + nog, snd piece) assoc2) = false 
      then Exists else Blocked
    | (-1,0) ->if ( mem (fst piece - nog, snd piece) assoc2) = false
      then Exists else Blocked
    | (0,1) -> if (mem (fst piece , snd piece + nog) assoc2) = false
      then Exists else Blocked
    | (0,-1) -> if (mem (fst piece , snd piece - nog) assoc2) = false
      then Exists else Blocked
    | (1,1) -> if(mem (fst piece + nog, snd piece + nog) assoc2) = false
      then Exists else Blocked
    | (1,-1) -> if(mem (fst piece + nog, snd piece - nog) assoc2)= false
      then Exists else Blocked
    | (-1,1) -> if(mem (fst piece - nog, snd piece + nog) assoc2)= false
      then Exists else Blocked  
    | (-1,-1) -> if(mem (fst piece - nog, snd piece - nog) assoc2)=false
      then Exists else Blocked                                       
    | _ -> failwith "no pattern"


let score_horizontal a assoc1 assoc2 bool= 
  let four =
    match (score_pattern a assoc1 assoc2 4 (1,0) 4) with 
    | DNE -> 0
    | Exists ->  100000
    | Blocked -> 100000
  in
  let four' =
    match (score_pattern a assoc1 assoc2 4 (-1,0) 4) with 
    | DNE -> 0
    | Exists ->  100000
    | Blocked -> 100000
  in
  let three =
    match (score_pattern a assoc1 assoc2 3 (1,0) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let three' =
    match (score_pattern a assoc1 assoc2 3 (-1,0) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked ->  0
  in
  let two =
    match (score_pattern a assoc1 assoc2 2 (1,0) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  let two' =
    match (score_pattern a assoc1 assoc2 2 (-1,0) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  four + four' + three + three' + two + two' 

let score_vertical a assoc1 assoc2 bool= 
  let four =
    match (score_pattern a assoc1 assoc2 4 (0,1) 4) with 
    | DNE -> 0
    | Exists -> 100000
    | Blocked -> 100000
  in
  let four' =
    match (score_pattern a assoc1 assoc2 4 (0,-1) 4) with 
    | DNE -> 0
    | Exists ->100000
    | Blocked -> 100000
  in
  let three =
    match (score_pattern a assoc1 assoc2 3 (0,1) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let three' =
    match (score_pattern a assoc1 assoc2 3 (0,-1) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let two =
    match (score_pattern a assoc1 assoc2 2 (0,1) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  let two' =
    match (score_pattern a assoc1 assoc2 2 (0,-1) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  four + four' + three + three' + two + two'



let score_diagonal a assoc1 assoc2 bool= 
  let four =
    match (score_pattern a assoc1 assoc2 4 (1,1) 4) with 
    | DNE -> 0
    | Exists ->  100000
    | Blocked ->  100000
  in
  let four1 =
    match (score_pattern a assoc1 assoc2 4 (1,-1) 4) with 
    | DNE -> 0
    | Exists ->  100000
    | Blocked ->  100000
  in
  let four2 =
    match (score_pattern a assoc1 assoc2 4 (-1,1) 4) with 
    | DNE -> 0
    | Exists ->  100000
    | Blocked ->  100000
  in
  let four3 =
    match (score_pattern a assoc1 assoc2 4 (-1,-1) 4) with 
    | DNE -> 0
    | Exists ->  100000
    | Blocked ->  100000
  in
  let three =
    match (score_pattern a assoc1 assoc2 3 (1,1) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let three1 =
    match (score_pattern a assoc1 assoc2 3 (1,-1) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let three2 =
    match (score_pattern a assoc1 assoc2 3 (-1,1) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let three3 =
    match (score_pattern a assoc1 assoc2 3 (-1,-1) 3) with 
    | DNE -> 0
    | Exists -> if bool then 2000 else 150
    | Blocked -> 0
  in
  let two =
    match (score_pattern a assoc1 assoc2 2 (1,1) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  let two1 =
    match (score_pattern a assoc1 assoc2 2 (1,-1) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  let two2 =
    match (score_pattern a assoc1 assoc2 2 (-1,1) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  let two3 =
    match (score_pattern a assoc1 assoc2 2 (-1,-1) 2) with 
    | DNE -> 0
    | Exists -> 30
    | Blocked -> 0
  in
  four + four1 + four2 +four3 + three +three1 
  +three2 +three3 +two +two1 +two2 +two3

(** [ai_score st] gives a numeric score to the favorability of the board [st]*)
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



let rec test_list lst = 
  match lst with
  |(a,b)::c -> " "^ string_of_int a ^ ":" ^ string_of_int (ai_score b)^ test_list c
  | [] -> ""


(** How AI hard will work: takes in a board state and makes a new temporary 
    board for all possible insertions/rotations. Then a board score function 
    will be called that gives each board after the move a score. Finally, the 
    AI chooses the board which has the highest score to use.*)
let hard_response st = 
  let cols = snd (get_dimensions st) in
  let board_list = [] in
  let rotate_1 = st |> rotate 1 |> gravity in
  let rotate_2 = st |> rotate 2 |> gravity in 
  let rotate_3 = st |> rotate 3 |> gravity in 
  let board_list = (-3,rotate_3)::(-2,rotate_2)::
                   (-1,rotate_1)::board_list in 
  let rec insert_maker cols board_list = 
    if cols < 0 then board_list else
      try (insert_maker (cols-1) ((cols, st |> insert cols 0)::board_list)) with
      |Failure a -> insert_maker (cols-1) board_list
  in
  let board_list = insert_maker (cols-1) board_list in
  let rec best_board board_list (x,y)= 
    match board_list with
    |(a,b)::c -> if ai_score b > ai_score y then( best_board c (a,b)) else 
        best_board c (x,y)
    |[] -> (x,y)
  in 
  print_string (test_list ((-4,try (st |> insert (cols/2) 0) with | Failure a -> st)::board_list));
  let chosen = best_board board_list (-4,try (st |> insert 0 0) with | Failure a -> st) in 
  match chosen with
  | (a,_) -> if a= -4 then Insert (cols/2, 0) else if a = -1 then Rotate 1 else if a = -2 
    then Rotate 2 else if a = -3 then Rotate 3 else Insert (a, 0)



let get_response d st = 
  match d with 
  | Easy -> easy_response st
  | Hard -> hard_response st

