open State
open Command
open Ai
open String

(* TODO: 
   - print column numbers below columns
   - pretty-print scores
*)

(** Stuff main needs to do:
    - instructions to start game (list valid commands)
    - print score
    - tick_turn
    - update the state
    - insert piece (account for failure)
    - check win at every turn
    - print new board after every turn
    - win prompt/game over/bye bye
    - quit/exit command
*)

(****************** Begin string resources for Main ******************)
let stall_time = 0.15

let loading_str = "Loading... \n"

let print_welcome () = 
  print_string "\n~~~ Welcome to ";
  ANSITerminal.(print_string [red; Bold] "Connect ");
  ANSITerminal.(print_string [blue; Bold] "Mour ");
  print_string "~~~ \n\nFor the best experience, adjust your terminal to show 22 lines. \n\n"

let dimensions_prompt = {|
Press enter to play on the classic 6 x 6 board. 
Else type "[rows]" with each dimension between 0 and 50.
|}

let print_colors_prompt () = 
  print_string "Press enter to play with the classic ";
  ANSITerminal.(print_string [red] "Red ");
  print_string "& "; 
  ANSITerminal.(print_string [blue] "Blue ");
  print_string {|colors. 
Else type "[color1] [color2]" from these: |};
  ANSITerminal.(print_string [red] "Red, ");
  ANSITerminal.(print_string [green] "Green, ");
  ANSITerminal.(print_string [yellow] "Yellow, ");
  ANSITerminal.(print_string [blue] "Blue, ");
  ANSITerminal.(print_string [magenta] "Magenta, ");
  ANSITerminal.(print_string [cyan] "Cyan. \n")

let cpu_prompt = {|
If either Player 1 or Player 2 is a CPU, type "p[n] [difficulty]". 
If not, press enter. 
|}

let game_mode_prompt = {|
If you want to play with random valued chips, type "random". 
If not, press enter. 
|}

let input_prompt = " > "

(** [start_game_str x y] is the string that introduces the game using a board of 
    dimensions [x] x [y].  *)
let print_start_game (x, y) (c1, c2)= 
  print_string "\nPlaying ";
  ANSITerminal.(print_string [style_of_color c1] "Connect ");
  ANSITerminal.(print_string [style_of_color c2] "Mour ");
  let r = string_of_int x in 
  let c = string_of_int y in
  print_string ("with a " ^ r ^ " x " ^ c ^ " board. \n")

(* TODO: update the instructions *)
let print_instructions () = 
  ANSITerminal.(print_string [Bold] "These are the rules: \n");
  print_string "  GOAL: Score more points than your opponent. 
  1. First player to connect 4 chips earns 20 points. 
  2. Chips in horizontal, vertical, or diagonal lines that add up to 10 earn the
     corresponding player 10 points. It is possible to win without connecting 4. 
  3. You may play only one of each piece from 1-9 but unlimited 0 chips. \n";
  ANSITerminal.(print_string [Bold] "How to play: \n");
  print_string {|  On your turn, these are the commands you can type: 
   - Insert chip: "insert [column number] [0...9]"
   - Rotate board: "rotate [1...3]"
   - Switch colors: "switch"
   - Check score: "score"
   - Undo move: "undo"
   - Quit game: "quit" 

|}

let print_cmd_prompt c st = 
  print_string "\nIt\'s ";
  ANSITerminal.(print_string [style_of_color c] (string_of_color c ^ "\'s "));
  if get_gamemode st then 
    print_string {|turn. Make your move: "insert [column]", "rotate [num]", "switch", "score", "undo", "quit".
|}
  else
    print_string {|turn. Make your move: "insert [column] [value]", "rotate [num]", "switch", "score", "undo", "quit".
|}

let print_err err_str = 
  ANSITerminal.(print_string [red] ("~ERROR: " ^ err_str ^ " \n\n"))

let invalid_dims_err = "Invalid dimensions. Size must be greater than 4x4. "
let same_color_err = "Player colors cannot be the same."
let invalid_col_err = "That column doesn't exist."
let invalid_rot_err = "Cannot rotate that many times. Try a number between 1-3."
let ins_val_err = "Cannot insert piece of that value. May have already been played. "
let full_col_err = "That column is full."
let invalid_cmd_err = "Invalid command. Try again."
let undo_err = "Cannot undo without first making a move."

(****************** End string resources for Main ******************)

(** [print_win st c] prints the state [st] and the win message. *)
let print_win st c = 
  print_win st c;

  let c1 = get_p1_color st in
  let c2 = get_p2_color st in
  ANSITerminal.(print_string [style_of_color c1] (string_of_color c1));
  print_string (": " ^ (st |> score |> fst |> string_of_int) ^ " points\n");
  ANSITerminal.(print_string [style_of_color c2] (string_of_color c2));
  print_string (": " ^ (st |> score |> snd |> string_of_int) ^ " points\n");
  print_string "\nGame over! ";
  ANSITerminal.(print_string[style_of_color c] (string_of_color c));
  print_string " wins!";
  print_newline ();
  exit 0

(** [print_tie st] prints the state [st] and the tie message. *)
let print_tie st = 
  print st;
  print_col_nums st;
  let c1 = get_p1_color st in 
  let c2 = get_p2_color st in
  print_newline ();
  ANSITerminal.(print_string[style_of_color (c1)] "GAME");
  print_newline ();
  ANSITerminal.(print_string[style_of_color (c2)] "OVER");
  print_newline ();
  ANSITerminal.(print_string [style_of_color (c1)] (string_of_color c1));
  print_string (" and ");
  ANSITerminal.(print_string [style_of_color (c2)] (string_of_color c2));
  print_string (" TIE! ");
  print_newline ()

let stall (time) =
  ignore (Unix.select [] [] [] time)

let print_space () = 
  let rec space_rec i = 
    if i = 0 then () 
    else (print_newline (); space_rec (i-1)) in
  space_rec 10

let rec print_stripe start fin st = 
  let st = insert start 0 st in
  if start = fin then (st)
  else begin 
    stall stall_time;
    print_string loading_str;
    print st;
    print_space ();
    print_stripe (start + 1) fin st 
  end

let print_loadup c1 c2 = 
  let rec stripe_rec start fin reps st = 
    if reps = 0 then (stall stall_time; st)
    else let st' = (print_stripe start fin st) |> tick_turn in 
      stripe_rec start fin (reps - 1) st' in
  let st = new_state (c2 , c1) (6, 7) false in
  let st = st |> stripe_rec 0 6 3 |> stripe_rec 3 6 3 in
  stall stall_time;
  let ins_prnt_stall c v st = 
    let st' = st |> insert c v |> tick_turn in
    print_string loading_str;
    State.print st';
    print_space ();
    stall stall_time;
    st' in
  let _ = st |> ins_prnt_stall 0 0 |> ins_prnt_stall 1 0 
          |> ins_prnt_stall 2 0 |> ins_prnt_stall 0 0 |> ins_prnt_stall 1 0
          |> ins_prnt_stall 2 0 |> ins_prnt_stall 0 0 |> ins_prnt_stall 1 0 
          |> ins_prnt_stall 2 0 in
  stall 0.75;
  print_space ();
  print_space ()

let rec play (ai1_opt, ai2_opt) st : State.t =
  (* end game and print tie message if board is full. *)
  if check_full st then (print_tie st; exit 0) else
    print st;
  print_col_nums st;
  (* check and assign AI on this turn *)
  let current_player = get_current_color st in 
  let is_ai ai_opt = 
    match ai_opt with 
    | None -> false 
    | Some _ -> true in
  let current_cpu = 
    if (current_player = get_p1_color st && is_ai ai1_opt) then ai1_opt 
    else if (current_player = get_p2_color st && is_ai ai2_opt) then ai2_opt 
    else None in
  try begin 
    print_cmd_prompt (get_current_color st) st; 
    let cmd = 
      match current_cpu with 
      | None -> print_string input_prompt; Command.parse (read_line ())
      | Some d -> 
        let resp = get_response d st in 
        print_endline ("CPU: " ^ string_of_cmd resp); 
        resp in 
    eval_cmd (ai1_opt, ai2_opt) st cmd 
  end 
  with
  | Empty -> print_err invalid_cmd_err; play (ai1_opt, ai2_opt) st 
  | Malformed -> print_err invalid_cmd_err; play (ai1_opt, ai2_opt) st 

and eval_cmd ai_opts st (cmd:command) = 
  match cmd with
  | RInsert c ->  
    if get_gamemode st then 
      let v = get_valid_int (get_current_color st) st in 
      let new_st = 
        try insert c v st with exn -> 
          if exn = State.invalid_col_err then 
            (print_err invalid_col_err; play ai_opts st)
          else if exn = State.full_col_err then 
            (print_err full_col_err; play ai_opts st)
          else if exn = State.insert_value_err then 
            (print_err ins_val_err; play ai_opts st)
          else (print_err invalid_cmd_err; play ai_opts st) in
      check_win ai_opts new_st
    else (print_err invalid_cmd_err; play ai_opts st)
  | Insert (c, v) -> begin
      (*curr_ai is true if current player is an AI  *)
      let curr_ai = 
        match ai_opts with 
        | (None, Some v) -> (get_p2_color st = get_current_color st)
        | (Some v, None) -> (get_p1_color st = get_current_color st)
        | _ -> false in 
      if (not curr_ai && get_gamemode st) then 
        (print_err invalid_cmd_err; play ai_opts st)
      else
        let new_st = 
          try insert c v st with exn -> 
            if exn = State.invalid_col_err then 
              (print_err invalid_col_err; play ai_opts st)
            else if exn = State.full_col_err then 
              (print_err full_col_err; play ai_opts st)
            else if exn = State.insert_value_err then 
              (print_err ins_val_err; play ai_opts st)
            else (print_err invalid_cmd_err; play ai_opts st) in
        check_win ai_opts new_st
    end
  | Undo -> begin 
      try let st = undo st in play ai_opts st with exn -> 
        if exn = State.undo_err then 
          (print_err undo_err; play ai_opts st) 
        else (print_err invalid_cmd_err; play ai_opts st)
    end 
  | Rotate num -> 
    if num < 1 || num > 3 then 
      (print_err invalid_rot_err; play ai_opts st)
    else let new_st = st |> rotate num |> gravity in check_win ai_opts new_st
  | Score -> let (s1, s2) = st |> score in 
    let c1 = get_p1_color st in
    let c2 = get_p2_color st in
    ANSITerminal.(print_string [style_of_color c1] (string_of_color c1 ^ "\'s "));
    print_string "score: ";
    ANSITerminal.(print_string [style_of_color c1] (string_of_int s1));
    print_string "\n";
    ANSITerminal.(print_string [style_of_color c2] (string_of_color c2 ^ "\'s "));
    print_string "score : ";
    ANSITerminal.(print_string [style_of_color c2] (string_of_int s1));
    print_string "\n";
    play ai_opts st
  | Switch -> st |> switch_colors |> check_win ai_opts
  | Quit -> ANSITerminal.(print_string[green] "Bye-bye!\n"); exit 0

and check_win ai_opts new_st  =
  let c1 = get_p1_color new_st in 
  let c2 = get_p2_color new_st in 
  if State.check_win new_st 4 = Win c1 then 
    print_win new_st c1
  else if State.check_win new_st 4 = Win c2 then 
    print_win new_st c2
  else new_st |> tick_turn |> play ai_opts

let start_game ((r, c):int * int) ((c1, c2):color * color) ai_opts random =
  print_start_game (r, c) (c1, c2);
  let st = new_state (c1, c2) (r, c) random in
  play ai_opts st |> ignore 

let color_of_str str = 
  match String.lowercase_ascii str with 
  | "red" -> Red
  | "green" -> Green
  | "yellow" -> Yellow
  | "blue" -> Blue
  | "magenta" -> Magenta
  | "cyan" -> Cyan
  | _ -> failwith "Not a valid color. "

let rec dimensions_repl () : (int * int) = begin
  print_string dimensions_prompt;
  print_string input_prompt;
  match read_line () |> lowercase_ascii |> trim |> split_on_char ' ' with 
  | e :: [] -> if e = "" then (6, 6) 
    else 
      let num_rows = int_of_string e in
      if num_rows < 4 
      then (print_err invalid_dims_err; dimensions_repl ())
      else (num_rows, num_rows)
  | _ -> print_err invalid_cmd_err; dimensions_repl () 
end

let rec colors_repl () : (color * color) = begin
  print_colors_prompt ();
  print_string input_prompt;
  match read_line () |> lowercase_ascii |> trim |> split_on_char ' ' with 
  | e :: [] -> 
    if e = "" then (Red, Blue) 
    else (print_err invalid_cmd_err; colors_repl ())
  | c1 :: c2 :: [] -> 
    if c1 = c2 then (print_err same_color_err; colors_repl ()) 
    else (color_of_str c1, color_of_str c2)
  | _ -> print_err invalid_cmd_err; colors_repl ()
end

(** [diff_of_string s] is the difficulty that is represented in the string [s]. 
    Requires: s is either ["easy"] or ["hard"]. *)
let diff_of_string s = 
  if s = "easy" then Easy else Hard

let rec cpu_repl () : (difficulty option * difficulty option) = begin
  print_string cpu_prompt;
  print_string input_prompt;
  match read_line () |> lowercase_ascii |> trim |> split_on_char ' ' with 
  | e :: [] -> 
    if e = "" then (None, None) 
    else (print_err invalid_cmd_err; cpu_repl ())
  | p :: d :: [] -> 
    if (p = "p1" || p = "p2") && (d = "easy" || d = "hard") 
    then let diff = diff_of_string d in
      if p = "p1" then (Some diff, None) else (None, Some diff)
    else (print_err invalid_cmd_err; cpu_repl ())
  | _ -> print_err invalid_cmd_err; cpu_repl ()
end

let rec random_repl () = 
  print_string game_mode_prompt;
  print_string input_prompt;
  match read_line () |> lowercase_ascii |> trim with 
  | "random" -> true
  | _ -> false

let intro_repl () = 
  let (c1, c2) = colors_repl () in
  let (ai1_opt, ai2_opt) = cpu_repl () in
  let print_cpu ai_opt = 
    match ai_opt with 
    | None -> print_string ""
    | Some d -> 
      let diff_str = if d = Easy then "n easy" else " hard" in
      print_string ("a" ^ diff_str ^ " CPU of color ") in
  print_string "Player 1 is ";
  print_cpu ai1_opt;
  ANSITerminal.(print_string [style_of_color c1] (string_of_color c1));
  print_string " and Player 2 is ";
  print_cpu ai2_opt;
  ANSITerminal.(print_string [style_of_color c2] (string_of_color c2));
  print_string ". \n";
  let random = random_repl () in 
  let print_random random = 
    match random with 
    | true -> print_string "Gamemode: random"; 
    | false -> print_string "Gamemode: normal"; in 
  print_random random;
  start_game (dimensions_repl ()) (c1, c2) (ai1_opt, ai2_opt) random

let main () =
  (* TODO: uncomment this. 
     print_loadup Red Blue; *)
  print_welcome ();
  print_instructions ();
  intro_repl ()

let () = main ()
