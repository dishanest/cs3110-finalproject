open State
open Command

(* TODO: 
   - main has to validate user input to guarantee all the preconditions. 
     -- When the game begins, size of the board should be at least 4x4
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

let dimensions_prompt = "\nHit enter to play on the classic 6 x 7 board. \nElse type \"Rows Columns\" with each dimension between 0 and 50. \n"

let print_colors_prompt () = 
  print_string "Hit enter to play with the classic ";
  ANSITerminal.(print_string [red] "Red ");
  print_string "& "; 
  ANSITerminal.(print_string [blue] "Blue ");
  print_string "colors. \nElse type \"Color1 Color2\" from the following options: ";
  ANSITerminal.(print_string [red] "Red, ");
  ANSITerminal.(print_string [green] "Green, ");
  ANSITerminal.(print_string [yellow] "Yellow, ");
  ANSITerminal.(print_string [blue] "Blue, ");
  ANSITerminal.(print_string [magenta] "Magenta, ");
  ANSITerminal.(print_string [cyan] "Cyan. \n")

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
  1. The first player to connect 4 pieces in a horizontal, vertical, or diagonal
     line ends the game and earns 20 points. 
  2. Consecutive horizontal, vertical, and diagonal pieces with points that add 
     up to exactly 10 give the corresponding player 10 points. Thus it is 
     possible to win without connecting 4 pieces in a row. 
  3. You may place as many 0 pieces as you want, but you can place only one of 
     each piece from 1-9. \n";
  ANSITerminal.(print_string [Bold] "How to play: \n");
  print_string "  On your turn, you can either insert a piece, rotate the board, or switch the 
  colors of the pieces on the board. Here are the commands you can type: 
   - Insert piece: \"insert [column] [0...9]\" 
   - Rotate board: \"rotate [number]\"
   - Switch colors: \"switch\"
   - Check score: \"score\"
   - Undo move: \"undo\"
   - Quit game: \'quit\' \n\n"

let print_cmd_prompt c = 
  print_string "\nIt\'s ";
  ANSITerminal.(print_string [style_of_color c] (string_of_color c ^ "\'s "));
  print_string "turn. Make your move: \"insert [column] [value]\", \"rotate [num]\", \"switch\", \"score\", \"undo\", \"quit\". \n"

let print_err err_str = 
  ANSITerminal.(print_string [red] ("~ERROR: " ^ err_str ^ " \n\n"))

let same_color_err = "Player colors cannot be the same."
let invalid_col_err = "That column doesn't exist."
let full_col_err = "That column is full."
let invalid_cmd_err = "Invalid command. Try again."
let undo_err = "Cannot undo without first making a move."

(****************** End string resources for Main ******************)

(** [print_win st c] prints the state [st] and the win message. *)
let print_win st c = 
  print_win st c;
  (* TODO: uncomment this when score is done. 
     let c1 = get_p1_color st in
     let c2 = get_p2_color st in
     ANSITerminal.(print_string [style_of_color c1] (string_of_color c1));
     print_string (": " ^ (st |> score |> string_of_int) ^ "points\n");
     ANSITerminal.(print_string [style_of_color c2] (string_of_color c2));
     print_string (": " ^ (st |> score |> string_of_int) ^ "points\n"); *)
  print_string "\nGame over! ";
  ANSITerminal.(print_string[style_of_color c] (string_of_color c));
  print_string " wins!";
  print_newline ();
  exit 0

(** [print_tie st] prints the state [st] and the tie message. *)
let print_tie st = 
  print st;
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
    else begin
      let st' = (print_stripe start fin st) |> tick_turn in 
      stripe_rec start fin (reps - 1) st' 
    end in
  let st = new_state (c2 , c1) 6 7 in
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

let rec play st =
  if check_full st then (print_tie st; exit 0) else
    print st;
  print_cmd_prompt (get_current_color st); 
  print_string input_prompt;
  try let cmd = Command.parse (read_line ()) in
    match cmd with
    | Insert (c, v) -> begin
        let new_st = 
          try insert c v st with exn -> 
            if exn = State.invalid_col_err then 
              (print_err invalid_col_err; play st)
            else if exn = State.full_col_err then 
              (print_err full_col_err; play st)
            else (print_err invalid_cmd_err; play st) in
        check_win new_st
      end
    | Undo -> begin
        try let st = undo st in play st with exn -> 
          if exn = State.undo_err then 
            (print_err undo_err; play st) 
          else (print_err invalid_cmd_err; play st)
      end
    | Rotate num -> let new_st = st |> rotate num |> gravity in check_win new_st
    | Score -> st |> score |> string_of_int |> print_string; play st
    | Switch -> st |> switch_colors |> check_win
    | Quit -> ANSITerminal.(print_string[green] "Bye-bye!\n"); exit 0 
  with
  | Empty -> print_err invalid_cmd_err; play st
  | Malformed -> print_err invalid_cmd_err; play st

and check_win new_st =
  let c1 = get_p1_color new_st in 
  let c2 = get_p2_color new_st in 
  if State.check_win new_st 4 = Win c1 then 
    print_win new_st c1
  else if State.check_win new_st 4 = Win c2 then 
    print_win new_st c2
  else new_st |> tick_turn |> play

let start_game ((r, c):int * int) ((c1, c2):color * color) =
  print_start_game (r, c) (c1, c2);
  let st = new_state (c1, c2) r c in
  play st |> ignore

let color_of_str str = 
  match String.lowercase_ascii str with 
  | "red" -> Red
  | "green" -> Green
  | "yellow" -> Yellow
  | "blue" -> Blue
  | "magenta" -> Magenta
  | "cyan" -> Cyan
  | _ -> failwith "Not a valid color. "

let intro_repl () = 
  let rec dimensions_repl () : (int * int) = begin
    print_string dimensions_prompt;
    print_string input_prompt;
    match read_line () |> String.trim |> String.split_on_char ' ' with 
    | e :: [] -> if e = "" then (6, 7) else (print_err invalid_cmd_err; dimensions_repl ())
    | r :: c :: [] -> (int_of_string r, int_of_string c)
    | _ -> print_err invalid_cmd_err; dimensions_repl () 
  end in
  let rec colors_repl () : (color * color) = begin
    print_colors_prompt ();
    print_string input_prompt;
    match read_line () |> String.trim |> String.split_on_char ' ' with 
    | e :: [] -> 
      if e = "" then (Red, Blue) 
      else (print_err invalid_cmd_err; colors_repl ())
    | c1 :: c2 :: [] -> 
      if c1 = c2 then (print_err same_color_err; colors_repl ()) 
      else (color_of_str c1, color_of_str c2)
    | _ -> print_err invalid_cmd_err; colors_repl ()
  end in 
  let (c1, c2) = colors_repl () in
  print_string "Player 1 is ";
  ANSITerminal.(print_string [style_of_color c1] (string_of_color c1));
  print_string " and Player 2 is ";
  ANSITerminal.(print_string [style_of_color c2] (string_of_color c2));
  print_string ". \n";
  start_game (dimensions_repl ()) (c1, c2)

let main () =
  (* TODO: uncomment this. 
     print_loadup Red Blue; *)
  print_welcome ();
  print_instructions ();
  intro_repl ()

let () = main ()
