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

let print_welcome () = 
  print_string "\n~~ Welcome to ";
  ANSITerminal.(print_string [red] "Connect ");
  ANSITerminal.(print_string [blue] "Mour ");
  print_string "~~ \n\nFor the best experience, expand your terminal to half the screen. \n\n"

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
let instructions = "These are the rules: the first player to connect 4 chips wins. \nTo exit game, type \'quit\'. \n"

let print_cmd_prompt c = 
  print_string "\nIt\'s ";
  ANSITerminal.(print_string [style_of_color c] (string_of_color c ^ "\'s "));
  print_string "turn. Make your move: \"insert [column] [value]\", \"rotate [num]\", \"score\", \"undo\", \"quit\". \n"

let print_err err_str = 
  ANSITerminal.(print_string [red] ("~ERROR: " ^ err_str))

let same_color_err = "Player colors cannot be the same. \n\n"
let invalid_col_err = "That column doesn't exist. \n\n"
let full_col_err = "That column is full. \n\n"
let invalid_cmd_err = "Invalid command. Try again. \n\n"

(****************** End string resources for Main ******************)

(** [print_win st c] prints the state [st] and the win message. *)
let print_win st c = 
  print st;
  print_string ("Game over! ");
  ANSITerminal.(print_string[style_of_color c] (string_of_color c));
  print_string (" wins!");
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
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ();
  print_newline ()

let rec print_stripe start fin st = 
  let st = insert start 0 st in
  if start = fin then 
    (st)
  else 
    (stall 0.2;
     print st;
     print_space ();
     print_stripe (start +1) fin st)

let print_loadup c1 c2 = 
  let st = new_state (c1,c2)  6 6 in
  let st = print_stripe 0 5 st in
  let st  = tick_turn st in
  let st = print_stripe 0 5 st in
  let st  = tick_turn st in
  let st = print_stripe 0 5 st in
  let st  = tick_turn st in
  let st = print_stripe 3 5 st in
  let st  = tick_turn st in
  let st = print_stripe 3 5 st in
  let st  = tick_turn st in
  let st = print_stripe 3 5 st in
  stall 0.2;

  let st  = tick_turn st in
  let st =  insert 0 0 st in 
  print st;
  stall 0.2;

  let st  = tick_turn st in
  let st =  insert 1 0 st in 
  print_space ();
  print st;
  stall 0.2;
  let st =  insert 0 0 st in 
  print_space ();
  print st;
  stall 0.2;

  let st  = tick_turn st in
  let st =  insert 2 0 st in 
  print_space ();
  print st;
  stall 0.2;
  let st =  insert 1 0 st in 
  print_space ();
  print st;
  stall 0.2;
  let st =  insert 0 0 st in 
  print_space ();
  print st;
  stall 0.2;

  let st  = tick_turn st in
  let st =  insert 2 0 st in 
  print_space ();
  print st;
  stall 0.2;
  let st =  insert 1 0 st in 
  print_space ();
  print st;
  stall 0.2;

  let st  = tick_turn st in
  let st =  insert 2 0 st in 
  print_space ();
  print st;

  print_space ();
  stall 2.0;
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
            else play st in
        check_win new_st
      end
    | Undo -> let st = undo st in play st
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
  print_loadup c1 c2;
  print_start_game (r, c) (c1, c2);
  print_string instructions;
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
  print_welcome ();
  intro_repl ()

let () = main ()
