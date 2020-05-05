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

let colors_prompt  = "Please enter the colors of the players. \n"

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

let invalid_col_err = "That column doesn't exist. \n"

let full_col_err = "That column is full. \n"

let invalid_cmd_err = "Invalid command. Try again. \n"

(****************** End string resources for Main ******************)

(** [get_next_el lst] returns the head of [lst] *)
let get_next_el (lst: string list) : string = 
  match lst with
  | [] -> failwith "no next element"
  | h :: t -> h

(** [print_win st c] prints the state [st] and the win message. *)
let print_win st c = 
  print st;
  print_string ("Game over! ");
  ANSITerminal.(print_string[style_of_color c] (string_of_color c));
  print_string (" wins!");
  print_newline ();
  exit 0

let rec play st =
  print st;
  print_cmd_prompt (get_current_color st); 
  print_string input_prompt;
  try let cmd = Command.parse (read_line()) in
    match cmd with
    | Insert cmd_lst -> 
      begin
        match cmd_lst with
        | [] -> ANSITerminal.(print_string[red] invalid_cmd_err); play st
        | h::t -> 
          let y = get_next_el t in
          let new_st = try insert (int_of_string h) (int_of_string y) st 
            with exn -> 
              if exn = State.invalid_col_err then 
                (ANSITerminal.(print_string[red] invalid_col_err); play st) 
              else if exn = State.full_col_err then 
                (ANSITerminal.(print_string[red] full_col_err); play st)
              else play st in
          let p1_color = get_p1_color new_st in 
          let p2_color = get_p2_color new_st in 
          if State.check_win new_st 4 = Win p1_color then 
            print_win new_st p1_color
          else if State.check_win new_st 4 = Win p2_color then 
            print_win new_st p2_color
          else new_st |> tick_turn |> play
      end
    | Undo -> let st = undo st in play st
    | Rotate cmd_lst -> 
      begin
        match cmd_lst with
        | [] -> ANSITerminal.(print_string[red] invalid_cmd_err); play st
        | h::t ->  
          let new_st = st |> rotate (int_of_string h) |> gravity in 
          let p1_color = get_p1_color new_st in 
          let p2_color = get_p2_color new_st in
          if State.check_win new_st 4 = Win p1_color then 
            print_win new_st p1_color
          else if State.check_win new_st 4 = Win p2_color then 
            print_win new_st p2_color
          else new_st |> tick_turn |> play
      end
    | Score -> st |> score |> string_of_int |> print_string; play st
    | Quit -> ANSITerminal.(print_string[red] "Goodbye.\n"); exit 0 
  with
  | Empty -> ANSITerminal.(print_string[red] invalid_cmd_err); play st
  | Malformed -> ANSITerminal.(print_string[red] invalid_cmd_err); play st

let start_game ((r, c):int * int) ((c1, c2):color * color) =
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
    | e :: [] -> if e = "" then (6, 7) else (print_endline invalid_cmd_err; dimensions_repl ())
    | r :: c :: [] -> (int_of_string r, int_of_string c)
    | _ -> print_endline invalid_cmd_err; dimensions_repl () 
  end in
  let rec colors_repl () : (color * color) = begin
    print_colors_prompt ();
    print_string input_prompt;
    match read_line () |> String.trim |> String.split_on_char ' ' with 
    | e :: [] -> if e = "" then (Red, Blue) else (print_endline invalid_cmd_err; colors_repl ())
    | c1 :: c2 :: [] -> (color_of_str c1, color_of_str c2)
    | _ -> print_endline invalid_cmd_err; colors_repl ()
  end in start_game (dimensions_repl ()) (colors_repl ())

let main () =
  print_welcome ();
  intro_repl ()

let () = main ()
