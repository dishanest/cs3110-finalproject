open Adventure
open Command
open State

(** String output for an [Invalid] type exception. *)
let invalid = "Invalid command. Hint: try typing \'go' + destination. \n\n"
(** String output for an [Illegal] type command. *)
let illegal = "You can\'t go there because it doesn\'t exist, is too far, or you\'re already there. Try another destination. \n"
(** String output for a [Quit] type command. *)
let bye = "\nThanks for playing! \n"

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  print_string ("Playing text adventure from file: \"" ^ f ^ "\". \n");
  print_string ("To exit game, type \'quit\'. \n\n");
  (** [repl adv st] is the read-evaluate-print loop for the engine. *)
  let rec repl (adv:Adventure.t) (st:State.t) = 
    print_endline ((st |> current_room_id |> description adv) ^ "\n");
    print_string "> ";
    match read_line () with 
    | exception End_of_file -> ()
    | cmd_str -> 
      try 
        match parse cmd_str with
        | Go cmd_lst -> let res = st |> go (String.concat " " cmd_lst) adv in (
            match res with 
            | Legal next_st -> repl adv next_st
            | Illegal -> print_string illegal; repl adv st) 
        | Quit -> print_string bye; exit 0 
      with 
        Malformed | Empty -> print_string invalid; repl adv st in
  let adv = f |> Yojson.Basic.from_file |> from_json in 
  repl adv (init_state adv)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to the 3110 Text Adventure Game engine.\n\n");
  print_endline "Please enter the name of the game file you want to load.";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
