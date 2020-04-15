open State

let _ = print_endline "testing main"

(* TODO: 
   - main has to validate user input to guarantee all the preconditions. 
     -- When the game begins, size of the board should be at least 4x4
*)
let play_game x y =
  print_string ("Playing connect 4 with a\"" ^ x ^ " by " ^ y"grid. \n");
  print_string ("To exit game, type \'quit\'. \n\n"); 
  let play = failwith "unimplemented"

let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to Connect 4.\n\n");
  print_endline "Please enter the board size (x,y) you want to load.";
  print_string  "> ";
  match (read_line () |> String.split_on_char ' ' )with
  | exception End_of_file -> ()
  | h::t -> play_game h t

let () = main ()


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