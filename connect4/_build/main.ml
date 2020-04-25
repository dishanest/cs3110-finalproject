open State
open Command

let _ = print_endline "testing main"

(* TODO: 
   - main has to validate user input to guarantee all the preconditions. 
     -- When the game begins, size of the board should be at least 4x4
*)

(** [get_next_el lst] returns the head of [lst] *)
let get_next_el (lst: string list) : string = 
  match lst with
  | [] -> failwith "no next element"
  | h::t -> h

let rec play st =
  print st;
  ANSITerminal.(print_string[white] "\n enter command (Insert col val, Rotate 1-4, Score, Quit)\n");
  try let cmd = Command.parse (read_line()) in
    match cmd with
    | Insert cmd_lst -> 
      begin
        match cmd_lst with
        | [] -> ANSITerminal.(print_string[red] "Invalid command\n"); play st
        | h::t -> let y = get_next_el t in
          let new_st = try insert (int_of_string h) (int_of_string y) st with
            | Error -> ANSITerminal.(print_string[red] "Error inserting\n"); play st in 
          if State.check_win new_st then 
            begin 
              print_string ("player " ^ get_state_color new_st ^ " wins!"); 
              exit 0
            end
          else
            new_st 
            |> tick_turn 
            |> play
      end
    | Rotate cmd_lst -> 
      begin
        match cmd_lst with
        | [] -> ANSITerminal.(print_string[red] "Invalid command\n"); play st
        | h::t ->  
          let new_st = 
            st 
            |> rotate (int_of_string h) in 
          if State.check_win new_st then 
            begin 
              print_string ("player " ^ get_state_color new_st ^ " wins!"); 
              exit 0
            end
          else
            new_st
            |> tick_turn 
            |> play 
      end
    | Score -> st 
               |> score 
               |> string_of_int 
               |> print_string; play st
    | Quit -> ANSITerminal.(print_string[red] "Goodbye.\n"); exit 0 
  with
  | Empty -> ANSITerminal.(print_string[red] "Empty Command\n"); play st
  | Malformed -> ANSITerminal.(print_string[red] "Malformed Command\n"); play st

let play_game x y =
  print_string ("Playing connect 4 with a\"" ^ x ^ " by " ^ y ^"grid. \n");
  print_string ("To exit game, type \'quit\'. \n\n");
  let st = new_state Red (int_of_string x) (int_of_string y) in
  play st |> ignore

let main () =
  ANSITerminal.(print_string [white]
                  "\nWelcome to Connect 4.\n\n
                  These are the rules: the first player to connect 4 chips wins.\n");
  print_endline "Please enter the board size (x,y) you want to load.";
  print_string  "> ";
  match (read_line () |> String.split_on_char ' ' )with
  | exception End_of_file -> ()
  | [] -> ()
  | h::t -> t |> get_next_el |> play_game h

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
