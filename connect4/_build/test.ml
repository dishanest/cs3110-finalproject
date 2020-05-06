open OUnit2
(* Run "make build" if the lines below get "Unbound module error" *)
open State

(* let make_test (name: string) = 0 *)


let three_red_col = 
  (new_state (Red, Blue) (6, 7) false) |> insert 0 0 |> insert 0 0 |> insert 0 0
let three_red_row = 
  (new_state (Red, Blue) (6, 7) false) |> insert 0 0 |> insert 1 0 |> insert 2 0
let diag = 
  (new_state (Red, Blue)  (6, 7) false) |> insert 0 0 |> tick_turn |> insert 1 0 |> tick_turn |> insert 1 0 |> tick_turn 
  |> insert 2 0 |> tick_turn |> insert 2 0 |> tick_turn |> insert 3 0 |> tick_turn |> insert 2 0 |> tick_turn |> insert 3 0 |> tick_turn 
  |> insert 3 0 |> tick_turn |> insert 0 0 |> tick_turn |> insert 3 0

let _ = State.print diag

let mk_chkwin_tst name expected state = 
  name >:: begin
    fun _ -> assert_equal (check_win state 4) expected 
        ~printer: begin 
          fun w -> 
            match w with 
            | NWin -> "nwin" 
            | Win color -> string_of_color color 
        end
  end

let check_win_tests = [
  mk_chkwin_tst "3 red in col" NWin three_red_col;
  mk_chkwin_tst "4 red in col" (Win Red) (three_red_col |> insert 0 0);
  mk_chkwin_tst "5 red in col" (Win Red) (three_red_col |> insert 0 0 |> insert 0 0);
  mk_chkwin_tst "3 red in row" NWin three_red_row;
  mk_chkwin_tst "4 red in row" (Win Red) (three_red_row |> insert 3 0);
  mk_chkwin_tst "5 red in row" (Win Red) (three_red_row |> insert 3 0);
  mk_chkwin_tst "4 red diag" (Win Red) (diag)
]

let tests =
  "test suite for connect4"  >::: List.flatten [
    check_win_tests
  ]

let _ = run_test_tt_main tests