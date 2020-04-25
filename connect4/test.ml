open OUnit2
(* Run "make build" if the lines below get "Unbound module error" *)
open State

(* let make_test (name: string) = 0 *)


let three_red_col = 
  (new_state Red 6 7) |> insert 0 0 |> insert 0 0 |> insert 0 0
let three_red_row  = 
  (new_state Red 6 7) |> insert 0 0 |> insert 1 0 |> insert 2 0

let mk_chkwin_tst name expected state = 
  name >:: (fun _ -> assert_equal (check_win state) expected ~printer:string_of_bool)

let check_win_tests = [
  mk_chkwin_tst "3 red in col" false three_red_col;
  mk_chkwin_tst "4 red in col" true (three_red_col |> insert 0 0);
  mk_chkwin_tst "5 red in col" true (three_red_col |> insert 0 0 |> insert 0 0);
  mk_chkwin_tst "3 red in row" false three_red_row;
  mk_chkwin_tst "4 red in row" true (three_red_row |> insert 3 0);
  mk_chkwin_tst "5 red in row" true (three_red_row |> insert 3 0);
]

let tests =
  "test suite for connect4"  >::: List.flatten [
    check_win_tests
  ]

let _ = run_test_tt_main tests