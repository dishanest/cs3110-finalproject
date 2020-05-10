open OUnit2
open State

(* TODO: 50 passing tests therefore is a minimal amount *)

(**************************** Testing Methodology ****************************)
(* 
Answer the following questions:
- which parts of the system were automatically tested by OUnit vs. manually tested
- explain what modules were tested by OUnit and how test cases were developed (black box, glass box, randomized, etc.).
- provide an argument for why the testing approach demonstrates the correctness of the system


Summary
  We break our testing into three phases: System Testing, Subjective Testing, 
  and AI Testing. System Testing involves all the test cases written in this 
  OUnit test suite, including black box tests, as well as play testing to verify
  the logic of the system we created. Subjective Testing involves rigorous play 
  testing to demonstrate the usability of the program. AI Testing involves 
  playing against the AI and scaling up its difficulty/intelligence level to 
  provide a suitable challenge to human players.

System Testing (TODO)
  Black box tests
  Verifying interface specifications
  Black box play tests for each instance of the REPL - empty string, keyword without parameters, keyword with extra parameters, mispelled keyword, nonsensical command, 
  The correctness of the system is demonstrated by the correctness of every 
  function defined within the State interface. The [get_], [_of_], and  functions did not
  require testing. We tested every path of output for all other functions, 
  including exceptions. Certain functions required more cases than others:
  - [new_state]: 
  - [insert]: since this is independent of chip color, we tested using chips of 
    only one color. The correctness of using multiple colors is demonstrated 
    later. We tested using [make_assoc] to verify the coordinates of the 
    inserted chips. We made sure inserting into a full column raises 
    [full_col_err] and inserting into a nonexistent column raises 
    [invalid_col_err]. 
  - [get_valid_int]: this function produces a random value that cannot equal a 
    value that has already been played, so testing was done using 

  using test cases that worked with every
  permutation of patterns within the board. That is, every combination of 


Subjective Testing
  In addition to testing our system's logical correctness, we had to test it for
  usability, since after all, this is a game meant to be played. To test this, 
  we conducted rigorous human play tests by pitting ourselves against each other
  in round-robin format and recorded any criteria in the system in need of 
  improvement: convenience of typing commands, visual contrast, clarity of 
  instructions, etc. This testing happened continuously throughout development,
  every time we implemented a new feature. Only until we were satisfied with 
  both Subjective and System testing did we move on to implement other 
  features.

AI Testing
  During the Subjective Testing phase, we came up with several strategies and 
  analytical patterns to increase the changes of winning with our proprietary 
  set of rules. Certain were intuitive and difficult to implement in code, but 
  others could be quantified into certain criteria for the AI to evaluate in its
  weighted decision-making process. After each pattern we implemented, we played
  against the AI in a manner that intentionally set up to employ the use of 
  those patterns, and adjusted the AI's scoring mechanism accordingly. Moreover,
  we wrote several black box tests to verify that in certain board configs, 
  there are obvious "best moves" to take. 

*)

let three_red_col = 
  (new_state (Red, Blue) (6, 6) false) |> insert 0 0 |> insert 0 0 |> insert 0 0
let three_red_row = 
  (new_state (Red, Blue) (6, 6) false) |> insert 0 0 |> insert 1 0 |> insert 2 0
let diag = 
  (new_state (Red, Blue)  (6, 6) false) |> insert 0 0 |> tick_turn 
  |> insert 1 0 |> tick_turn |> insert 1 0 |> tick_turn |> insert 2 0 
  |> tick_turn |> insert 2 0 |> tick_turn |> insert 3 0 |> tick_turn 
  |> insert 2 0 |> tick_turn |> insert 3 0 |> tick_turn |> insert 3 0 
  |> tick_turn |> insert 0 0 |> tick_turn |> insert 3 0

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