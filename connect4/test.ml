open OUnit2
open State

(**************************** Testing Methodology ****************************)
(* 
Summary
  We break our testing into three phases: System Testing, Subjective Testing, 
  and AI Testing. System Testing involves all the test cases written in this 
  OUnit test suite, including black box tests, as well as play testing to verify
  the logic of the system we created. Subjective Testing involves rigorous play 
  testing to demonstrate the usability of the program. AI Testing involves 
  playing against the AI and scaling up its difficulty/intelligence level to 
  provide a suitable challenge to human players.

System Testing (TODO)
  The correctness of the system is demonstrated by the correctness of every 
  function defined within the State interface, as well as verifying the expected
  behavior for all other modules that can only be tested through play-testing. 

  Black Box Testing in OUnit: State
  The [get_] and [_of_] functions are simple and thus did not require 
  much testing. We tested every path of output for all other functions. 
  [print] and [print_win] were play-tested. We wrote a helper function 
  [apply_ins] to simulate players making moves. 
  Here is the approach for every other function: 
  - [check_full]: a full board is true and all non-full boards are false. 
  - [switch_colors]: all Red chips in st are Blue in st' and all Blue chips in 
    st are Red in st'. Tested on partially filled board and full board. 
  - [make_assoc]: matched partially and fully filled boards with their 
    corresponding assoc lists. 
  - [check_win]: Checked every possible orientation of winning 4 in a row, along
    with obstacles and intersecting lines. Checked non-winning states return 
    [NWin]. 
  - [check_val_used]: [false] for every value in empty board, [true] for every 
    value already placed inside board. 
  - [get_valid_int]: this function produces a random value that cannot equal a 
    value that has already been played, so we asserted that it could be anything
    EXCEPT for the numbers already played. 
  - [insert]: since this is independent of chip color, we tested using chips of 
    only one color. The correctness of using multiple colors is demonstrated 
    later. We tested using [make_assoc] to verify the coordinates of the 
    inserted chips. Exceptions were tested in play. 
  - [gravity]: Tested boards with various combinations of floating and grounded 
    chips, resulting in boards with only grounded chips in the same columns. 
  - [tick_turn]: Simple function. Verified that one color is changed to the 
    other between [st] and [st'] in other test groups. 
  - [undo]: This is essentially a get_previous_state function, so it was tested 
    here like a getter. Tested more in play testing. 
  - [new_state]: Simple function, has empty board and default fields. This is 
    where we tested getters. 
  - [score]: Verified that horizontal, vertical, diagonal lines individually 
    counted for scores, then tested overlapping/intersecting lines. Tested that
    chips on the same line separated by an empty cell or a chip of another color
    did not add score. Covered prototypes of every possible permution of 
    patterns in the board. 
  - [rotate]: Matched rotated states to expected output. Tested rotationally
    symmetrical cases as well as asymmetrical. 

  Play Testing: Main, Command, AI, State
  - For every opportunity to input a command, we tested the empty string, 
    keywords without parameters, keywords with extra parameters, and other 
    malformed commands. We also checked the expected behavior of all correct 
    commands. 
  - Raise exn if players choose the same color.
  - Raise exn if player chooses board that is too small or too large. 
  - AI works as Player 1 and Player 2. 
  - Both Random and Normal game modes and their respective [insert] commands
    behave properly. 
  - Output print matches expected behavior for every possible command. 
  - "Normal" win condition - player who connects 4 earns 20 points, allowing 
    them to win with the most points
  - "Underdog" win condition - the player who doesn't connect 4 pieces but still
    earns more points overall wins

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
  those patterns, and adjusted the AI's scoring mechanism accordingly. 

*)

(*************************** Begin Helper Functions ***************************)

(* [make_test n e i] tests [e = i] and does not print anything. *)
let make_test name expected input = 
  name >:: fun _ -> assert_equal input expected

(* [int_test n e i] tests [e = i] and prints ints. *)
let int_test name expected input = 
  name >:: fun _ -> assert_equal input expected ~printer: string_of_int

(** [st_test n e i] is the test with name [n], expected value [e], 
    input value [i]. *)
let st_test name expected_st input_st = 
  name >:: fun _ -> 
    assert_equal (get_board input_st) (get_board expected_st)

(** [print_st_test n e i] is the same as [st_test] but prints [e and i]. 
    This function is used for debugging. *)
let print_st_test name expected_st input_st = 
  let res = st_test name expected_st input_st in 
  print_endline "\nInput state: "; State.print input_st; 
  print_endline "\nExpected state: "; State.print expected_st; 
  res

(** creates a test for check_win *)
let checkwin_test name expected state = 
  name >:: fun _ -> 
    assert_equal (check_win state 4) expected ~printer: 
      (fun w -> match w with NWin -> "nwin" | Win c -> string_of_color c)


let rec print_assoc assoc = 
  match assoc with 
  | [] -> print_endline "\n";
  | (c, r) :: [] -> 
    print_string ("(" ^ string_of_int c ^ ", " ^ string_of_int r ^ ") \n")
  | (c, r) :: t -> 
    print_string ("(" ^ string_of_int c ^ ", " ^ string_of_int r ^ "); "); 
    print_assoc t

let rec print_board b = 
  match b with 
  | [] -> print_endline "\n";
  | h :: t -> 
    let rec print_row r = 
      match r with 
      | [] -> print_endline "\n";
      | h' :: t' -> match h' with 
        | None -> print_string "None; "; print_row t'
        | Some cell -> 
          let style = cell |> get_cell_color |> style_of_color in 
          let value = cell |> get_cell_value |> string_of_int in 
          print_string "Some "; ANSITerminal.(print_string [style] value); print_string "; ";
          print_row t'
    in print_row h; print_board t

let init_st = new_state (Red, Blue) (6, 6) false
let small_init_st = new_state (Red, Blue) (4, 4) false

(** [apply_ins init_st insertions] is the state that results from applying 
    [insertions] to [init_state]. Will [tick_turn] after every insertion
    if [turn] is [true] to simulate players taking turns. *)
let rec apply_ins (turn:bool) (insertions:(int*int) list) (init_st:State.t) = 
  match insertions with 
  | [] -> init_st
  | (c, v) :: t -> 
    apply_ins turn t
      (if turn then insert c v init_st |> tick_turn else insert c v init_st)

let almost_full_list = [
  (0, 0); (0, 0); (0, 0); 
  (1, 0); (1, 0); (1, 0); (1, 0); 
  (2, 0); (2, 0); (2, 0); (2, 0);
  (3, 0); (3, 0); (3, 0); (3, 0)
] 
let full_list = (0, 0) :: almost_full_list
let almost_full = small_init_st |> apply_ins false almost_full_list
let full = almost_full |> insert 0 0
let full_checker = small_init_st |> apply_ins true full_list

(**************************** End Helper Functions ****************************)

let check_full_tests = [
  make_test "almost full red board is false" false (check_full almost_full);
  make_test "full red is true" true (check_full full);
  make_test "full checker board is true" true (check_full full_checker);
  make_test "empty board is false" false (check_full init_st) ;
  make_test "partial board is false" false
    (init_st |> apply_ins false [(0, 0)] |> check_full);
]

let switch_tests = 
  let sw = switch_colors full in 
  let sw' = small_init_st |> tick_turn |> apply_ins false full_list in
  let sw_ch = switch_colors full_checker in 
  let sw_ch' = small_init_st |> tick_turn |> apply_ins true full_list in 
  let sw_almost = small_init_st |> tick_turn |> apply_ins false almost_full_list
  in [
    st_test "switch full once" (sw') sw;
    st_test "switch twice is same" full (switch_colors sw);
    st_test "switch checkers" sw_ch' sw_ch;
    st_test "switch partial" sw_almost (switch_colors almost_full);
  ]

let assoc_tests = 
  let full_red_assoc = make_assoc Red (get_board full) 0 in
  let assoc_result = [
    (0, 0); (1, 0); (2, 0); (3, 0); 
    (0, 1); (1, 1); (2, 1); (3, 1); 
    (0, 2); (1, 2); (2, 2); (3, 2); 
    (0, 3); (1, 3); (2, 3); (3, 3)
  ] in
  let red_checker_assoc = make_assoc Red (get_board full_checker) 0 in 
  let assoc_result'_red = [
    (1, 0); (3, 0); (1, 1); (3, 1); 
    (1, 2); (3, 2); (1, 3); (3, 3) 
  ] in
  let blue_checker_assoc = make_assoc Blue (get_board full_checker) 0 in
  let assoc_result'_blue = [
    (0, 0); (2, 0); (0, 1); (2, 1); 
    (0, 2); (2, 2); (0, 3); (2, 3) 
  ] in
  let full_blue_assoc = make_assoc Blue (get_board full) 0 in
  [ 
    make_test "full red assoc is full" assoc_result full_red_assoc;
    make_test "full blue assoc is empty" [] full_blue_assoc;
    make_test "alternating assoc red" assoc_result'_red red_checker_assoc;
    make_test "alternating assoc blue" assoc_result'_blue blue_checker_assoc;
  ]

let check_win_tests = 
  let three_col = init_st |> apply_ins false [(0, 0); (0, 0); (0, 0)] in
  let three_row = init_st |> apply_ins false [(0, 0); (1, 0); (2, 0)] in 
  let diag_simple = 
    init_st |> apply_ins true [
      (0, 0); (1, 0); (1, 0); (2, 0); (2, 0); (3, 0); 
      (2, 0); (3, 0); (3, 0); (0, 0); (3, 0)
    ] in [
    checkwin_test "3 in col" NWin three_col;
    checkwin_test "4 in col" (Win Red) (three_col |> insert 0 0);
    checkwin_test "5 in col" (Win Red) (three_col |> insert 0 0 |> insert 0 0);
    checkwin_test "3 in row" NWin three_row;
    checkwin_test "4 in row" (Win Red) (three_row |> insert 3 0);
    checkwin_test "5 in row" (Win Red) (three_row |> insert 3 0);
    checkwin_test "4 diag" (Win Red) diag_simple
  ]

let check_used_tests = 
  let all_vals_used = small_init_st |> apply_ins false [
      (0, 1); (0, 2); (0, 3); (0, 4); 
      (1, 5); (1, 6); (1, 7); (1, 8); 
      (2, 9); (2, 0)
    ] in
  [
    make_test "0 never used - init" false (check_val_used Red 0 init_st);
    make_test "init used none red" false (check_val_used Red 9 init_st);
    make_test "init used none blue" false (check_val_used Blue 4 init_st);
    make_test "0 never used - full" false (check_val_used Red 0 full);
    make_test "all used is true for nonzero" true 
      (check_val_used Red 3 all_vals_used);
    make_test "0 in all used is false" false (check_val_used Red 0 all_vals_used);
  ]

let insert_tests = 
  let assoc1 = [(3, 0)] in
  let inserted1 = small_init_st |> insert 0 0 in
  let assoc2 = [(2, 0); (3, 0)] in 
  let inserted2 = inserted1 |> insert 0 0 in 
  let assoc3 = [(3, 0); (3, 1)] in 
  let inserted3 = inserted1 |> insert 1 0 in 
  let assoc_full = [
    (0, 0); (1, 0); (2, 0); (3, 0); 
    (0, 1); (1, 1); (2, 1); (3, 1); 
    (0, 2); (1, 2); (2, 2); (3, 2); 
    (0, 3); (1, 3); (2, 3); (3, 3)
  ] in [
    make_test "insert 0 0" assoc1 (make_assoc Red (get_board inserted1) 0);
    make_test "insert same col" assoc2 (make_assoc Red (get_board inserted2) 0);
    make_test "insert same row" assoc3 (make_assoc Red (get_board inserted3) 0); 
    make_test "insert full" assoc_full (make_assoc Red (get_board full) 0);
  ]

let rotate_gravity_tests = 
  let col = small_init_st |> apply_ins false [(0, 0); (0, 0); (0, 0); (0, 0)] in
  let row = small_init_st |> apply_ins false [(0, 0); (1, 0); (2, 0); (3, 0)] in
  let diag = small_init_st |> apply_ins false [
      (0, 0); (1, 0); (1, 0); (2, 0); (2, 0); (2, 0); 
      (3, 0); (3, 0); (3, 0); (3, 0)
    ] in
  let diag' = small_init_st |> apply_ins false [
      (3, 0); (2, 0); (2, 0); (1, 0); (1, 0); (1, 0); 
      (0, 0); (0, 0); (0, 0); (0, 0)
    ] in
  let diag'' = [
    (0, 0); (1, 0); (2, 0); (3, 0); (0, 1); 
    (1, 1); (2, 1); (0, 2); (1, 2); (0, 3)
  ] in 
  let rotate_tests = [ (* rotate tests *)
    st_test "rotate row -> col" col (rotate 1 row);
    st_test "rotate 4 is identity" row (rotate 4 row);
    st_test "rotate 5 is rotate 1" (rotate 1 row) (rotate 5 row);
    st_test "rotate diag 1 -> diag'" diag' (rotate 1 diag);
    st_test "rotate is linear function" (rotate 2 diag') (rotate 3 diag);
    make_test "rotate diag 2" diag'' 
      (make_assoc Red (diag |> rotate 2 |> get_board) 0);
  ] in 
  let two_separate_cols = 
    col |> apply_ins false [(2, 0); (2, 0); (2, 0); (2, 0);] in
  let two_stacked_rows = 
    row |> apply_ins false [(0, 0); (1, 0); (2, 0); (3, 0)] in
  let gravity_tests = [ (* gravity tests *)
    st_test "gravity grounded row" col (gravity col);
    st_test "gravity grounded col" row (gravity row);
    st_test "gravity floating row" row (row |> rotate 2 |> gravity);
    st_test "gravity of grounded diag" diag (gravity diag);
    st_test "gravity of floating diag" diag' (diag |> rotate 2 |> gravity);
    st_test "gravity of full" full (gravity full);
    st_test "gravity of floating rows" 
      two_stacked_rows (two_separate_cols |> rotate 1 |> gravity);
  ] in rotate_tests @ gravity_tests

let new_st_tests = 
  let alt_row = 
    small_init_st |> apply_ins true [(0, 0); (1, 1); (2, 2); (3, 3)] in
  let alt_board = [ 
    [ None; None; None; Some { color = Red; value = 0 } ];
    [ None; None; None; Some { color = Blue; value = 1 } ];
    [ None; None; None; Some { color = Red; value = 2 } ]; 
    [ None; None; None; Some { color = Blue; value = 3 } ]; 
  ] in
  [
    make_test "get board" alt_board (get_board alt_row);
    make_test "get p1 color" Red (get_p1_color alt_row);
    make_test "get p2 color" Blue (get_p2_color alt_row);
    make_test "get current color" Red (get_current_color alt_row);
    make_test "get_dimensions" (4, 4) (get_dimensions alt_row);
    make_test "get_gamemode" false (get_gamemode alt_row);
    make_test "get random valid int" true (get_valid_int Red alt_row <> 2);
    make_test "get random valid int" true (get_valid_int Blue alt_row <> 1);
    make_test "get random valid int" true (get_valid_int Blue alt_row <> 3);
  ]

let score_tests = 
  let v0 = 
    small_init_st |> apply_ins false [(0, 0); (0, 0); (0, 0); (0, 0)] in
  let v10_cons = 
    small_init_st |> apply_ins false [(0, 1); (0, 2); (0, 3); (0, 4)] in
  let v10_gaps = 
    small_init_st |> apply_ins false [(0, 3); (0, 2); (0, 0); (0, 5)] in
  let v20 = v10_gaps |> apply_ins false [(3, 6); (3, 4)] in
  let h0 = 
    small_init_st |> apply_ins false [(0, 0); (1, 0); (2, 0); (3, 0)] in
  let h10_cons = 
    small_init_st |> apply_ins false [(0, 1); (1, 2); (2, 3); (3, 4)] in
  let h10_gaps = 
    small_init_st |> apply_ins false [(0, 3); (1, 2); (2, 0); (3, 5)] in
  let h20 = h10_gaps |> apply_ins false [(0, 6); (1, 4)] in
  let diag_solid = small_init_st |> apply_ins false [
      (0, 2); (1, 0); (1, 3); (2, 0); (2, 0); (2, 5)
    ] in
  let diag_alt = 
    small_init_st |> apply_ins true [
      (0, 2); (1, 0); (1, 3); (2, 0); (2, 0); (3, 0); (2, 5)
    ] in 
  let overlap_hz_diag = 
    small_init_st |> apply_ins false [(0, 4); (1, 1); (1, 6); (2, 5)] in
  let overlap_hz_diag' = 
    small_init_st |> apply_ins false [(0, 5); (1, 1); (1, 6); (2, 4)] in
  [
    int_test "zero score vertical" 0 (v0 |> score |> fst);
    int_test "consecutive vertical" 10 (v10_cons |> score |> fst);
    int_test "vertical with gaps" 10 (v10_gaps |> score |> fst);
    int_test "two columns" 20 (v20 |> score |> fst);
    int_test "zero score horizontal" 0 (h0 |> score |> fst);
    int_test "consecutive horizontal" 10 (h10_cons |> score |> fst);
    int_test "horizontal with gaps" 10 (h10_gaps |> score |> fst);
    int_test "two rows" 20 (h20 |> score |> fst);
    int_test "diag red only" 10 (diag_solid |> score |> fst);
    int_test "diag red and blue" 10 (diag_alt |> score |> fst);
    int_test "overlap hz and diag" 20 (overlap_hz_diag |> score |> fst);
    int_test "overlap hz and other diag" 20 (overlap_hz_diag' |> score |> fst);
  ]

let tests =
  "test suite for connect mour"  >::: List.flatten [
    check_win_tests; 
    check_full_tests;
    switch_tests;
    assoc_tests;
    check_used_tests;
    insert_tests;
    rotate_gravity_tests;
    new_st_tests;
    score_tests;
  ]

let _ = run_test_tt_main tests