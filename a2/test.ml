open OUnit2
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(** [test_str name input expected] constructs an OUnit test named [name]
    that asserts the equality of [expected] with [input]. *)
let test_str (name : string) input expected : test = 
  name >:: (fun _ -> assert_equal ~printer:pp_string expected input)

(* [test_cmp_lst name input expected] constructs an OUnit test named [name]
    that asserts the equality of [lst1] with [lst2]. *)
let test_cmp_lst name input expected = 
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) 
        input expected)

let test_cmd name input expected = 
  name >:: (fun _ -> 
      assert_equal input expected 
        ~printer:(fun cmd -> 
            match cmd with 
            | Go lst -> pp_list pp_string lst 
            | Quit -> "command : Quit"))

let test_state name input expected = 
  name >:: (fun _ -> 
      assert_equal input expected 
        ~printer: (fun result -> 
            match result with 
            | Legal t -> current_room_id t
            | Illegal -> "illegal"))

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

let lonely_adv = "lonely_room.json" |> Yojson.Basic.from_file |> from_json
let lonely_init = init_state lonely_adv

let ho_adv = "ho_plaza.json" |> Yojson.Basic.from_file |> from_json
let ho_init = init_state ho_adv
let ho_tower = 
  match go "clock tower" ho_adv ho_init with 
  | Legal st -> st
  | Illegal -> failwith "illegal"
let ho_tower_nirvana = 
  match go "higher" ho_adv ho_tower with 
  | Legal st -> st
  | Illegal -> failwith "illegal"

let adventure_tests =
  [
    (* TODO: add tests for the Adventure module here *)

    (* tests for lonely_adv *)
    test_str "lonely strt rm" (start_room lonely_adv) "the room";
    test_cmp_lst "lonely rms" (room_ids lonely_adv) ["the room"];
    test_str "lonely dscrptn" 
      (description lonely_adv "the room") "A very lonely room.";
    test_cmp_lst "lonely exits"
      (exits lonely_adv "the room") [];

    (* tests for ho_adv *)
    test_str "ho strt rm" (start_room ho_adv) "ho plaza"; 
    test_cmp_lst "ho rms in order" 
      (room_ids ho_adv) ["ho plaza"; "health"; "tower"; "nirvana"];
    test_cmp_lst "ho rms diff order" 
      (room_ids ho_adv) ["nirvana"; "health"; "ho plaza"; "tower"];
    test_str "ho plaza dscrptn"
      (description ho_adv "ho plaza") "You are on Ho Plaza. Cornell Health is to the southwest. The chimes are playing a concert in the clock tower. Someone tries to hand you a quartercard, but you avoid them.";
    test_cmp_lst "ho plaza exits"
      (exits ho_adv "ho plaza") ["southwest"; "south west"; "Cornell Health"; 
                                 "Gannett"; "chimes"; "concert"; "clock tower"];
    test_cmp_lst "tower exits"
      (exits ho_adv "tower") ["down"; "back"; "Ho Plaza"; "higher"];
    test_str "ho plaza to clock tower" 
      (next_room ho_adv "ho plaza" "clock tower") "tower";
    test_str "ho plaza to chimes" 
      (next_room ho_adv "ho plaza" "chimes") "tower";  
    test_str "clock tower to higher" 
      (next_room ho_adv "tower" "higher") "nirvana";
    test_cmp_lst "clock tower exit rooms" 
      (next_rooms ho_adv "tower") ["ho plaza"; "nirvana"];
  ]
let command_tests =
  [
    test_cmd "cmd quit" (parse "quit") Quit;
    test_cmd "cmd go somewhere" (parse "go somewhere") (Go ["somewhere"]);
    test_cmd "cmd go two words" (parse "go two words") (Go ["two"; "words"]);
    test_cmd "cmd edge spaces" (parse "   go    x  ") (Go ["x"]);
    "exn empty string" >:: 
    (fun _ -> assert_raises (Empty) (fun _ -> parse ""));
    "exn empty spaces" >:: 
    (fun _ -> assert_raises (Empty) (fun _ -> parse "      "));
    "exn malformed go + empty" >:: 
    (fun _ -> assert_raises (Malformed) (fun _ -> parse "go"));
    "exn malformed quit + nonempty" >:: 
    (fun _ -> assert_raises (Malformed) (fun _ -> parse "quit it"));
  ]

let state_tests =
  [
    (* tests for lonely_adv *)
    test_str "lonely_init start room" (current_room_id lonely_init) "the room";
    test_cmp_lst "lonely_init visited" (visited lonely_init) ["the room"];

    (* tests for ho_adv *) 
    test_str "ho_init start room" (current_room_id ho_init) "ho plaza";
    test_cmp_lst "ho_init visited" (visited ho_init) ["ho plaza"];
    test_str "ho_tower current" (current_room_id ho_tower) "tower";
    test_cmp_lst "ho_tower visited" (visited ho_tower) ["ho plaza"; "tower"];
    test_str "ho_tower_nirvana current" 
      (current_room_id ho_tower_nirvana) "nirvana";
    test_cmp_lst "ho_tower_nirvana visited" 
      (visited ho_tower_nirvana) ["ho plaza"; "tower"; "nirvana"];
    test_state "ho_init legal" (go "chimes" ho_adv ho_init) (Legal ho_tower);
    test_state "ho_init illegal" (go "home" ho_adv ho_init) (Illegal);
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
