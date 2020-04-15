(* Note: You may introduce new code anywhere in this file. *) 
open Adventure
open Command

type t = {
  adv : Adventure.t;
  current : string; 
  visited : string list;
}

let init_state adv = {
  adv = adv;
  current = start_room adv;
  visited = [start_room adv];
}

let current_room_id st = st.current

let visited st = st.visited

type result = Legal of t | Illegal

let go ex adv st =
  try
    let rm = next_room adv (st.current) ex in
    Legal {
      adv = adv;
      current = rm;
      visited = st |> visited |> (fun lst -> rm::lst)|> List.sort_uniq compare;
    }
  with 
  | UnknownRoom _ -> Illegal
  | UnknownExit _ -> Illegal
  | Empty -> Illegal
  | Malformed -> Illegal
