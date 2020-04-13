(* Note: You may introduce new code anywhere in this file. *) 
open Yojson.Basic.Util

type room_id = string
type exit_name = string

exception UnknownRoom of room_id
exception UnknownExit of exit_name

(** The abstract type of values that represents an exit. *)
type exit = { name : exit_name; exit_id : room_id; }

(** The abstract type of values that represents a room. *)
type room = { id : room_id; description : string; exits : exit list; }

type t = { rooms : room list; start : string; }

(******************* Begin helper functions. *******************)

(** [exit_of_json j] is the exit parsed from given Yjson value [j]. *)
let exit_of_json j = {
  name = j |> member "name" |> to_string;
  exit_id = j |> member "room id" |> to_string;
} 

(** [room_of_json j] is the room parsed from given Yjson value [j]. *)
let room_of_json j = {
  id = j |> member "id" |> to_string;
  description = j |> member "description" |> to_string;
  exits = j |> member "exits" |> to_list |> List.map exit_of_json;
}

(** [room_from_id id] is the room with [id] in a given [room list]. 
    Raises: [UnknownRoom id] if [id] not found. *)
let rec room_from_id id = function
  | [] -> raise (UnknownRoom id)
  | h :: t -> if h.id = id then h else room_from_id id t

(** [exit_from_id ex_id] is the exit with [ex_id] in a given [exit list]. 
    Raises: [UnknownExit ex_id] if [ex_id] not found. *)
let rec exit_from_id ex_id = function 
  | [] -> raise (UnknownExit ex_id)
  | h :: t -> if h.name = ex_id then h else exit_from_id ex_id t

(******************* End helper functions. *******************)

let from_json json = {
  rooms = json |> member "rooms" |> to_list |> List.map room_of_json;
  start = json |> member "start room" |> to_string;
}

let start_room adv =
  adv.start

let room_ids adv = 
  adv.rooms |> List.map (fun rm -> rm.id) |> List.sort_uniq compare

let description adv id =
  adv.rooms |> room_from_id id |> fun rm -> rm.description

let exits adv id = 
  adv.rooms |> room_from_id id |> (fun rm -> rm.exits) 
  |> List.map (fun ex -> ex.name) |> List.sort_uniq compare

let next_room adv rm_id ex_name =
  adv.rooms |> room_from_id rm_id |> (fun rm -> rm.exits)
  |> exit_from_id ex_name |> fun ex -> ex.exit_id

let next_rooms adv rm_id =
  adv.rooms |> room_from_id rm_id |> (fun rm -> rm.exits)
  |> List.map (fun ex -> ex.exit_id) |> List.sort_uniq compare
