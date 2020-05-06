(** The representation for an AI responder to play the game against. 

    This module contains functions to gather information about the state and 
    return responses of type Command.t to use in Main. 
*)

open Command
open State

(** The representation type for a response from the AI. *)
type response = command

(* TODO: add functions for obtaining information about the board here *)

(** Levels of difficulty of an AI. *)
type difficulty = 
  | Easy 
  | Hard 

val get_response: difficulty -> State.t -> response