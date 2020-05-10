(** The representation for an AI responder to play the game against. 

    This module contains functions to gather information about the state and 
    return responses of type Command.t to use in Main. 
*)

open Command
open State

(** The representation type for a response from the AI. *)
type response = command

(** Levels of difficulty of an AI. *)
type difficulty = 
  | Easy 
  | Hard 

(** [get_response d st] is the CPU's command based on difficulty [d]. 
    - if [d] = Easy, [get_response] returns an unintelligent and random response 
      using little contextual understanding of the board 
    - if [d] = Hard, [get_response] returns a command based on an evaluation
      of all possible insertion and rotation commands in state [st]. *)
val get_response: difficulty -> State.t -> response