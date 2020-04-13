(**  *)

type color = Red | Blue

type cell = 
  {
    color:color;
    value:int;
  }

type board = cell list list

type t = 
  {
    board: board;
    player: color;
    score: int;
  }

let print b = 
  failwith "Unimplemented"

let get_cell_color c = 
  c.color

let get_cell_value c = 
  c.value

let check_win t = 
  failwith "Unimplemented"

let insert x y value b = 
  failwith "Unimplemented"

let tick_turn t = 
  if t.player = Red then 
    {
      board =  t.board;
      player = Blue;
      score =  t.score;
    }
  else 
    {
      board =  t.board;
      player = Red;
      score =  t.score;
    }

let score board = 
  failwith "Unimplemented"