open! Base

module Color = struct
  type t = Green | Blue [@@deriving sexp_of]
end

type t = { location : Position.t; color: Color.t} [@@deriving sexp_of]

let location t = t.location

let color t =
  t.color;
;;

let amount_to_grow t = 2


(* Exercise 05:

   [create] takes in a board and a snake and creates a new apple with a location chosen
   randomly from all possible locations inside the board that are not currently occupied
   by the snake. If there is no location possible it should return None. (This will
   happen if the player wins!)

   We have used records before, but creating the apples location is the first time we'll
   need to make a new one. We can define a record like so:

   {[
     { field_name1 = value1
     ; field_name2 = value2
     }
   ]}

   One function you might find handy for this exercise is [Board.all_locations], which is
   defined in board.ml. [Board.all_locations] takes a board and returns a list of all
   positions on the board.

   Another function that may be useful is [Snake.all_locations], which is defined in
   snake.ml. This function takes a snake and returns all locations it currently occupies.

   Before you get started, check out LIST_FUNCTIONS.mkd for some useful new List
   functions. You probably won't need all of these functions, but they should give you a
   few options for how to write [create].

   You may feel like your solution to this function is inefficient. That's perfectly fine,
   since the board is quite small. Talk to a TA if you'd like to learn other tools that
   might help make this function more efficient.

   When you're done writing [create], make sure to check that tests pass by running

   $ dune runtest tests/exercise05 *)
let create ~board ~snake1 ~snake2 =
  let posNotIn list x = not (List.mem list x ~equal:(Position.equal)) in
  let notSnake1 x = posNotIn (Snake.all_locations snake1) x in
  let notSnake2 x = posNotIn (Snake.all_locations snake2) x in
  let possibleApples1 = List.filter (Board.all_locations board) ~f:(fun x -> (notSnake1 x && notSnake2 x)) in
  match possibleApples1 with
  | [_] | []-> None 
  | l -> let apple1 = List.random_element_exn l in
         let possibleApples2 = List.filter l ~f:(fun x -> not (Position.equal x apple1)) in
         let apple2 = List.random_element_exn possibleApples2 in
         Some ({location = apple1; color = Color.Green},
         {location = apple2; color = Color.Blue})

;;

