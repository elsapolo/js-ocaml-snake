open! Base

type t =
  { mutable snake1 : Snake.t
  ; mutable snake2 : Snake.t
  ; mutable score1 : int
  ; mutable score2 : int
  ; mutable game_state : Game_state.t
  ; mutable apples : Apple.t * Apple.t
  ; board : Board.t
  }
[@@deriving sexp_of]

let to_string
    { snake1; snake2; score1; score2; game_state; apples = apple1, apple2; board }
  =
  Core.sprintf
    !{|Game state: %{sexp:Game_state.t}
Apple1: %{sexp:Apple.t}
Apple2: %{sexp:Apple.t}
Board: %{sexp:Board.t}
Snake1:
%s
score: %d
Snake2:
%s
score: %d |}
    game_state
    apple1
    apple2
    board
    (Snake.to_string ~indent:2 snake1)
    score1
    (Snake.to_string ~indent:2 snake2)
    score2
;;

let create ~height ~width ~initial_snake_length =
  let board = Board.create ~height ~width in
  let snake1 =
    Snake.create ~length:initial_snake_length ~height ~width ~direction:Direction.Right
  in
  let snake2 =
    Snake.create ~length:initial_snake_length ~height ~width ~direction:Direction.Left
  in
  let apples = Apple.create ~board ~snake1 ~snake2 in
  match apples with
  | None -> failwith "unable to create initial apples"
  | Some apples ->
    let t =
      { snake1; snake2; score1 = 0; score2 = 0; game_state = In_progress; apples; board }
    in
    if List.exists
         (Snake.all_locations snake1 @ Snake.all_locations snake2)
         ~f:(fun pos -> not (Board.in_bounds t.board pos))
    then failwith "unable to create initial snakes"
    else t
;;

let snake1 t = t.snake1
let snake2 t = t.snake2
let score1 t = t.score1
let score2 t = t.score2
let apples t = t.apples
let game_state t = t.game_state

(* Exercise 02b:

   Now, we're going to write a function that will be called whenver the user presses a
   key. For now, the only keys we care about are the ones that should cause the snake to
   change direction.

   To start, let's explore this module a little.

   This module represents the game state. Take a look at the type [t] at the top of this
   file. This is a record definition.

   A record is a data structure that allows you to group several pieces of data together.
   The names of the fields are on the left side of the : and the types of those fields are
   on the right side. By default record fields are immutable. The keyword mutable allows
   us to modify the value of that field.

   This record has 4 elements: a snake, a game_state, an apple, and a board. We'll explain
   each field when it's needed.

   Take a note of the [set_direction] function provided in snake.ml. Given a snake and a
   direction, this function will update the direction stored in the snake.

   Note the signature of this function:
   {[
     val set_direction : t -> Direction.t -> unit
   ]}

   The "unit" type is a special type that is returned by all side-effecting
   functions. This includes behaviors like printing or, as in this function, setting a
   mutable value.

   Let's use the [of_key] function we just wrote in direction.ml to get the direction the
   user intended and set it in the snake.

   The way that you reference the snake field in the record is with a '.' :
   {[
     t.snake
   ]}

   If the key wasn't a valid input key, our [of_key] function will return None. In that
   case, we have no action to take. Because we will use a match we will still need to
   specify what action to take in that case. To implement this, we once again use the
   "unit" type. I expect you will have the following case in your match statement:
   {[
     | None -> ()
   ]}

   Once you've implemented [handle_key], run

   $ dune runtest tests/exercise02b


   You should see no more failures.

   Now if you build and run the game again, you should be able to use the 'w', 'a', 's',
   and 'd' keys to control the snake.

   You may notice weird behavior if you run the snake off the game board. We'll handle
   collision behavior in the next exercise.

   Once you're done, go back to README.mkd for the next exercise.
*)
let handle_key t key =
  match Direction.of_key key with
  | Some (d, 1) -> Snake.set_direction t.snake1 d
  | Some (d, 2) -> Snake.set_direction t.snake2 d
  | _ -> ()
;;

(* Exercise 03b:

   Take a look at the definition of the [Game_state.t] type in game_state.mli. Do the
   three variants make sense?

   [check_for_collisons] will be called after the snake has been updated to walk forward
   one space. It should check to make sure the snake is still inside the bounds of
   the game board. If the snake is now out of bounds we want to update the game_state
   to note that fact that the game is now over.

   The in_bounds function you wrote in 03a was in board module, so you can access it with
   Board.in_bounds

   If there is a collison we should set the game_state of t to be
   {[
     Game_over "Out of bounds!"
   ]}

   The way that you set a mutable record value is with the "<-" operator. For example:

   {[
     type t = { mutable counter : int }

     let increment_counter t =
       t.counter <- t.counter + 1
     ;;
   ]}

   [Snake.head] is a function we've provided for you that returns a Position.t
   representing the head of the snake.
   {[
     val head : Snake.t -> Position.t
   ]}

   Once you have implemented [check_for_collisions],

   $ dune runtest tests/exercise03b

   should have no output.

   Return to README.mkd for instructions on exercise 04.
*)
let check_for_collisions t =
  match
    (Board.in_bounds t.board) (Snake.head t.snake1)
    && (Board.in_bounds t.board) (Snake.head t.snake2)
  with
  | true -> t.game_state <- Game_state.In_progress
  | false -> t.game_state <- Game_state.Game_over "Snake out of bounds!"
;;

(* Exercise 06b:

   Every time the snake steps forward, [maybe_consume_apple] should be called.
   It should check if the snake head is at the current location of the apple
   stored in the game.

   Hint: We've given you some functions to help with this.  Take a look at snake.mli and
   apple.mli to find functions to get the locations you need to consider

   If it is, we should call the [grow_over_next_steps] function in snake.ml that we just
   implemented to update the snake so that it can grow over the next few time steps. The
   amount it should grow is based on the value of [Apple.amount_to_grow].

   If the apple is consumed, we should also spawn a new apple on the board using the
   function you implemented in exercise 05, [Apple.create].

   Recall that if [Apple.create] returns None, that means that we have won the game, so we
   should update the game_state to reflect that.

*)

let maybe_consume_apple t =
  let snake1 = t.snake1 in
  let snake2 = t.snake2 in
  let apple1, apple2 = t.apples in
  let snake1_apple1 = Position.equal (Snake.head snake1) (Apple.location apple1) in
  let snake2_apple2 = Position.equal (Snake.head snake2) (Apple.location apple2) in
  let snake1_apple2 = Position.equal (Snake.head snake1) (Apple.location apple2) in
  let snake2_apple1 = Position.equal (Snake.head snake2) (Apple.location apple1) in
  if snake1_apple1
  then (
    Snake.grow_over_next_steps snake1 (Apple.amount_to_grow apple1);
    t.score1 <- t.score1 + Apple.amount_to_grow apple1);
  if snake2_apple2
  then (
    Snake.grow_over_next_steps snake2 (Apple.amount_to_grow apple2);
    t.score2 <- t.score2 + Apple.amount_to_grow apple2);
  if snake1_apple1 || snake2_apple2
  then (
    match Apple.create ~board:t.board ~snake1:t.snake1 ~snake2:t.snake2 with
    | None -> t.game_state <- Game_state.Win2
    | Some a -> t.apples <- a);
  if snake1_apple2 || snake2_apple1
  then (
    Stdio.print_endline "snake ate wrong apple";
    t.game_state <- Game_state.Game_over "WhO Ate tHE WrONg ApPle?!?")
;;

(* Exercise 04b:

   [step] is the function that is called in a loop to make the game progress. As you can
   see, we have provided part of this for you.

   [step] returns false if the snake collided with itself, and true if the game can
   continue.

   We've already handled the case where the value is true, but when the value is false, we
   currently do nothing.

   Modify this function to set the [game_state] field of the game with the message
   "Self collision!".

   When all the tests for exercise 04 pass, return to README.mkd for exercise 05. *)
let step t =
  if Snake.step t.snake1 t.snake2 && Snake.step t.snake2 t.snake1
  then (
    check_for_collisions t;
    maybe_consume_apple t)
  else t.game_state <- Game_state.Game_over "Collision!"
;;
