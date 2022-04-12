open! Base

(** A [t] represents the entire game state, including the current snake, apple,
    and game state. *)
type t [@@deriving sexp_of]

(** Used for pretty-printing game contents for tests. *)
val to_string : t -> string

(** [create] creates a new game with specified parameters. *)
val create : height:int -> width:int -> initial_snake_length:int -> t

(** [snake] returns the snake1 that is currently in the game. *)
val snake1 : t -> Snake.t

(** [snake] returns the snake2 that is currently in the game. *)
val snake2 : t -> Snake.t

(** [snake] returns the apples that is currently in the game. *)
val apples : t -> Apple.t * Apple.t

(** [snake] returns the score1 that is currently in the game. *)
val score1 : t -> int

(** [snake] returns the score2 that is currently in the game. *)
val score2 : t -> int

(** [handle_key] will be called whenever the user presses a key.  It takes that key and
    updates the game accordingly *)
val handle_key : t -> char -> unit

(** [game_state] returns the state of the current game. *)
val game_state : t -> Game_state.t

(** [step] is called in a loop, and the game is re-rendered after each call. *)
val step : t -> unit

