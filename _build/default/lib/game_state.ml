open! Base

type t =
  | In_progress
  | Game_over of string
  | Win1
  | Win2
[@@deriving sexp_of, compare]

let to_string t =
  match t with
  | In_progress -> ""
  | Game_over x -> "Game over: " ^ x
  | Win1 -> "Snake1 WIN!"
  | Win2 -> "Snake2 WIN!"
;;
