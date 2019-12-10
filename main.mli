val to_letter : int -> char

(** 
   The main entry point for the game interface. Since none of the other
   files require any of the information that this file holds as of right
   now, this file is left empty. If at some point this changes, code that
   exposes info in main.ml should be prescribed here.
*)
val eval_move : Board.Board.t -> string list -> string * Board.Board.t