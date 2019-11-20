open Board
open Command

(** [make_move difficulty board] is the move that the AI
    makes, corresponding with difficulty [difficulty] and board [board].
    The output is a move command. *)
val make_move : int -> Board.P.t option array-> int * int