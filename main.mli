(** 
   The main entry point for the game interface.
*)

(** Converts an integer from 0 to 7 to a letter from a to h. For use in creating
    letters in coordinates. *)
val to_letter : int -> char

(** Takes in a board and a list of coordinates for a potential move. Makes the
    move if it is a valid move. Evaluates to a tuple of a string containing a
    possible error message and the board after (potentially) making a move.*)
val eval_move : Board.Board.t -> string list -> string * Board.Board.t

(** Converts a coordinate (e.g. ["h8"]) corresponding to that coordinate's
    number if each square were assigned to the integers in ascending order from
    1 to 64 going from left-to-right, top-to-bottom.*)
val coord_to_int : string -> int option

(** Forms a command out of two coordinate strings (e.g. ["c3"] and ["d4"]) to be
    interpreted by the Command module.*)
val command_of_coords : string-> string -> string