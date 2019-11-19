open Board
open Command

(* allvalidmoves board is a list of all valid moves that the AI
   can make. All difficulties should call this function to determine
   what kind of move it can take. *)
let allvalidmoves board = 
  failwith "Unimplemented"

(* diff1 board is the helper function that will allow the AI
   to make a decision given input board that corresponds to 
   level 1 difficulty. This difficulty's AI makes random
   (but valid) moves. *)
let diff1 board = 
  failwith "Unimplemented"

(* diff2 board is the helper function that will allow the AI
   to make a decision given input board that corresponds to 
   level 2 difficulty. This difficulty allows the AI to avoid 
   making moves that would directly cause it to lose pieces (for
   example, the AI will not move its own pieces into a position
   where they can be taken unless those moves are the only valid ones.) *)
let diff2 board =
  failwith "Unimplemented"

(* make_move difficulty board is the move the AI takes with the input
   difficulty and the given board. Different difficulties
   correspond with and will call different helper functions
   to help the AI make its decision. *)
let make_move difficulty board =
  if difficulty = 1 then diff1 board else
  if difficulty = 2 then diff2 board else
    failwith "A difficulty specified in main is invalid."