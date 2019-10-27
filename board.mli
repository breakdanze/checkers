
module type PieceSig = sig
  type side = Red | Black
  type king = bool
  type t

  (** [rep_ok p] returns [p] if it satisfies the representation invariants,
      otherwise throw an exception.*)
  val rep_ok: t -> t


  (** [is_king p] is true iff [p] is king*)
  val is_king: t -> bool

  (** [side_of p] is the side of [p].*)
  val side_of: t -> side

  (** [create s k] creates a piece with side [s] and king [k].*)
  val create: side -> king -> t
end


module type BoardSig = sig
  type t

  module Piece : PieceSig
  (** [pos] is the index for the board.*)
  type pos

  (** [turn] indicates the current player's side.*)
  type turn

  (** [init] is the init state of board.*)
  val init: t

  (** [movable b] is the list of movable positions on board [b].*)
  val movable: t -> pos list

  (** [is_valid b pos1 pos2] is true iff [pos2] is a valid move of the piece at 
      [pos1] on board [b].*)
  val is_valid: t -> pos -> pos -> bool

  (** [move pos1 pos2] moves the piece at [pos1] to [pos2] on board [b].*)
  val move: t -> pos -> pos -> unit

  (** [capture b pos] captures the piece at [pos] on board [b].*)
  val capture: t -> pos -> unit

  (** [current_turn b] is the current turn of [b].*)
  val current_turn: t -> turn

  (** [win b] is true iff one side of [b] has won.*)
  val win: t -> bool
end

module type BoardMaker = functor (P:PieceSig) -> BoardSig with module Piece = P

module Board : BoardMaker