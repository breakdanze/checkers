
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

  module P : PieceSig
  type t = P.t option array

  (** [rows] is the # of rows and columns of the board.*)
  val rows: int ref

  (** [init] is the init state of board.*)
  val init: int -> t

  (** [to_list b] is the list representation of [b].*)
  val to_list: t -> (int * string) list

  (** [check b d p s side] is true iff [p] with side [side] is possible to be
      moved towards [d] in [s] steps.*)
  val check: t -> int -> int -> int -> int -> bool

  (** [movable b] is the list containing two lists, one for movable positions on
      board [b], the other for jumpable list.*)
  val movable: t -> int list * int list

  (** [is_valid_move b pos1 pos2] is true iff [pos2] is a valid move of the
      piece at [pos1] on board [b].*)
  val is_valid_move: t -> int -> int -> bool

  (** [is_valid_jump b pos1 pos2] is true iff [pos2] is a valid jump of the
      piece at [pos1] on board [b].*)
  val is_valid_jump: t -> int -> int -> bool

  (** [move pos1 pos2] moves the piece at [pos1] to [pos2] on board [b].*)
  val move: t -> int -> int -> unit

  (** [jump pos1 pos2] jumps the piece at [pos1] to [pos2] on board [b].*)
  val jump: t -> int -> int -> unit

  (** [current_turn b] is the current turn of [b].*)
  val current_turn: t -> string

  (** [change_turn b] changes the current turn of [b] to the opposite side.*)
  val change_turn: t -> unit

  (** [win b] is true iff one side of [b] has won.*)
  val win: t -> bool
end

module Board : BoardSig