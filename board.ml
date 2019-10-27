module type PieceSig = sig
  type side = Red | Black
  type king = bool
  type t
  val rep_ok: t -> t
  val is_king: t -> bool
  val side_of: t -> side
  val create: side -> king -> t
end

module Piece = struct
  type side = Red | Black
  type king = bool
  type t = side * king

  let rep_ok p =
    p

  let is_king (p: t) =
    match p with
    | (s, k) -> k

  let side_of (p:t) =
    match p with
    | (s, k) -> s

  let create (s:side) (k:king) =
    (s, k)
end

module type BoardSig = sig
  type t
  module Piece : PieceSig
  type pos
  type turn
  val init: t
  val movable: t -> pos list
  val is_valid: t -> pos -> pos -> bool
  val move: t -> pos -> pos -> unit
  val capture: t -> pos -> unit
  val current_turn: t -> turn
  val win: t -> bool
end

module Board (Piece:PieceSig)= struct
  type t = unit

  type turn = unit

  type pos = unit

  let init = ()

  let movable b = []

  let is_valid b p1 p2= false

  let move b p1 p2 = ()

  let capture b p1 = ()

  let current_turn b = ()

  let win b =false
end