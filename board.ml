module type PieceSig = sig
  type side = Red | Black
  type king = bool
  type t
  val rep_ok: t -> t
  val is_king: t -> bool
  val side_of: t -> side
  val create: side -> king -> t
end

module type BoardSig = sig
  type t
  module P : PieceSig
  type pos
  type turn
  val rows: int ref
  val init: int -> t
  val movable: t -> pos list
  val is_valid: t -> pos -> pos -> bool
  val move: t -> pos -> pos -> unit
  val capture: t -> pos -> unit
  val current_turn: t -> turn
  val win: t -> bool
end


module Board= struct

  module P = struct
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

  type t = P.t option array

  type turn = P.side

  type pos = int

  let rows = ref 8

  (** [initPieces a i num1 num2 num3] initializes [a] with pieces in original locations
      starting at [i] and black side before [num1] rows and red side between from
      [num2] + 1 row to [num3] row on a board of [row3] * [col].*)
  let rec initPieces a i num1 num2 num3 col= 
    if i < num1 * col then 
      match (i / col) mod 2 with
      | 0 -> a.(i) <- (if i mod 2 = 0 then None else Some (P.create P.Red false));
        initPieces a (i+1) num1 num2 num3 col
      | 1 -> a.(i) <- (if i mod 2 = 1 then None else Some (P.create P.Red false));
        initPieces a (i+1) num1 num2 num3 col
      | _ -> failwith "How could int mod 2 be greater than 2?"
    else if i >= num1 * col && i < num2 * col then 
      (a.(i) <- None; initPieces a (i+1) num1 num2 num3 col)
    else if i >= num2 * col && i < num3 * col then
      match (i / col) mod 2 with
      | 0 -> a.(i) <- (if i mod 2 = 0 then None else Some (P.create P.Black false));
        initPieces a (i+1) num1 num2 num3 col
      | 1 -> a.(i) <- (if i mod 2 = 1 then None else Some (P.create P.Black false));
        initPieces a (i+1) num1 num2 num3 col
      | _ -> failwith "How could int mod 2 be greater than 2?"
    else a

  let init r = 
    rows:= r;
    let a = Array.make 64 None in
    initPieces a 0 3 5 r r

  let to_list (b:t) =
    failwith "unimplemented"


  let movable b = 
    failwith "unimplemented"

  let is_valid b p1 p2=
    failwith "unimplemented"

  let move b p1 p2 =
    failwith "unimplemented"

  let capture b p1 =
    failwith "unimplemented"

  let current_turn b =
    failwith "unimplemented"

  let win b =
    failwith "unimplemented"
end