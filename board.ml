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
  val rows: int ref
  val init: int -> t
  val to_list: t -> (int * string) list
  val movable: t -> int list
  val is_valid_move: t -> int -> int -> bool
  val is_valid_jump: t -> int -> int -> bool
  val move: t -> int -> int -> unit
  val jump: t -> int -> int -> unit
  val current_turn: t -> string
  val change_turn: t -> unit
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


  let rows = ref 8

  (** [initPieces a i num1 num2 num3] initializes [a] with pieces in original locations
      starting at [i] and black side before [num1] rows and red side between from
      [num2] + 1 row to [num3] row on a board of [row3] * [col].*)
  let rec initPieces a i num1 num2 num3 col= 
    if i < num1 * col then 
      match (i / col) mod 2 with
      | 0 -> a.(i) <- (if i mod 2 = 0 then None else Some (P.create Red false));
        initPieces a (i+1) num1 num2 num3 col
      | 1 -> a.(i) <- (if i mod 2 = 1 then None else Some (P.create Red false));
        initPieces a (i+1) num1 num2 num3 col
      | _ -> failwith "How could int mod 2 be greater than 2?"
    else if i >= num1 * col && i < num2 * col then 
      (a.(i) <- None; initPieces a (i+1) num1 num2 num3 col)
    else if i >= num2 * col && i < num3 * col then
      match (i / col) mod 2 with
      | 0 -> a.(i) <- (if i mod 2 = 0 then None else Some (P.create Black false));
        initPieces a (i+1) num1 num2 num3 col
      | 1 -> a.(i) <- (if i mod 2 = 1 then None else Some (P.create Black false));
        initPieces a (i+1) num1 num2 num3 col
      | _ -> failwith "How could int mod 2 be greater than 2?"
    else if i = num3 * col then (a.(i) <- Some (P.create Black false);a)
    else a

  let init r = 
    rows:= r;
    let a = Array.make (r*r+1) None in
    initPieces a 0 ((r-2)/2) ((r+2)/2) r r

  let to_list (b:t) =
    let rec toList b i r acc =
      if i < r * r then
        match b.(i) with
        | None -> toList b (i+1) r ((i+1,"Space") :: acc)
        | Some p -> (match P.side_of p with
            |Red -> toList b (i+1) r ((i+1, if P.is_king p then "Red King" else "Red") :: acc)
            |Black -> toList b (i+1) r ((i+1, if P.is_king p then "Black King" else "Black") :: acc)
          ) else 
        match b.(r*r) with
        |None -> (0, "WTF") ::acc
        |Some p -> (match P.side_of p with 
            |Red -> (0,"Current turn is Red") :: acc
            |Black -> (0, "Current turn is Black") :: acc)
    in
    List.rev (toList b 0 (!rows) [])

  let movable b = 
    failwith "unimplemented"

  let is_valid_move b p1 p2=
    true

  let is_valid_jump b p1 p2=
    true

  let move b p1 p2 =
    let p1 = p1 - 1 in
    let p2 = p2 - 1 in
    if is_valid_move b p1 p2 then (
      (b.(p2) <- b.(p1); b.(p1) <- None;)
    ) else print_string "invalid";()

  let jump b p1 p2 =
    let p1 = p1 -1 in
    let p2 = p2 -1 in
    if is_valid_jump b p1 p2 then 
      (b.(p2) <- b.(p1); b.((p1+p2)/2) <- None;b.(p1) <- None;)
    else print_string "invalid";()

  let current_turn b =
    match b.(!rows * !rows) with
    | None -> failwith "What"
    | Some p -> (match (P.side_of p) with
        | Red -> "Red"
        | Black -> "Black")

  let change_turn b =
    match current_turn b with
    | "Black" -> b.(!rows * !rows) <- Some (P.create Red false)
    | "Red" -> b.(!rows * !rows) <- Some (P.create Black false)
    | _ -> failwith "WHAT"

  let win b =
    failwith "unimplemented"
end