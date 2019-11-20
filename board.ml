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
  module P : PieceSig
  type t = P.t option array
  val rows: int ref
  val init: int -> t
  val to_list: t -> (int * string) list
  val check: t -> int -> int -> int -> int -> bool
  val movable: t -> int list * int list
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

  (** [getState b i] is the piece at location [i].
      Throws exception if no piece is there.*)
  let getState b i =
    match b.(i) with
    | Some p -> p
    | None -> failwith "No piece at location"

  (* d stands for direction, 1 for left up, 2 for right up, 3 for right down,
       4 for left down. s for steps, jump takes 2, move takes 1.
       side represents side, 1 for red, 2 for black.*)
  (* input takes in ACTUAL POSITION (starting on 0)*)
  let check b d p s side=
    let p= p+ 1 in
    match d, (p - 1) / !rows, p mod !rows, s with
    | 1, 0, _, _
    | 1, _, 1, _
    | 1, 1, _, 2
    | 1, _, 2, 2 -> false
    | 2, 0, _, _
    | 2, _, 0, _
    | 2, 1, _, 2 -> false
    | 2, _, i, 2 when i = !rows - 1 -> false
    | 3, i, _, _ when i = !rows - 1 -> false
    | 3, _, 0, _ -> false
    | 3, i, _, 2 when i = !rows - 2 -> false
    | 3, _, i, 2 when i = !rows - 1 -> false
    | 4, i, _, _ when i = !rows - 1 -> false
    | 4, _, 1, _ -> false
    | 4, i, _, 2 when i = !rows - 2 -> false
    | 4, _, 2, 2 -> false
    | 1, _, _, 1 -> (
        match b.(p - !rows - 1 -1) with
        | None -> true
        | _ -> false
      )
    | 1, _, _, 2 -> (
        match b.(p - !rows - 1 - 1), b.(p - (2 * !rows) - 2 - 1) with
        | None, _ -> false
        | Some piece, Some piece2 -> false
        | Some piece, None -> P.side_of piece = (if side = 1 then P.Black else P.Red)
      )
    | 2, _, _, 1 ->  (
        match b.(p - !rows + 1 - 1) with
        | None -> true
        | _ -> false
      )
    | 2, _, _, 2 -> (
        match b.(p - !rows + 1 - 1), b.(p - (2 * !rows) + 2 - 1) with
        | None, _ -> false
        | Some piece, Some piece2 -> false
        | Some piece, None -> P.side_of piece = (if side = 1 then P.Black else P.Red)
      )
    | 3, _, _, 1 -> (
        match b.(p + !rows + 1 - 1) with
        | None -> true
        | _ -> false
      )
    | 3, _, _, 2 -> (
        match b.(p + !rows + 1 - 1), b.(p + (2 * !rows) + 2 - 1) with
        | None, _ -> false
        | Some piece, Some piece2 -> false
        | Some piece, None -> P.side_of piece = (if side = 1 then P.Black else P.Red)
      )
    | 4, _, _, 1 -> (
        match b.(p + !rows - 1 - 1) with
        | None -> true
        | _ -> false
      )
    | 4, _, _, 2 -> (
        match b.(p + !rows - 1 - 1), b.(p + (2 * !rows) - 2 - 1) with
        | None, _ -> false
        | Some piece, Some piece2 -> false
        | Some piece, None -> P.side_of piece = (if side = 1 then P.Black else P.Red)
      )
    | _ -> failwith "What"


  let movable b = 
    let rec checkboard i lst1 lst2= 
      if i = !rows * !rows then (lst1, lst2) else
        match b.(i) with
        | None -> checkboard (i+1) lst1 lst2
        | Some p -> if (P.side_of (getState b (!rows * !rows)) = P.side_of p) then (
            match P.side_of p, P.is_king p with
            | P.Red, false -> (
                match check b 3 i 1 1 || check b 4 i 1 1, check b 3 i 2 1 || check b 4 i 2 1 with
                | true, true -> checkboard (i+1) (i+1::lst1) (i+1::lst2)
                | true, false -> checkboard (i+1) (i+1::lst1) lst2
                | false, true -> checkboard (i+1) lst1 (i+1::lst2)
                | false, false -> checkboard (i+1) lst1 lst2
              )
            | P.Black, false -> (
                match check b 1 i 1 2 || check b 2 i 1 2, check b 1 i 2 2 || check b 2 i 2 2 with
                | true, true -> checkboard (i+1) (i+1::lst1) (i+1::lst2)
                | true, false -> checkboard (i+1) (i+1::lst1) lst2
                | false, true -> checkboard (i+1) lst1 (i+1::lst2)
                | false, false -> checkboard (i+1) lst1 lst2
              )
            | P.Red, true -> (
                match check b 1 i 1 1 || check b 2 i 1 1 || check b 3 i 1 1 || check b 4 i 1 1
                    , check b 1 i 2 1 || check b 2 i 2 1 || check b 3 i 2 1 || check b 4 i 2 1 with
                | true, true -> checkboard (i+1) (i+1::lst1) (i+1::lst2)
                | true, false -> checkboard (i+1) (i+1::lst1) lst2
                | false, true -> checkboard (i+1) lst1 (i+1::lst2)
                | false, false -> checkboard (i+1) lst1 lst2
              )
            | P.Black, true -> (
                match check b 1 i 1 2 || check b 2 i 1 2 || check b 3 i 1 2 || check b 4 i 1 2
                    , check b 1 i 2 2 || check b 2 i 2 2 || check b 3 i 2 2 || check b 4 i 2 2 with
                | true, true -> checkboard (i+1) (i+1::lst1) (i+1::lst2)
                | true, false -> checkboard (i+1) (i+1::lst1) lst2
                | false, true -> checkboard (i+1) lst1 (i+1::lst2)
                | false, false -> checkboard (i+1) lst1 lst2
              )
          )else checkboard (i+1) lst1 lst2 in

    checkboard 0 [] []

  (* p1 p2 are DISPLAY POSITIONS (starting on 1)*)
  let is_valid_move b p1 p2=
    match b.(p1 - 1) with
    | None -> false
    | Some p -> let side = (
        match P.side_of p with
        | P.Red -> 1
        | P.Black -> 2
      ) in
      match (p2 - p1) with
      | i when i = - !rows - 1 -> if (side = 1 && P.is_king p = false) then false else check b 1 (p1 - 1) 1 side
      | i when i = - !rows + 1 -> if (side = 1 && P.is_king p = false) then false else check b 2 (p1 - 1) 1 side
      | i when i = !rows + 1 -> if (side = 2 && P.is_king p = false) then false else check b 3 (p1 - 1) 1 side
      | i when i = !rows - 1 -> if (side = 2 && P.is_king p = false) then false else check b 4 (p1 - 1) 1 side
      | _ -> false

  (* p1 p2 are DISPLAY POSITION*)
  let is_valid_jump b p1 p2=
    match b.(p1 - 1) with
    | None -> false
    | Some p -> let side = (
        match P.side_of p with
        | P.Red -> 1
        | P.Black -> 2
      ) in
      match (p2 - p1) with
      | i when i = - 2 * !rows - 2 -> if (side = 1 && P.is_king p = false) then false else check b 1 (p1 - 1) 2 side
      | i when i = - 2 * !rows + 2 -> if (side = 1 && P.is_king p = false) then false else check b 2 (p1 - 1) 2 side
      | i when i = 2 * !rows + 2 -> if (side = 2 && P.is_king p = false) then false else check b 3 (p1 - 1) 2 side
      | i when i = 2 * !rows - 2 -> if (side = 2 && P.is_king p = false) then false else check b 4 (p1 - 1) 2 side
      | _ -> false

  (* p1 p2 are DISPLAY POSITION*)
  let move b p1 p2 = 
    let lst = (
      match movable b with
      | (l1, []) -> l1
      | (l1, l2) -> []
    ) in
    if is_valid_move b p1 p2 && (List.mem p1 lst) then (
      let p1 = p1 - 1 in
      let p2 = p2 - 1 in
      match b.(p1) with
      | None -> failwith "???"
      | Some p ->
        if (p2 / !rows = !rows - 1 && P.side_of p = P.Red) || (p2 / !rows = 0 && P.side_of p = P.Black) then
          (b.(p2) <- Some (P.create (P.side_of p) true); b.(p1) <- None;)
        else (b.(p2) <- b.(p1); b.(p1) <- None;)
    ) else print_string "invalid";()

  (* p1 p2 are DISPLAY POSITION*)
  let jump b p1 p2 =
    let lst = (
      match movable b with
      | (l1, l2) -> l2
    ) in
    if is_valid_jump b p1 p2 && (List.mem p1 lst) then 
      let p1 = p1 -1 in
      let p2 = p2 -1 in
      match b.(p1) with
      | None -> failwith "???"
      | Some p ->
        if (p2 / !rows = !rows - 1 && P.side_of p = P.Red) || (p2 / !rows = 0 && P.side_of p = P.Black) then
          (b.(p2) <- Some (P.create (P.side_of p) true); b.((p1+p2)/2) <- None;b.(p1) <- None;)
        else (b.(p2) <- b.(p1); b.((p1+p2)/2) <- None;b.(p1) <- None;)
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
    match movable b with
    | ([], []) -> change_turn b; true
    | _ -> false
end