open Board
open Command

let kingcheck completeboard piecenumber = 
  match (List.nth completeboard piecenumber) with
  | None -> failwith "This shouldn't be possible."
  | Some p -> Board.P.is_king p

let sidecheck completeboard piecenumber = 
  match (List.nth completeboard piecenumber) with
  | None -> failwith "This shouldn't be possible."
  | Some p -> Board.P.side_of p

let arraytolist (board : Board.t) =
  Array.to_list board

let rec redboundaryvalidmoves piece completeboard piecenumber movenumber = 
  if (Board.P.is_king piece) = false then  
    if piecenumber > 55 then [] else 
    if piecenumber mod 8 = 0 then 
      if List.nth completeboard (piecenumber+9) = None then 
        (piecenumber, piecenumber+9) :: [] else
      if piecenumber < 48 && List.nth completeboard (piecenumber+18) = None && 
         sidecheck completeboard (piecenumber+9) = Black then
        (piecenumber, piecenumber+18) :: [] else [] else
    if piecenumber mod 8 = 7 then
      if List.nth completeboard (piecenumber+7) = None then 
        (piecenumber, piecenumber+7) :: [] else
      if piecenumber < 48 && List.nth completeboard (piecenumber+14) = None &&
         sidecheck completeboard (piecenumber+7) = Black then
        (piecenumber, piecenumber+14) :: [] else [] 
    else
    if movenumber = 0 then
      if List.nth completeboard (piecenumber+7) = None then 
        (piecenumber, piecenumber+7) :: redboundaryvalidmoves piece 
          completeboard piecenumber (movenumber+1) else
      if List.nth completeboard (piecenumber+14) = None &&
         sidecheck completeboard (piecenumber+7) = Black then
        (piecenumber, piecenumber+14) :: redboundaryvalidmoves piece 
          completeboard piecenumber (movenumber+1) else []
    else
    if movenumber = 1 then 
      if List.nth completeboard (piecenumber+9) = None then 
        (piecenumber, piecenumber+9) :: [] else
      if List.nth completeboard (piecenumber+18) = None && 
         sidecheck completeboard (piecenumber+9) = Black then
        (piecenumber, piecenumber+18) :: [] else [] else []
  else
  if (Board.P.is_king piece) then 
    if piecenumber mod 8 = 0 then
      if movenumber = 0 then
        if piecenumber > 7 && List.nth completeboard (piecenumber-7) = None
        then (piecenumber, piecenumber-7) :: redboundaryvalidmoves piece 
               completeboard piecenumber (movenumber+1) else
        if piecenumber > 15 && List.nth completeboard (piecenumber-14) = None && 
           sidecheck completeboard (piecenumber-7) = Black then
          (piecenumber, piecenumber-14) :: redboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else [] else 
      if movenumber = 1 then
        if piecenumber < 56 && List.nth completeboard (piecenumber+9) = None
        then (piecenumber, piecenumber+9) :: [] else
        if piecenumber < 48 && List.nth completeboard (piecenumber+18) = None &&
           sidecheck completeboard (piecenumber+9) = Black then
          (piecenumber, piecenumber+18) :: [] else [] else [] else
    if piecenumber mod 8 = 7 then
      if movenumber = 0 then
        if piecenumber < 56 && List.nth completeboard (piecenumber+7) = None
        then (piecenumber, piecenumber+7) :: redboundaryvalidmoves piece 
               completeboard piecenumber (movenumber+1) else
        if piecenumber < 48 && List.nth completeboard (piecenumber+14) = None && 
           sidecheck completeboard (piecenumber+7) = Black then
          (piecenumber, piecenumber+14) :: redboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else [] else 
      if movenumber = 1 then
        if piecenumber > 7 && List.nth completeboard (piecenumber-9) = None then 
          (piecenumber, piecenumber-9) :: [] else
        if piecenumber > 15 && List.nth completeboard (piecenumber-18) = None &&
           sidecheck completeboard (piecenumber-9) = Black then
          (piecenumber, piecenumber-18) :: [] else [] else [] else
    if piecenumber < 8 then 
      if movenumber = 0 then
        if List.nth completeboard (piecenumber+7) = None then 
          (piecenumber, piecenumber+7) :: redboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else
        if List.nth completeboard (piecenumber+14) = None &&
           sidecheck completeboard (piecenumber+7) = Black then
          (piecenumber, piecenumber+14) :: redboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else []
      else
      if movenumber = 1 then 
        if List.nth completeboard (piecenumber+9) = None then 
          (piecenumber, piecenumber+9) :: [] else
        if List.nth completeboard (piecenumber+18) = None && 
           sidecheck completeboard (piecenumber+9) = Black then
          (piecenumber, piecenumber+18) :: [] else [] else [] else
    if piecenumber > 55 then 
      if movenumber = 0 then
        if List.nth completeboard (piecenumber-7) = None then 
          (piecenumber, piecenumber-7) :: redboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else
        if List.nth completeboard (piecenumber-14) = None &&
           sidecheck completeboard (piecenumber-7) = Black then
          (piecenumber, piecenumber-14) :: redboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else []
      else
      if movenumber = 1 then 
        if List.nth completeboard (piecenumber-9) = None then 
          (piecenumber, piecenumber-9) :: [] else
        if List.nth completeboard (piecenumber-18) = None && 
           sidecheck completeboard (piecenumber-9) = Black then
          (piecenumber, piecenumber-18) :: [] else [] else [] else
      []
  else
    []

let rec redvalidmoves piece completeboard piecenumber movenumber =
  if (piecenumber mod 8 = 0) || (piecenumber mod 8 = 7 || 
                                 piecenumber < 8 || piecenumber > 55) then 
    redboundaryvalidmoves piece completeboard piecenumber 0 else
  if (Board.P.is_king piece) = false then 
    if movenumber = 0 then 
      if List.nth completeboard (piecenumber+7) = None then 
        (piecenumber, piecenumber+7) :: redvalidmoves piece 
          completeboard piecenumber (movenumber+1) else
      if piecenumber < 50 && piecenumber mod 8 <> 1 && 
         List.nth completeboard (piecenumber+14) = None && 
         sidecheck completeboard (piecenumber+7) = Black then
        (piecenumber, piecenumber+14) :: redvalidmoves piece 
          completeboard piecenumber (movenumber+1) else 
        redvalidmoves piece completeboard piecenumber (movenumber+1) else
    if movenumber = 1 then
      if List.nth completeboard (piecenumber+9) = None then 
        (piecenumber, piecenumber+9) :: [] else
      if piecenumber < 46 && piecenumber mod 8 <> 6 && 
         List.nth completeboard (piecenumber+18) = None && 
         sidecheck completeboard (piecenumber+9) = Black then
        (piecenumber, piecenumber+18) :: [] else 
        [] else []
  else 
  if movenumber = 0 then 
    if List.nth completeboard (piecenumber+7) = None then 
      (piecenumber, piecenumber+7) :: redvalidmoves piece 
        completeboard piecenumber (movenumber+1) else
    if piecenumber < 50 && piecenumber mod 8 <> 1 && 
       List.nth completeboard (piecenumber+14) = None && 
       sidecheck completeboard (piecenumber+7) = Black then
      (piecenumber, piecenumber+14) :: redvalidmoves piece 
        completeboard piecenumber (movenumber+1) else 
      redvalidmoves piece completeboard piecenumber (movenumber+1) else
  if movenumber = 1 then 
    if List.nth completeboard (piecenumber+9) = None then 
      (piecenumber, piecenumber+9) :: redvalidmoves piece 
        completeboard piecenumber (movenumber+1) else
    if piecenumber < 46 && piecenumber mod 8 <> 6 && 
       List.nth completeboard (piecenumber+18) = None && 
       sidecheck completeboard (piecenumber+9) = Black then
      (piecenumber, piecenumber+18) :: redvalidmoves piece 
        completeboard piecenumber (movenumber+1) else 
      redvalidmoves piece completeboard piecenumber (movenumber+1) else
  if movenumber = 2 then 
    if List.nth completeboard (piecenumber-7) = None then 
      (piecenumber, piecenumber-7) :: redvalidmoves piece 
        completeboard piecenumber (movenumber+1) else
    if piecenumber > 13 && piecenumber mod 8 = 6 && 
       List.nth completeboard (piecenumber-14) = None && 
       sidecheck completeboard (piecenumber-7) = Black then
      (piecenumber, piecenumber-14) :: redvalidmoves piece 
        completeboard piecenumber (movenumber+1) else 
      redvalidmoves piece completeboard piecenumber (movenumber+1) else
  if movenumber = 3 then 
    if List.nth completeboard (piecenumber-9) = None then 
      (piecenumber, piecenumber-9) :: [] else
    if piecenumber > 17 && piecenumber mod 8 <> 1 && 
       List.nth completeboard (piecenumber-18) = None && 
       sidecheck completeboard (piecenumber-9) = Black then
      (piecenumber, piecenumber-18) :: [] else 
      [] else []

let rec blackboundaryvalidmoves piece completeboard piecenumber movenumber =
  if (Board.P.is_king piece) = false then  
    if piecenumber < 8 then [] else 
    if piecenumber mod 8 = 0 then 
      if List.nth completeboard (piecenumber-7) = None then 
        (piecenumber, piecenumber-7) :: [] else
      if piecenumber > 15 && List.nth completeboard (piecenumber-14) = None && 
         sidecheck completeboard (piecenumber-7) = Red then
        (piecenumber, piecenumber-14) :: [] else [] else
    if piecenumber mod 8 = 7 then
      if List.nth completeboard (piecenumber-9) = None then 
        (piecenumber, piecenumber-9) :: [] else
      if piecenumber > 15 && List.nth completeboard (piecenumber-18) = None && 
         sidecheck completeboard (piecenumber-9) = Red then
        (piecenumber, piecenumber-18) :: [] else [] 
    else
    if movenumber = 0 then
      if List.nth completeboard (piecenumber-7) = None then 
        (piecenumber, piecenumber-7) :: blackboundaryvalidmoves piece 
          completeboard piecenumber (movenumber+1) else
      if List.nth completeboard (piecenumber-14) = None &&
         sidecheck completeboard (piecenumber-7) = Red then
        (piecenumber, piecenumber-14) :: blackboundaryvalidmoves piece 
          completeboard piecenumber (movenumber+1) else []
    else
    if movenumber = 1 then 
      if List.nth completeboard (piecenumber-9) = None then 
        (piecenumber, piecenumber-9) :: [] else
      if List.nth completeboard (piecenumber-18) = None && 
         sidecheck completeboard (piecenumber-9) = Red then
        (piecenumber, piecenumber-18) :: [] else [] else []
  else
  if (Board.P.is_king piece) then 
    if piecenumber mod 8 = 0 then
      if movenumber = 0 then
        if piecenumber > 7 && List.nth completeboard (piecenumber-7) = None then 
          (piecenumber, piecenumber-7) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else
        if piecenumber > 15 && List.nth completeboard (piecenumber-14) = None && 
           sidecheck completeboard (piecenumber-7) = Red then
          (piecenumber, piecenumber-14) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else [] else 
      if movenumber = 1 then
        if piecenumber < 56 && List.nth completeboard (piecenumber+9) = None
        then (piecenumber, piecenumber+9) :: [] else
        if piecenumber < 48 && List.nth completeboard (piecenumber+18) = None && 
           sidecheck completeboard (piecenumber+9) = Red then
          (piecenumber, piecenumber+18) :: [] else [] else [] else
    if piecenumber mod 8 = 7 then
      if movenumber = 0 then
        if piecenumber < 56 && List.nth completeboard (piecenumber+7) = None
        then (piecenumber, piecenumber+7) :: blackboundaryvalidmoves piece 
               completeboard piecenumber (movenumber+1) else
        if piecenumber < 48 && List.nth completeboard (piecenumber+14) = None && 
           sidecheck completeboard (piecenumber+7) = Red then
          (piecenumber, piecenumber+14) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else [] else 
      if movenumber = 1 then
        if piecenumber > 7 && List.nth completeboard (piecenumber-9) = None then 
          (piecenumber, piecenumber-9) :: [] else
        if piecenumber > 15 && List.nth completeboard (piecenumber-18) = None && 
           sidecheck completeboard (piecenumber-9) = Red then
          (piecenumber, piecenumber-18) :: [] else [] else [] else
    if piecenumber < 8 then 
      if movenumber = 0 then
        if List.nth completeboard (piecenumber+7) = None then 
          (piecenumber, piecenumber+7) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else
        if List.nth completeboard (piecenumber+14) = None &&
           sidecheck completeboard (piecenumber+7) = Red then
          (piecenumber, piecenumber+14) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else []
      else
      if movenumber = 1 then 
        if List.nth completeboard (piecenumber+9) = None then 
          (piecenumber, piecenumber+9) :: [] else
        if List.nth completeboard (piecenumber+18) = None && 
           sidecheck completeboard (piecenumber+9) = Red then
          (piecenumber, piecenumber+18) :: [] else [] else [] else
    if piecenumber > 55 then 
      if movenumber = 0 then
        if List.nth completeboard (piecenumber-7) = None then 
          (piecenumber, piecenumber-7) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else
        if List.nth completeboard (piecenumber-14) = None &&
           sidecheck completeboard (piecenumber-7) = Red then
          (piecenumber, piecenumber-14) :: blackboundaryvalidmoves piece 
            completeboard piecenumber (movenumber+1) else []
      else
      if movenumber = 1 then 
        if List.nth completeboard (piecenumber-9) = None then 
          (piecenumber, piecenumber-9) :: [] else
        if List.nth completeboard (piecenumber-18) = None && 
           sidecheck completeboard (piecenumber-9) = Red then
          (piecenumber, piecenumber-18) :: [] else [] else [] else
      []
  else
    []

let rec blackvalidmoves piece completeboard piecenumber movenumber = 
  if (piecenumber mod 8 = 0) || (piecenumber mod 8 = 7 || 
                                 piecenumber < 8 || piecenumber > 55) then 
    blackboundaryvalidmoves piece completeboard piecenumber 0 else
  if (Board.P.is_king piece) = false then 
    if movenumber = 0 then 
      if List.nth completeboard (piecenumber-7) = None then 
        (piecenumber, piecenumber-7) :: blackvalidmoves piece 
          completeboard piecenumber (movenumber+1) else
      if piecenumber > 13 && piecenumber mod 8 = 6 && 
         List.nth completeboard (piecenumber-14) = None && 
         sidecheck completeboard (piecenumber-7) = Red then
        (piecenumber, piecenumber-14) :: blackvalidmoves piece 
          completeboard piecenumber (movenumber+1) else 
        blackvalidmoves piece completeboard piecenumber (movenumber+1) else
    if movenumber = 1 then 
      if List.nth completeboard (piecenumber-9) = None then 
        (piecenumber, piecenumber-9) :: [] else
      if piecenumber > 17 && piecenumber mod 8 <> 1 && 
         List.nth completeboard (piecenumber-18) = None && 
         sidecheck completeboard (piecenumber-9) = Red then
        (piecenumber, piecenumber-18) :: [] else 
        [] else []
  else 
  if movenumber = 0 then 
    if List.nth completeboard (piecenumber+7) = None then 
      (piecenumber, piecenumber+7) :: blackvalidmoves piece 
        completeboard piecenumber (movenumber+1) else
    if piecenumber < 50 && piecenumber mod 8 <> 1 && 
       List.nth completeboard (piecenumber+14) = None && 
       sidecheck completeboard (piecenumber+7) = Red then
      (piecenumber, piecenumber+14) :: blackvalidmoves piece 
        completeboard piecenumber (movenumber+1) else 
      blackvalidmoves piece completeboard piecenumber (movenumber+1) else
  if movenumber = 1 then 
    if List.nth completeboard (piecenumber+9) = None then 
      (piecenumber, piecenumber+9) :: blackvalidmoves piece 
        completeboard piecenumber (movenumber+1) else
    if piecenumber < 46 && piecenumber mod 8 <> 6 && 
       List.nth completeboard (piecenumber+18) = None && 
       sidecheck completeboard (piecenumber+9) = Red then
      (piecenumber, piecenumber+18) :: blackvalidmoves piece 
        completeboard piecenumber (movenumber+1) else 
      blackvalidmoves piece completeboard piecenumber (movenumber+1) else
  if movenumber = 2 then 
    if List.nth completeboard (piecenumber-7) = None then 
      (piecenumber, piecenumber-7) :: blackvalidmoves piece 
        completeboard piecenumber (movenumber+1) else
    if piecenumber > 13 && piecenumber mod 8 = 6 && 
       List.nth completeboard (piecenumber-14) = None && 
       sidecheck completeboard (piecenumber-7) = Red then
      (piecenumber, piecenumber-14) :: blackvalidmoves piece 
        completeboard piecenumber (movenumber+1) else 
      blackvalidmoves piece completeboard piecenumber (movenumber+1) else
  if movenumber = 3 then 
    if List.nth completeboard (piecenumber-9) = None then 
      (piecenumber, piecenumber-9) :: [] else
    if piecenumber > 17 && piecenumber mod 8 <> 1 && 
       List.nth completeboard (piecenumber-18) = None && 
       sidecheck completeboard (piecenumber-9) = Red then
      (piecenumber, piecenumber-18) :: [] else 
      [] else []

let lastpiecesidecheck completeboard = 
  match (List.nth completeboard 64) with
  | None -> failwith "This shouldn't be possible."
  | Some p -> Board.P.side_of p

let rec validmovelistmaker
    boardlist (completeboard : 'a option list) piecenumber =
  if piecenumber = 64 then [] else
    match boardlist with
    | [] -> []
    | None :: t -> validmovelistmaker t completeboard (piecenumber+1)
    | Some p :: t -> if (Board.P.side_of p = 
                         lastpiecesidecheck completeboard &&
                         Board.P.side_of p = Red) then 
        redvalidmoves p completeboard piecenumber 0 :: validmovelistmaker t 
          completeboard (piecenumber+1) else
      if (Board.P.side_of p = lastpiecesidecheck completeboard &&
          Board.P.side_of p = Black) then 
        blackvalidmoves p completeboard piecenumber 0 :: validmovelistmaker t 
          completeboard (piecenumber+1) else 
        validmovelistmaker t completeboard (piecenumber+1)

let allvalidmoves (board : Board.t) = 
  List.flatten (validmovelistmaker (arraytolist board) (arraytolist board) 0)

let rec allvalidjumps validmoves =
  match validmoves with
  | [] -> []
  | (p, n) :: t -> if ((p - n) = 14 || (p - n) = 18 || 
                       (p - n) = -14 || (p - n) = -18) then 
      (p, n) :: allvalidjumps t 
    else
      allvalidjumps t

let safemove n boardlist =
  if sidecheck boardlist 64 = Red then
    if (List.nth boardlist (n+7) = None || 
        sidecheck boardlist (n+7) = Red) = false then false else
    if (List.nth boardlist (n+9) = None || 
        sidecheck boardlist (n+9) = Red) = false then false else
    if (List.nth boardlist (n-7) = None || 
        sidecheck boardlist (n-7) = Red || 
        kingcheck boardlist (n-7) = false) = false then false else
    if (List.nth boardlist (n-9) = None || 
        sidecheck boardlist (n-9) = Red || 
        kingcheck boardlist (n-9) = false) = false then false else true
  else
  if sidecheck boardlist 64 = Black then
    if (List.nth boardlist (n-7) = None || 
        sidecheck boardlist (n-7) = Black) = false then false else
    if (List.nth boardlist (n-9) = None || 
        sidecheck boardlist (n-9) = Black) = false then false else
    if (List.nth boardlist (n+7) = None || 
        sidecheck boardlist (n+7) = Black || 
        kingcheck boardlist (n+7) = false) = false then false else
    if (List.nth boardlist (n+9) = None || 
        sidecheck boardlist (n+9) = Black || 
        kingcheck boardlist (n+9) = false) = false then false else true 
  else
    failwith "This shouldn't be possible."

let rec allsafemoves (board : Board.t) validmoves = 
  let boardlist = arraytolist board in
  match validmoves with
  | [] -> []
  | (p, n) :: t -> if safemove n boardlist then 
      (p, n) :: allsafemoves board t 
    else allsafemoves board t 

let allsafejumps board validmoves =
  let safemoves = allsafemoves board validmoves in
  allvalidjumps (safemoves)

(* diff2 board is the helper function that will allow the AI
   to make a decision given input board that corresponds to 
   level 2 difficulty. This difficulty allows the AI to avoid 
   making moves that would directly cause it to lose pieces (for
   example, the AI will not move its own pieces into a position
   where they can be taken unless those moves are the only valid ones.) *)
let diff2 board =
  let safejumplength = List.length (allsafejumps board (allvalidmoves board)) in
  let validjumplength = List.length (allvalidjumps (allvalidmoves board)) in
  let safemovelength = List.length (allsafemoves board (allvalidmoves board)) in
  if safejumplength > 0 then let randintsj = Random.int safejumplength in
    (List.nth (allsafejumps board (allvalidmoves board)) randintsj) 
  else
  if validjumplength > 0 then let randintj = Random.int validjumplength in
    (List.nth (allvalidjumps (allvalidmoves board)) randintj) 
  else
  if safemovelength > 0 then let randints = Random.int safemovelength in
    (List.nth (allsafemoves board (allvalidmoves board)) randints) 
  else
    let randintm = Random.int (List.length (allvalidmoves board)) in
    (List.nth (allvalidmoves board) randintm)

(* diff1 board is the helper function that will allow the AI
   to make a decision given input board that corresponds to 
   level 1 difficulty. This difficulty's AI makes random
   (but valid) moves. *)
let diff1 (board : Board.t) : (int * int) =   
  let validjumplength = List.length (allvalidjumps (allvalidmoves board)) in
  if validjumplength > 0 then let randintj = Random.int validjumplength in
    (List.nth (allvalidjumps (allvalidmoves board)) randintj) 
  else
    let randintm = Random.int (List.length (allvalidmoves board)) in
    (List.nth (allvalidmoves board) randintm)

(* make_move difficulty board is the move the AI takes with the input
   difficulty and the given board. Different difficulties
   correspond with and will call different helper functions
   to help the AI make its decision. *)
let make_move difficulty (board : Board.t) : (int * int) =
  if difficulty = 1 then diff1 board else
  if difficulty = 2 then diff2 board else
    failwith "Unimplemented"