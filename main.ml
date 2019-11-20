open Board
open Command

(** display function *)
let display b =
  let rec displaylst = function
    |[] -> ()
    |[(0,_)] -> print_endline ""
    |(i, s)::t ->(if i mod (!Board.rows) = 1 then 
                    (print_endline "";
                     print_int (i / !Board.rows + 1); 
                     print_string (if i / !Board.rows + 1 < 10 then " " else "")) else ()); 
      (match s with
       |"Space" -> print_string "[ ]"; displaylst t
       |"Black" -> print_string " B "; displaylst t
       |"Red" -> print_string " R "; displaylst t
       |"Red King" -> print_string " RK"; displaylst t
       |"Black King" -> print_string " BK"; displaylst t
       | _ -> failwith "WHAT") in
  let rec displaycol i =
    if i < !Board.rows then 
      ( print_string (if (i mod !Board.rows) + 1 < 10 then "  " else " "); 
        print_int ((i mod !Board.rows) + 1);displaycol (i+1))
    else print_endline "" in
  displaylst (Board.to_list b); print_string " "; displaycol 0;;

let coord_to_int c = 
  try Some (
      let column = (Char.code (String.get c 0) - 97) in 
      if column>7 || column<0 then (raise Malformed) else
        let row = (Char.code (String.get c 1) - 49) in 
        if row>8 || row<0 then (raise Malformed) else 
          (column+(7-row)*8)+1
    ) with Malformed -> ();
    None

let eval_move board move_phrase = 
  let int_coord1 = (match (coord_to_int (List.nth move_phrase 0)) with 
      | Some c -> c
      | None -> -1) in 
  let int_coord2 = (match (coord_to_int (List.nth move_phrase 1)) with 
      | Some c -> c
      | None -> -1) in 
  if int_coord1 = -1 || int_coord2 = -1 then 
    (print_string ("\n\nInvalid coordinates. Please try again.");
     board)
  else 
    let movable = Board.movable board in 
    if (snd movable <> []) then (*one or more jumps are available *)
      if (List.mem int_coord1 (snd movable)) then  (*chosen piece is able to make a jump *) 
        if (Board.is_valid_jump board int_coord1 int_coord2) then (*chosen jump is valid *)
          (Board.jump board int_coord1 int_coord2;
           Board.change_turn board;
           board)
        else ((print_string ("\n\nInvalid jump. Please try again."));
              board)
      else ((print_string ("\n\nA jump is available. Please try again."));
            board)
    else (*one or more moves are available*) 
    if (List.mem int_coord1 (fst movable)) then (*chosen piece is able to move*)
      if (Board.is_valid_move board int_coord1 int_coord2) then (*chosen move is valid*)
        (Board.move board int_coord1 int_coord2;
         Board.change_turn board;
         board)          else ((print_string ("\n\nInvalid move. Please try again."));
                               board)
    else ((print_string "\n\nThe chosen piece has no available moves. Please try again.");
          board)


let rec change_state (board:Board.t) : unit =
  let _ = display board in 
  if (Board.win board) then 
    print_string ((Board.current_turn board)^" wins!")
  else ();
  print_string ("\n"^Board.current_turn board^"'s turn.\n");
  print_string ("Please enter a command.\n") ;
  try (
    let user_input = read_line () in 
    let input_parsed = parse user_input in 
    match input_parsed with 
    | Move move_phrase -> change_state (eval_move board move_phrase)


    | Quit -> (
        print_string "\nQuitting...\n\n"; 
        exit 0 ) 
    | Help -> (
        print_string "\nUse 'move [coordinate1] [coordinate2]' to move your 
        piece from coordinate1 to coordinate2 (ex. 'move a3 b4')\nUse 'help' to 
        see this menu. \nUse 'quit' to exit the game.\nPress enter to continue.
        \n";
        match read_line () with 
        | _ -> change_state board
      )

  )
  with
  | Malformed -> (
      print_string "\n\nMalformed command. Try again.";
      change_state board
    )
  | Empty -> ( 
      print_string "\n\nEmpty command. Try again.";
      change_state board
    )

let play_game (mode:string) : unit= 
  if String.equal mode "1p" then
    change_state (Board.init  8)
  else if String.equal mode "2p" then 
    change_state (Board.init  8)
  else print_endline "\nInvalid mode\n\n"

let main () =
  ANSITerminal.(print_string [red] "\n\n\nWelcome to checkers.\n");
  print_endline "Enter '1p' for single player or '2p' for multiplayer";
  match read_line () with 
  | exception End_of_file -> () 
  | mode -> play_game mode

(* Execute the game engine. *)
let () = main ()