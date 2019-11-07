 
(** statechanger recursively changes the state and progresses the game
    based on user input *)
open Board
open Command

(** display function*)
let display b =
  let rec displaylst = function
    |[] -> ()
    |[(0,_)] -> print_endline ""
    |(i, s)::t ->(if i mod (!Board.rows) = 1 then 
                    (print_endline "";
                     print_int (i / !Board.rows + 1); print_string " ") else ()); 
      (match s with
       |"Space" -> print_string "[ ]"; displaylst t
       |"Black" -> print_string " B "; displaylst t
       |"Red" -> print_string " R "; displaylst t
       |"Red King" -> print_string " RK"; displaylst t
       |"Black King" -> print_string " BK"; displaylst t
       | _ -> failwith "WHAT") in
  let rec displaycol i =
    if i < !Board.rows then 
      ( print_string "  "; print_int ((i mod !Board.rows) + 1);displaycol (i+1))
    else print_endline "" in
  displaylst (Board.to_list b); print_string " "; displaycol 0;;


let rec change_state (board:Board.t) : unit =
  (* let player = state.player in *)
  (* print_endline ("It is " ^ player ^ "'s turn."); *)
  (* print_string ("> "); *)
  (* let written = read_line () in *)
  (* try 
     match Command.parse written with 
  *)
  let _ = display board in 
  print_string "\n"; 
  try (
    let user_input = read_line () in 
    let input_parsed = parse user_input in 
    match input_parsed with 
    | Move move_phrase -> ()
    | Quit -> (
        print_string "\nQuitting...\n\n"; 
        exit 0 ) 
    | Help -> (
        print_string "\nHelpful message!\nPress enter to continue.\n";
        match read_line () with 
        | _ -> change_state board
      )

  )
  with
  | Malformed -> (
      print_string "\n\nMalformed command. Try again."
    )
  | Empty -> ( 
      print_string "\n\nEmpty command. Try again."
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
