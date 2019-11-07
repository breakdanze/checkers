(** statechanger recursively changes the state and progresses the game
    based on user input *)
open Board
open Command


let rec change_state board =
  (* let player = state.player in *)
  (* print_endline ("It is " ^ player ^ "'s turn."); *)
  (* print_string ("> "); *)
  (* let written = read_line () in *)
  (* try 
     match Command.parse written with 
  *)
  let board_disp = display board in 
  print_string "\n"^desc^"\n"
let 
  try (
    | Move -> ()
    | Quit -> ()
  )
  with
  | Malformed -> (
      print_string "\n\nMalformed command. Try again."
    )
  | Empty -> ( 
      print_string "\n\nEmpty command. Try again."
    )


(** [begin_game f] begins the game with given settings [f]. *)
(*
let begin_game f = 


*)

(** [main ()] asks for settings for the game to play, then starts it *)
(* 
let main () =



*)

(* Execute the game *)
(* 
let () = main () 
*)