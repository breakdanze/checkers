open Board

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
  displaylst (Board.to_list b); print_string " "; displaycol 0