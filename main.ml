open Board

(** statechanger recursively changes the state and progresses the game
    based on user input *)
let rec statechanger state =
  (* let player = state.player in *)
  (* print_endline ("It is " ^ player ^ "'s turn."); *)
  (* print_string ("> "); *)
  (* let written = read_line () in *)
  (* try 
     match Command.parse written with 
  *)
  failwith "Unimplemented"


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
    |[(0,_)] -> ()
    |(i, s)::t ->(if i mod (!Board.rows) = 1 then print_endline "" else ()); 
      (match s with
       |"Space" -> print_string "[ ]"; displaylst t
       |"Black" -> print_string " B "; displaylst t
       |"Red" -> print_string " R "; displaylst t
       |"Red King" -> print_string " RK"; displaylst t
       |"Black King" -> print_string " BK"; displaylst t
       | _ -> failwith "WHAT") in
  displaylst (Board.to_list b)
