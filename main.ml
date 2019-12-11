open Board
open Command
open Graphics
open Ai

(** display function *)
let display b =
  let rec displaylst = function
    |[] -> ()
    |[(0,_)] -> print_endline ""
    |(i, s)::t ->(if i mod (!Board.rows) = 1 then 
                    (print_endline "";
                     print_int (!Board.rows - (i / !Board.rows)); 
                     print_string 
                       (if (!Board.rows-(i/ !Board.rows)) <10 then " " else ""))
                  else ()); 
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
        print_char (Char.chr ((i mod !Board.rows) + 97));displaycol (i+1))
    else print_endline "" in
  displaylst (Board.to_list b); print_string " "; displaycol 0;;

let fill_rect_of_int i width height =
  fill_rect ((((i-1) mod !Board.rows) + 1)*60-27)
    (600-(((i-1) / !Board.rows + 1)*60+27)) width height
let draw_rect_of_int width height i = 
  draw_rect ((((i-1) mod !Board.rows) + 1)*60-27)
    (600-(((i-1) / !Board.rows + 1)*60+27)) width height

let draw_checker b = match b with
  | (i, "Space") when (i mod 8 + (i-1) / 8) mod 2 <> 1 -> 
    set_color (rgb 255 204 102);
    fill_rect_of_int i 54 54;
  | (i, "Space") when (i mod 8 + (i-1) / 8) mod 2 = 1 -> 
    set_color (rgb 85 0 0);
    fill_rect_of_int i 54 54;
  | (i, "Black") -> set_color (rgb 255 204 102);
    fill_rect_of_int i 54 54; set_color (rgb 0 0 0);
    fill_circle ((((i-1) mod !Board.rows) + 1)*60)
      (600-(((i-1) / !Board.rows + 1)*60)) 20;
  | (i, "Red") -> set_color (rgb 255 204 102);
    fill_rect_of_int i 54 54; set_color (rgb 170 0 0);
    fill_circle ((((i-1) mod !Board.rows) + 1)*60)
      (600-(((i-1) / !Board.rows + 1)*60)) 20;
  | (i, "Red King") -> set_color (rgb 255 204 102);
    fill_rect_of_int i 54 54; set_color (rgb 255 85 85);
    fill_circle ((((i-1) mod !Board.rows) + 1)*60)
      (600-(((i-1) / !Board.rows + 1)*60)) 20;
  | (i, "Black King") -> set_color (rgb 255 204 102);
    fill_rect_of_int i 54 54; set_color (rgb 85 85 85);
    fill_circle ((((i-1) mod !Board.rows) + 1)*60)
      (600-(((i-1) / !Board.rows + 1)*60)) 20;
  | _ -> ()

let to_letter x = Char.chr (x + 97)

let display2 b = 
  open_graph " 600x600"; set_color (rgb 170 85 0); fill_rect 15 75 510 510;
  set_color (rgb 170 170 170); fill_rect 300 10 140 50; fill_rect 450 10 150 50;
  set_color (rgb 0 0 0); moveto 325 35; draw_string "Help";
  moveto 475 35; draw_string "Quit";
  for x = 0 to 7 do moveto 530 (120+60*x); draw_string (string_of_int (x+1)) done;
  for x = 0 to 7 do moveto (60+60*x) 60; draw_char (to_letter x) done;
  let rec draw_lst b n = match b with
    | h::t -> draw_checker h; draw_lst t n-1
    | _ -> -1 in
  draw_lst (Board.to_list b) 64

let coord_to_int c = 
  try Some (
      let column = (Char.code (String.get c 0) - 97) in 
      if column>7 || column<0 then (raise Malformed) else
        let row = (Char.code (String.get c 1) - 49) in 
        if row>7 || row<0 then (raise Malformed) else 
          (column+(7-row)*8)+1
    ) with Malformed -> None

let eval_move board move_phrase = 
  let int_coord1 = (match (coord_to_int (List.nth move_phrase 0)) with 
      | Some c -> c
      | None -> -1) in 
  let int_coord2 = (match (coord_to_int (List.nth move_phrase 1)) with 
      | Some c -> c
      | None -> -1) in 
  if int_coord1 = -1 || int_coord2 = -1 then 
    ("Invalid coordinates. Please try again. ",
     board)
  else 
    let movable = Board.movable board in 
    (*one or more jumps are available *)
    if (snd movable <> []) then 
      (*chosen piece is able to make a jump *) 
      if (List.mem int_coord1 (snd movable)) then  
        (*chosen jump is valid *)
        if (Board.is_valid_jump board int_coord1 int_coord2) then 
          (Board.jump board int_coord1 int_coord2;
           (*no jump is available for int_coord2. change turn.*)
           if not (List.mem (int_coord2) (snd (Board.movable board))) then 
             Board.change_turn board;
           ("", board))
        else ("Invalid jump. Please try again. ",
              board)
      else ("A jump is available. Please try again. ",
            board)
    else (*one or more moves are available*) 
      (*chosen piece is able to move*)
    if (List.mem int_coord1 (fst movable)) then 
      (*chosen move is valid*)
      if (Board.is_valid_move board int_coord1 int_coord2) then 
        (Board.move board int_coord1 int_coord2;
         Board.change_turn board;
         ("", board))          else ("Invalid move. Please try again. ",
                                     board)
    else ("The chosen piece has no available moves. Please try again. "
         , board)

let within_rect status x y w h =
  x < status.mouse_x && status.mouse_x < x+w &&
  y < status.mouse_y && status.mouse_y < y+h

let x_coord_of_point x = (x-33)/60
let y_coord_of_point y = (y-93)/60+1

let coord_of_status status = 
  (status.mouse_x |> x_coord_of_point |> to_letter |> Char.escaped)
  ^(status.mouse_y |> y_coord_of_point |> string_of_int)

let command_of_coords p1 p2 =
  if p1 = "help" || p2 = "help" then "help" else
  if p1 = "quit" || p2 = "quit" then "quit" else
    "move "^p1^" "^p2 

let rec change_state (board:Board.t) (mode) (difficulty) (message:string): unit =
  let _ = display2 board in 
  if (Board.win board) then (
    moveto 0 30;
    set_text_size 10;
    draw_string (Board.current_turn board^" wins!");
    (*set_color (rgb 170 170 170);
      fill_rect 0 20 50 10;
      moveto 0 20;
      set_color (rgb 0 0 0);
      draw_string "Restart ";
      set_color (rgb 170 170 170);
      fill_rect 50 20 50 10;
      moveto 50 20;
      set_color (rgb 0 0 0);
      draw_string "Quit ";*)
    let no_good_click_yet = ref true in
    while !no_good_click_yet do
      let status = wait_next_event [Button_up] in
      if (within_rect status 0 20 50 10) then
        let () = no_good_click_yet := false in () else
      if (within_rect status 450 10 150 50) then 
        let () = no_good_click_yet := false in exit 0
    done)
  else
    (
      moveto 0 20;
      draw_string (Board.current_turn board^"'s turn. ");
      moveto 0 0;
      draw_string message;
      if 
        (String.equal mode "1p") && (* difficulty = 1 && *)(String.equal (Board.current_turn board) "Red")
      then 
        (
          let movement = Ai.make_move 1 board in
          let action = if 
            Board.is_valid_move board ((fst movement)+1) ((snd movement)+1) 
            then Board.move else Board.jump in
          action board ((fst movement)+1) ((snd movement)+1);
          (if 
            not (List.mem (snd movement) (snd (Board.movable board))) 
           then (* TODO: fix multijump, make sure you can only multijump after a jump*)
             Board.change_turn board);
          change_state board mode difficulty ""
        )
      else 
      if 
        (String.equal mode "1p") && (* difficulty = 2 && *) (String.equal (Board.current_turn board) "Red")
      then 
        (
          let movement = Ai.make_move 2 board in
          let action = if 
            Board.is_valid_move board ((fst movement)+1) ((snd movement)+1) 
            then Board.move else Board.jump in
          action board ((fst movement)+1) ((snd movement)+1);
          (if 
            not (List.mem (snd movement) (snd (Board.movable board))) 
           then (* TODO: fix multijump, make sure you can only multijump after a jump*)
             Board.change_turn board);
          Unix.sleepf 0.5;
          change_state board mode difficulty ""
        )
      else 
        try (
          let status1 = wait_next_event [Button_down] in
          let user_input = if (within_rect status1 300 10 150 50) then "help"
            else if (within_rect status1 450 10 150 50) then "quit" else
              let coord1 = coord_of_status status1 in
              let int1 = coord_to_int coord1 in
              (match int1 with
               | Some i -> set_color (rgb 0 0 0); draw_rect_of_int 54 54 i;
               | None -> print_endline "oops");
              let status2 = wait_next_event [Button_down] in
              let coord2 = coord_of_status status2 in
              let int2 = coord_to_int coord2 in
              (match int2 with
               | Some i -> draw_rect_of_int 54 54 i;
               | None -> ());
              command_of_coords coord1 coord2 in
          let input_parsed = parse user_input in 
          match input_parsed with 
          | Move move_phrase -> let pair = eval_move board move_phrase in
            change_state (snd pair) mode difficulty (fst pair)
          | Quit -> (
              draw_string "Quitting... "; 
              exit 0 ) 
          | Help -> (
              change_state board mode difficulty 
                "Click a checker and then a square you want to move it to. "
            )

        )
        with
        | Malformed -> (
            change_state board mode difficulty "Malformed command. Try again. "
          )
        | Empty -> ( 
            change_state board mode difficulty "\n\nEmpty command. Try again. "
          )
    )

let play_game (mode:string) (difficulty:int) : unit= 
  if (String.equal mode "1p" || String.equal mode "2p") then
    change_state (Board.init  8) mode difficulty ""
  else print_endline "\nInvalid mode\n\n"

let main () =
  open_graph " 600x600";
  (*set_text_size 30;*)
  draw_string "Welcome to checkers.";
  set_color (rgb 170 170 170);
  fill_rect 150 375 300 100;
  fill_rect 150 150 300 100;
  fill_rect 150 263 300 100;
  set_color (rgb 0 0 0);
  moveto 200 425;
  draw_string "Singleplayer Level 1";
  moveto 200 312;
  draw_string "Singleplayer Level 2";
  moveto 200 200;
  draw_string "Multiplayer";
  let no_good_click_yet = ref true in
  while !no_good_click_yet do
    let status = wait_next_event [Button_down] in
    if (within_rect status 150 375 300 100) then 
      let () = no_good_click_yet := false in play_game "1p" 1; else
    if (within_rect status 150 263 300 100) then
      let () = no_good_click_yet := false in play_game "1p" 2; else
    if (within_rect status 150 150 300 100) then
      let () = no_good_click_yet := false in play_game "2p" 2
  done

(* Execute the game engine. *)
let () = main ()
