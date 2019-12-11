open OUnit2
open Board
open Command
(* open Ai*)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let board = Board.init 8;;
let board_init = board;;
let board_basic = Main.eval_move board ["a3";"d4"];;

let list_basic = 
  [(1, "Space"); (2, "Red"); (3, "Space"); (4, "Red"); (5, "Space"); (6, "Red");
   (7, "Space"); (8, "Red"); (9, "Red"); (10, "Space"); (11, "Red");
   (12, "Space"); (13, "Red"); (14, "Space"); (15, "Red"); (16, "Space");
   (17, "Space"); (18, "Red"); (19, "Space"); (20, "Red"); (21, "Space");
   (22, "Red"); (23, "Space"); (24, "Red"); (25, "Space"); (26, "Space");
   (27, "Space"); (28, "Space"); (29, "Space"); (30, "Space"); (31, "Space");
   (32, "Space"); (33, "Space"); (34, "Space"); (35, "Space"); (36, "Space");
   (37, "Space"); (38, "Space"); (39, "Space"); (40, "Space"); (41, "Black");
   (42, "Space"); (43, "Black"); (44, "Space"); (45, "Black"); (46, "Space");
   (47, "Black"); (48, "Space"); (49, "Space"); (50, "Black"); (51, "Space");
   (52, "Black"); (53, "Space"); (54, "Black"); (55, "Space"); (56, "Black");
   (57, "Black"); (58, "Space"); (59, "Black"); (60, "Space"); (61, "Black");
   (62, "Space"); (63, "Black"); (64, "Space"); (0, "Current turn is Black")]

let first_move = 
  ["Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";
   "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space";
   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";
   "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space";
   "Space"; "Space"; "Space"; "Black"; "Space"; "Space"; "Space"; "Space";
   "Black"; "Space"; "Space"; "Space"; "Black"; "Space"; "Black"; "Space";
   "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black";
   "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space";
   "Current turn is Red"]

let board_tests = [
  "init" >::
  (fun _ -> assert_equal (Board.to_list (snd board_basic)) (list_basic))
]

let main_tests = [
  "to_letter" >:: (fun _ -> assert_equal (Main.to_letter 0) 'a');

  "eval_move invalid coords" >:: 
  (fun _ -> assert_equal (Main.eval_move board ["g9"; "h5"])
      ("Invalid coordinates. Please try again. ", board));

  "eval_move c3d4" >:: (fun _ ->
      assert_equal
        (Main.eval_move board ["c3"; "d4"] 
         |> snd |> Board.to_list |> List.map snd) 
        ["Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";
         "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space";
         "Space"; "Red";   "Space"; "Red";   "Space"; "Red"; "Space"; "Red";
         "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space";
         "Space"; "Space"; "Space"; "Black"; "Space"; "Space"; "Space"; "Space";
         "Black"; "Space"; "Space"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black";
         "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Current turn is Red"]
        ~printer:(pp_list pp_string));

  "eval_move c3d4 f6e5 " >:: (fun _ ->
      assert_equal (Main.eval_move 
                      (Main.eval_move board 
                         ["c3"; "d4"] |> snd) 
                      ["f6"; "e5"] |> snd |> Board.to_list |> List.map snd) 
        ["Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";
         "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space";
         "Space"; "Red";   "Space"; "Red";   "Space"; "Space"; "Space"; "Red";
         "Space"; "Space"; "Space"; "Space"; "Red";   "Space"; "Space"; "Space";
         "Space"; "Space"; "Space"; "Black"; "Space"; "Space"; "Space"; "Space";
         "Black"; "Space"; "Space"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black";
         "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Current turn is Black"]
        ~printer:(pp_list pp_string));

  "eval_move c3d4 f6e5 force jump" >:: (fun _ ->
      assert_equal (Main.eval_move 
                      (Main.eval_move 
                         (Main.eval_move board 
                            ["c3"; "d4"] |> snd) 
                         ["f6"; "e5"] |> snd)
                      ["a3";"b4"]) 
        ("A jump is available. Please try again. ",board));

  "eval_move c3d4 f6e5 d4f6 jump" >:: (fun _ ->
      assert_equal (Main.eval_move 
                      (Main.eval_move 
                         (Main.eval_move board 
                            ["c3"; "d4"] |> snd) 
                         ["f6"; "e5"] |> snd) 
                      ["d4";"f6"] |> snd |> Board.to_list |> List.map snd) 
        ["Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";
         "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space";
         "Space"; "Red";   "Space"; "Red";   "Space"; "Black"; "Space"; "Red";
         "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space";
         "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space";
         "Black"; "Space"; "Space"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black";
         "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Current turn is Red"]
        ~printer:(pp_list pp_string));      

  "eval_move multijump" >:: (fun _ ->
      assert_equal 
        (Main.eval_move
           (Main.eval_move
              (Main.eval_move 
                 (Main.eval_move 
                    (Main.eval_move 
                       (Main.eval_move 
                          (Main.eval_move (Board.init 8) 
                             ["e3"; "d4"] |> snd) 
                          ["h6"; "g5"] |> snd) 
                       ["f2";"e3"] |> snd)
                    ["g5";"h4"] |> snd) 
                 ["d4";"c5"] |> snd) 
              ["b6";"d4"] |> snd) 
           ["d4";"f2"] |> snd |> Board.to_list |> List.map snd) 
        ["Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";
         "Red";   "Space"; "Red";   "Space"; "Red";   "Space"; "Red";   "Space";
         "Space"; "Space"; "Space"; "Red";   "Space"; "Red";   "Space"; "Space";
         "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space";
         "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Space"; "Red";
         "Black"; "Space"; "Black"; "Space"; "Space"; "Space"; "Black"; "Space";
         "Space"; "Black"; "Space"; "Black"; "Space"; "Red";   "Space"; "Black";
         "Black"; "Space"; "Black"; "Space"; "Black"; "Space"; "Black"; "Space";
         "Current turn is Black"]
        ~printer:(pp_list pp_string));    

  "coord_to_int e3 = 45" >::
  (fun _ -> assert_equal (Some 45) (Main.coord_to_int "e3"));

  "coord_to_int a8 = 1" >::
  (fun _ -> assert_equal (Some 1) (Main.coord_to_int "a8"));

  "coord_to_int a1 = 57" >::
  (fun _ -> assert_equal (Some 57) (Main.coord_to_int "a1"));

  "coord_to_int h1 = 64" >::
  (fun _ -> assert_equal (Some 64) (Main.coord_to_int "h1"));

  "coord_to_int h8 = 64" >::
  (fun _ -> assert_equal (Some 8) (Main.coord_to_int "h8"));

  "coord_to_int h9 = None" >::
  (fun _ -> assert_equal None (Main.coord_to_int "h9"));

  "coord_to_int `1 = None" >::
  (fun _ -> assert_equal None (Main.coord_to_int "`1"));

  "coord_to_int {1 = None" >::
  (fun _ -> assert_equal None (Main.coord_to_int "{1"));

  "command_of_coords c3 d4 = \"move c3 d4\"" >::
  (fun _ -> assert_equal "move c3 d4" (Main.command_of_coords "c3" "d4"));
]

let command_tests = []

let suite =
  "test suite for checkers"  >::: List.flatten [
    board_tests;
    main_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite