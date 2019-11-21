open OUnit2
open Board
open Main
open Command
(* open Ai*)

let board = Board.init 8;;
let board_init = board;;
let board_basic = eval_move board ["a3";"d4"];;

let list_basic = [(1, "Space"); (2, "Red"); (3, "Space"); (4, "Red"); (5, "Space"); (6, "Red");
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

let board_tests = [
  "init" >:: (fun _ -> assert_equal (Board.to_list board_basic) (list_basic))
]

let main_tests = []

let command_tests = []

let suite =
  "test suite for checkers"  >::: List.flatten [
    board_tests;
    main_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite