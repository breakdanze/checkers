open OUnit2
open Board
open Main
open Command
(* open Ai*)

let board_tests = []

let main_tests = []

let command_tests = []

let suite =
  "test suite for checkers"  >::: List.flatten [
    board_tests;
    main_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite