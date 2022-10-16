open OUnit2
open Dbms
open Authors

let author_test (name:string):test= 
  name >:: fun _ -> assert_equal 6 (total_hours [2;2;2]) ~printer:string_of_int


(*TODO:Delete this after adding actual tests *)
let author_tests=[
author_test "Dummy test"
]

let suite = 
  "test suite for final project"
  >::: List.flatten [author_tests]

let _ = run_test_tt_main suite

