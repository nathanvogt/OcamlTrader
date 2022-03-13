open OUnit2
open Indicators

(** [sma_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [sma input]. *)
let sma_test
    (name : string)
    (input : float list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (sma input) ~printer:string_of_float

let sma_tests = [ sma_test "First SMA Test" [ 1.; 2.; 3.; 4.; 5. ] 3. ]
let suite = "test suite for A2" >::: List.flatten [ sma_tests ]
let _ = run_test_tt_main suite
