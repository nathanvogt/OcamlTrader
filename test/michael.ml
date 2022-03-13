open OUnit2
open Indicators

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let sma_test (name : string) (input : float list) (expected_output : float) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (sma input) ~printer:string_of_float

(* You will find it helpful to write functions like [index_test] for
   each of the other functions you are testing. They will keep your
   lists of tests below very readable, and will also help you to avoid
   repeating code. You will also find it helpful to create [~printer]
   functions for the data types in use. *)

let sma_tests =
  [
    sma_test "First SMA Test" [1.; 2.; 3.; 4.; 5.] 3.;
  ]

  let suite =
    "test suite for A2"
    >::: List.flatten [ sma_tests; ]
  
  let _ = run_test_tt_main suite