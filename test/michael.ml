open OUnit2
open Indicator
open Ma
open Ethdata

(** [sma_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [sma input]. *)
let sma_test
    (name : string)
    (lst : float list)
    (n : int)
    (expected_output : float list) : test =
  name >:: fun _ -> assert_equal expected_output (sma lst n)

let sma_tests =
  [
    sma_test "First SMA Test" [ 1.; 2.; 3.; 4.; 5. ] 2
      [ 1.5; 2.5; 3.5; 4.5 ];
    sma_test "Second SMA Test"
      [ 11.; 12.; 13.; 14.; 15.; 16.; 17. ]
      5 [ 13.; 14.; 15. ];
  ]

let suite = "test suite for indicators" >::: List.flatten [ sma_tests ]
let _ = run_test_tt_main suite