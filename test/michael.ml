open OUnit2
open Indicator
open Ma
open Ethdata
open Rsi

(** [sma_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [sma input]. *)
let sma_test (name : string) (lst : float list) (n : int)
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

(** [rsi_test name lst expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [rsi lst]. *)
let rsi_test (name : string) (lst : float list)
    (expected_output : float list) : test =
  name >:: fun _ -> assert_equal expected_output (rsi lst)

let rsi_tests =
  [
    rsi_test "First RSI Test"
      [
        19.;
        17.;
        24.;
        19.;
        22.;
        18.;
        16.;
        14.;
        15.;
        13.;
        11.;
        12.;
        16.;
        13.;
        15.;
        12.;
        18.;
        14.;
        12.;
        11.;
        12.;
      ]
      [ 45.; 41.64; 49.73; 45.23; 43.12; 42.07; 43.56 ];
  ]

let suite = "test suite for indicators" >::: List.flatten [ sma_tests ]
let _ = run_test_tt_main suite