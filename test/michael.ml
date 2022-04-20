open OUnit2
open Indicator
open Ma
open Ethdata
open Rsi

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_string s] pretty-prints float [s]. *)
let pp_float s = "\"" ^ string_of_float s ^ "\""

(** [pp_string s] pretty-prints float * float [s]. *)
let pp_float_float s =
  "\"" ^ string_of_float (fst s) ^ ", " ^ string_of_float (snd s) ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list]
   to get helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] );
    (* Uncomment this test to see what happens when a test case
       fails. *)
    ( "duplicates not allowed" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "foo" ] [ "foo" ] );
  ]

(********************************************************************
       End helper functions.
 ********************************************************************)

(** [sma_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [sma input]. *)
let sma_test
    (name : string)
    (lst : float list)
    (n : int)
    (expected_output : float list) : test =
  name >:: fun _ ->
  assert_equal ~printer:(pp_list pp_float) expected_output (sma lst n)

(** Ounit test for [avg] in module [Ma] *)
let avg_test
    (name : string)
    (lst : float list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_float expected_output (avg lst)

(** Ounit test for [diff] in module [Ma] *)
let diff_test
    (name : string)
    (lst : float list)
    (expected_output : float list) : test =
  name >:: fun _ ->
  assert_equal ~printer:(pp_list pp_float) expected_output (diff lst)

(** Ounit test for [gain_loss] in module [Ma] *)
let gain_loss_test
    (name : string)
    (lst : float list)
    (expected_output : float * float) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_float_float expected_output
    (gain_loss (0., 0.) lst)

let ma_tests =
  [
    sma_test "First SMA Test" [ 1.; 2.; 3.; 4.; 5. ] 2
      [ 1.5; 2.5; 3.5; 4.5 ];
    sma_test "Second SMA Test"
      [ 11.; 12.; 13.; 14.; 15.; 16.; 17. ]
      5 [ 13.; 14.; 15. ];
    avg_test "First SMA Test" [ 3.; 4.; 5.; 6.; 7. ] 5.;
    avg_test "Second SMA Test" [ 0.; 0.; 0.; 0.; 0. ] 0.;
    avg_test "Third SMA Test" [ -3.; -4.; -5.; -6.; -7. ] (5. *. -1.);
    diff_test "First test case for diff function in module Ma"
      [ 1.; 2.; 3. ] [ 1.; 1. ];
    diff_test "Second test case for diff function in module Ma"
      [ 0.; 0.; 0. ] [ 0.; 0. ];
    diff_test "Third test case for diff function in module Ma"
      [ 3.; 2.; 1. ] [ -1.; -1. ];
    diff_test "Fourth test case for diff function in module Ma"
      [ 3.; -5.; 20. ] [ -8.; 25. ];
    gain_loss_test "First test case for gain_loss function in module Ma"
      [ 1.; 2. ] (3., 0.);
    gain_loss_test
      "Second test case for gain_loss function in module Ma"
      [ -1.; -2. ] (0., -3.);
    gain_loss_test "Third test case for gain_loss function in module Ma"
      [ 0.; 0. ] (0., 0.);
    gain_loss_test
      "Fourth test case for gain_loss function in module Ma"
      [ 1.; -1.; 10.; -10. ] (11., -11.);
  ]

(** [rsi_test name lst expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [rsi lst]. *)
let rsi_test
    (name : string)
    (lst : float list)
    (expected_output : float list) : test =
  let rsi_output = List.map (fun x -> Float.round x) (rsi lst) in
  let expected_output =
    List.map (fun x -> Float.round x) expected_output
  in
  name >:: fun _ ->
  assert_equal ~printer:(pp_list pp_float) expected_output rsi_output

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
    rsi_test "Second RSI Test"
      [
        10.;
        80.;
        110.;
        20.;
        200.;
        20.;
        60.;
        90.;
        10.;
        200.;
        610.;
        120.;
        230.;
        40.;
        150.;
      ]
      [ 53. ];
    rsi_test "Third RSI Test based on AAPL historical data"
      [
        176.23;
        174.2;
        167.65;
        164.91;
        163.01;
        159.76;
        157.63;
        154.46;
        150.38;
        150.1;
        154.5;
        155.98;
        159.41;
        155.8;
        159.04;
        162.1;
        165.55;
        162.95;
        161.97;
        162.43;
      ]
      [ 30.; 35.; 41.; 38.; 37.; 38. ];
    rsi_test "Fourth RSI Test based on AMZN historical data"
      [
        3055.290039;
        3094.080078;
        3161.;
        3226.72998;
        3223.820068;
        3279.389893;
        3299.300049;
        3372.199951;
        3379.389893;
        3400.;
        3333.;
        3379.090088;
        3399.439941;
        3372.01001;
        3334.689941;
        3362.02002;
        3309.040039;
        3340.879883;
        3409.;
        3417.429932;
        3458.5;
        3471.310059;
        3467.419922;
        3386.48999;
        3311.870117;
        3270.540039;
        3306.370117;
        3291.610107;
        3190.48999;
      ]
      [
        75.;
        76.;
        69.;
        71.;
        74.;
        75.;
        77.;
        77.;
        77.;
        65.;
        56.;
        52.;
        55.;
        53.;
        44.;
      ];
  ]

let suite =
  "test suite for indicators" >::: List.flatten [ ma_tests; rsi_tests ]

let _ = run_test_tt_main suite