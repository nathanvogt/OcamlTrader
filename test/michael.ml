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
let rsi_test
    (name : string)
    (lst : float list)
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