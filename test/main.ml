open OUnit2
open Indicator
open Ma
open Rsi
open Obv

(* OTHER POSSIBLE TESTS: *)
(* - new_indic_val in state.ml *)
(* - feeder general functionality (should be very easy, just see see if
   pulled data matches expected data) *)
(* - related easy test is to make sure header labels are correct (close
   corresponds to close, high corresponds to high, nothing goes below
   low, etc.) *)

(********************************************************************
 Helper functions helping with list comparison
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

(** [pp_float s] pretty-prints float [s]. *)
let pp_float s = "\"" ^ string_of_float s ^ "\""

(** [pp_float_float s] pretty-prints float * float [s]. *)
let pp_float_float s =
  "\"" ^ string_of_float (fst s) ^ ", " ^ string_of_float (snd s) ^ "\""

(** [pp_float_int s] pretty-prints float * int [s]. *)
let pp_float_int s =
  "\"" ^ string_of_float (fst s) ^ ", " ^ string_of_int (snd s) ^ "\""

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

(********************************************************************
  Testing Indicators
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

[@@@ocamlformat "disable=true"]

let rsi_tests =
  [
    rsi_test "First RSI Test"
      [19.; 17.; 24.; 19.; 22.; 18.; 16.; 14.; 15.; 13.; 11.; 12.; 16.; 13.; 
      15.; 12.; 18.; 14.; 12.; 11.; 12.]
      [ 45.; 41.64; 49.73; 45.23; 43.12; 42.07; 43.56 ];
    rsi_test "Second RSI Test"
      [10.; 80.; 110.; 20.; 200.; 20.; 60.; 90.; 10.; 200.; 610.; 120.; 230.; 
      40.; 150.]
      [ 53. ];
    rsi_test "Third RSI Test based on AAPL historical data"
      [176.23; 174.2; 167.65; 164.91; 163.01; 159.76; 157.63; 154.46; 150.38; 
      150.1; 154.5; 155.98; 159.41; 155.8; 159.04; 162.1; 165.55; 162.95; 
      161.97; 162.43]
      [ 30.; 35.; 41.; 38.; 37.; 38. ];
    rsi_test "Fourth RSI Test based on AMZN historical data"
      [3055.290039; 3094.080078; 3161.; 3226.72998; 3223.820068; 3279.389893;
      3299.300049; 3372.199951; 3379.389893; 3400.; 3333.; 3379.090088; 
      3399.439941; 3372.01001; 3334.689941; 3362.02002; 3309.040039; 
      3340.879883; 3409.; 3417.429932; 3458.5; 3471.310059; 3467.419922; 
      3386.48999; 3311.870117; 3270.540039; 3306.370117; 3291.610107; 
      3190.48999]
      [75.; 76.; 69.; 71.; 74.; 75.; 77.; 77.; 77.; 65.; 56.; 52.; 55.; 53.; 
      44.];
  ]

[@@@ocamlformat "disable=false"]

let obv_test
    (name : string)
    (prev_obv : int)
    (prev_close : float)
    (vol : int)
    (close : float)
    (coin : string)
    (expected_output : float * int) =
  name >:: fun _ ->
  assert_equal ~printer:pp_float_int expected_output
    (Obv.update_val prev_obv prev_close vol close coin)

let obv_tests =
  [
    (* pricing for days 1-10 taken from
       https://www.investopedia.com/terms/o/onbalancevolume.asp*)
    obv_test
      "OBV Test of Day 1: closing price equals $10, volume equals \
       25,200 shares; Expected OBV = 0"
      25200 10.01 25200 10. "ETH" (10., 0);
    obv_test
      "OBV Test of Day 2: closing price equals $10.15, volume equals \
       30,000 shares; Expected OBV = 30,000"
      0 10. 30000 10.15 "ETH" (10.15, 30000);
    obv_test
      "OBV Test of Day 3: closing price equals $10.17, volume equals \
       25,600 shares; Expected OBV = 55,600"
      30000 10.15 25600 10.17 "ETH" (10.17, 55600);
    obv_test
      "OBV Test of Day 4: closing price equals $10.13, volume equals \
       32,000 shares; Expected OBV = 23,600"
      55600 10.17 32000 10.13 "ETH" (10.13, 23600);
    obv_test
      "OBV Test of Day 5: closing price equals $10.11, volume equals \
       23,000 shares; Expected OBV = 600"
      23600 10.13 23000 10.11 "ETH" (10.11, 600);
    obv_test
      "OBV Test of Day 6: closing price equals $10.15, volume equals \
       40,000 shares; Expected OBV = 40,600"
      600 10.11 40000 10.15 "ETH" (10.15, 40600);
    obv_test
      "OBV Test of Day 7: closing price equals $10.20, volume equals \
       36,000 shares; Expected OBV = 76,600"
      40600 10.15 36000 10.20 "ETH" (10.20, 76600);
    obv_test
      "OBV Test of Day 8: closing price equals $10.20, volume equals \
       20,500 shares; Expected OBV = 76,600"
      76600 10.20 20500 10.20 "ETH" (10.20, 76600);
    obv_test
      "OBV Test of Day 9: closing price equals $10.22, volume equals \
       23,000 shares; Expected OBV = 99,600"
      76600 10.20 23000 10.22 "ETH" (10.22, 99600);
    obv_test
      "OBV Test of Day 10: closing price equals $10.21, volume equals \
       27,500 shares; Expected OBV = 72,100"
      99600 10.22 27500 10.21 "ETH" (10.21, 72100);
  ]

(********************************************************************
  Testing State
  ********************************************************************)

(* mock position lists for testing *)
let positions_list_empty = []
let positions_list_one = [ 2.; 4.; 6.; 8.; 10. ]
let positions_list_two = [ 10.; 8.; 6.; 4.; 2. ]

(* mock budget to test buy *)
let budget = 100.

(* helper function copied from state, testing logic of calculating
   average position value after buy *)
let state_market_value_buy position_size average_position_val buy_price
    =
  if buy_price > budget then average_position_val
  else
    ((position_size *. average_position_val) +. buy_price)
    /. (position_size +. 1.)

(* helper function copied from state, testing logic of calculating
   average position value after sell *)
let state_market_value_sell
    position_size
    average_position_val
    position_list =
  if List.length position_list = 0 then 0.
  else
    ((position_size *. average_position_val) -. List.hd position_list)
    /. (position_size -. 1.)

(* testing logic of state averaging on buy *)
let state_market_value_buy_test
    (name : string)
    (average_position_val : float)
    (position_size : float)
    (buy_price : float)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected_output
    (state_market_value_buy position_size average_position_val buy_price)

(* testing logic of state averaging on sell *)
let state_market_value_sell_test
    (name : string)
    (average_position_val : float)
    (position_size : float)
    (position_list : float list)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected_output
    (state_market_value_sell position_size average_position_val
       position_list)

let state_tests =
  [
    state_market_value_buy_test "testing buy on average of no positions"
      0. 0. 10. 10.;
    state_market_value_buy_test
      "testing buy on average of 10 positions with same price" 10. 10.
      10. 10.;
    state_market_value_buy_test
      "testing buy on average of 2 positions with higher price" 10. 2.
      40. 20.;
    state_market_value_buy_test
      "testing buy on average of 4 positions with lower price" 10. 4. 0.
      8.;
    state_market_value_buy_test
      "attempting to buy price more expensive than current budget \
       should return original average"
      10. 4. 120. 10.;
    state_market_value_sell_test "testing sell on empty position list"
      0. 0. [] 0.;
    state_market_value_sell_test
      "testing sell on position list with 2. as most recent" 6. 5.
      positions_list_one 7.;
    state_market_value_sell_test
      "testing sell on position list with 10. as most recent" 6. 5.
      positions_list_two 5.;
  ]

let multiple_next_day n =
  let _ = Feeder.reset_reader () in
  let rec aux acc count =
    if count = 0 then acc
    else aux (Feeder.next_day () :: acc) (count - 1)
  in
  aux [] n
  |> List.filter (fun x ->
         match x with
         | None -> false
         | _ -> true)

let bad_test_1 = [
  "bad test 1" >:: (fun _ -> assert_equal 
  (List.length @@ Feeder.lookback "ETH" 34) (34));
]
let bad_test_2 = [
  "bad test 2" >:: (fun _ -> assert_equal 
  (List.length @@ Feeder.lookback "ETH" 52) (52));
]

let suite =
  let _ = Feeder.init_reader () in
  "test suite for indicators"
  >::: List.flatten [ ma_tests; obv_tests (* rsi_tests; *) ]

let num_feedback_tests = ref 0

let feeder_lookback_test name n expected =
  let n = List.length @@ multiple_next_day n in
  if n <> expected then
    print_endline @@ "F"
  else num_feedback_tests := !num_feedback_tests + 1;
   print_string "."

let multiple_next_day_test name n expected =
  let n = List.length @@ multiple_next_day n in
  if n <> expected then
    print_endline @@ "F"
  else num_feedback_tests := !num_feedback_tests + 1; print_string "."

let run_feeder_tests f tests = List.iter f tests

let lookback_tests =
  [
    ("lookback normal amount", 12, 12);
    ("lookback normal amount 2", 24, 24);
    ("lookback large amount", 99, 99);
    ("lookback large amount 2", 150, 150);
    ("lookback large amount even", 202, 202);
    ("lookback large amount odd", 305, 305);
    ("lookback max amount", 365, 365);
    ("lookback edge case 0", 0, 0);
    ("lookback edge case 1", 1, 1)
    (* ("lookback throw error", 400, -1); *);
  ]

let next_day_quantity_tests =
  [
    ("next day normal amount", 12, 12);
    ("next day normal amount 2", 24, 24);
    ("next day large amount", 99, 99);
    ("next day large amount 2", 150, 150);
    ("next day large amount even", 202, 202);
    ("next day large amount odd", 305, 305);
    ("next day max amount minus 1", 365, 365);
    ("next day edge case 0", 0, 0);
    ("next day edge case 1", 1, 1);
    ("next day edge case max amount", 366, 366);
    ("next day exceed max amount", 400, 366);
  ]

(* low price should be <= than all other price values, and max price
   should be >= all other price values*)
let validate_feeder = function
  | None ->
      print_endline
        "\n====expected day of market data but instead got None===\n"
  | Some d ->
      let low = Feeder.low d in
      let low_valid =
        if
          low <= Feeder.open_price d
          && low <= Feeder.close_price d
          && low <= Feeder.high d
        then true
        else false
      in
      let high = Feeder.high d in
      let high_valid =
        if high >= Feeder.open_price d && high >= Feeder.close_price d
        then true
        else false
      in
      if low_valid && high_valid then (num_feedback_tests := !num_feedback_tests + 1; print_string ".")
      else
        print_endline @@ "F"

let time_initial = Unix.gettimeofday ()

let _ = multiple_next_day 366 |> List.iter validate_feeder

let _ =
  run_feeder_tests
    (fun (name, n, expect) -> feeder_lookback_test name n expect)
    lookback_tests

let _ =
  run_feeder_tests
    (fun (name, n, expect) -> multiple_next_day_test name n expect)
    next_day_quantity_tests

let time_final = Unix.gettimeofday ()

let delta_time = (time_final -. time_initial)

let _ = "\nRan "^(string_of_int !num_feedback_tests)^
" feeder tests in: "^(string_of_float delta_time)^
" seconds" |> print_endline

let _ = run_test_tt_main suite
