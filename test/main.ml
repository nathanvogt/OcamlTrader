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

(** [pp_five_float s] pretty-prints float * float * float * float *
    float [s]. *)
let pp_five_float (s1, s2, s3, s4, s5) =
  "\"" ^ string_of_float s1 ^ ", " ^ string_of_float s2 ^ ", "
  ^ string_of_float s3 ^ ", " ^ string_of_float s4 ^ ", "
  ^ string_of_float s5 ^ "\""

(** [pp_four_float s] pretty-prints float * float * float * float [s]. *)
let pp_four_float (s1, s2, s3, s4) =
  "\"" ^ string_of_float s1 ^ ", " ^ string_of_float s2 ^ ", "
  ^ string_of_float s3 ^ ", " ^ string_of_float s4 ^ "\""

(** [pp_float_int s] pretty-prints float * int [s]. *)
let pp_float_int s =
  "\"" ^ string_of_float (fst s) ^ ", " ^ string_of_int (snd s) ^ "\""

(** [pp_int_float s] pretty-prints int * float [s]. *)
let pp_int_float s =
  "\"" ^ string_of_int (fst s) ^ ", " ^ string_of_float (snd s) ^ "\""

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

(** [obv_test] tests Obv.update_val *)
let obv_test
    (name : string)
    (prev_obv : int)
    (prev_close : float)
    (vol : int)
    (close : float)
    (coin : string)
    (expected_output : int * float) =
  name >:: fun _ ->
  assert_equal ~printer:pp_int_float expected_output
    (Obv.update_val prev_obv prev_close vol close coin)

let obv_tests =
  [
    (* pricing for days 1-10 taken from
       https://www.investopedia.com/terms/o/onbalancevolume.asp*)
    obv_test
      "OBV Test of Day 1: closing price = $10, volume = 25,200 shares; \
       Expected OBV = 0"
      25200 10.01 25200 10. "ETH" (0, 10.);
    obv_test
      "OBV Test of Day 2: closing price = $10.15, volume = 30,000 \
       shares; Expected OBV = 30,000"
      0 10. 30000 10.15 "ETH" (30000, 10.15);
    obv_test
      "OBV Test of Day 3: closing price = $10.17, volume = 25,600 \
       shares; Expected OBV = 55,600"
      30000 10.15 25600 10.17 "ETH" (55600, 10.17);
    obv_test
      "OBV Test of Day 4: closing price = $10.13, volume = 32,000 \
       shares; Expected OBV = 23,600"
      55600 10.17 32000 10.13 "ETH" (23600, 10.13);
    obv_test
      "OBV Test of Day 5: closing price = $10.11, volume = 23,000 \
       shares; Expected OBV = 600"
      23600 10.13 23000 10.11 "ETH" (600, 10.11);
    obv_test
      "OBV Test of Day 6: closing price = $10.15, volume = 40,000 \
       shares; Expected OBV = 40,600"
      600 10.11 40000 10.15 "ETH" (40600, 10.15);
    obv_test
      "OBV Test of Day 7: closing price = $10.20, volume = 36,000 \
       shares; Expected OBV = 76,600"
      40600 10.15 36000 10.20 "ETH" (76600, 10.20);
    obv_test
      "OBV Test of Day 8: closing price = $10.20, volume = 20,500 \
       shares; Expected OBV = 76,600"
      76600 10.20 20500 10.20 "ETH" (76600, 10.20);
    obv_test
      "OBV Test of Day 9: closing price = $10.22, volume = 23,000 \
       shares; Expected OBV = 99,600"
      76600 10.20 23000 10.22 "ETH" (99600, 10.22);
    obv_test
      "OBV Test of Day 10: closing price = $10.21, volume = 27,500 \
       shares; Expected OBV = 72,100"
      99600 10.22 27500 10.21 "ETH" (72100, 10.21);
    obv_test
      "OBV Test of Day 11: closing price = $10.24, volume = 32,500 \
       shares; Expected OBV = 104,600"
      72100 10.21 32500 10.24 "ETH" (104600, 10.24);
    obv_test
      "OBV Test of Day 12: closing price = $10.26, volume = 18,500 \
       shares; Expected OBV = 123100"
      104600 10.24 18500 10.26 "ETH" (123100, 10.26);
    obv_test
      "OBV Test of Day 13: closing price = $10.14, volume = 22,500 \
       shares; Expected OBV = 100600"
      123100 10.26 22500 10.14 "ETH" (100600, 10.14);
    obv_test
      "OBV Test of Day 14: closing price = $10.12, volume =  12,500 \
       shares; Expected OBV = 88100"
      100600 10.14 12500 10.12 "ETH" (88100, 10.12);
    obv_test
      "OBV Test of Day 15: closing price = $10.24, volume = 26,500 \
       shares; Expected OBV = 72,100"
      88100 10.12 26500 10.24 "ETH" (114600, 10.24);
  ]

(** [rsi_update_val_test] tests Rsi.update_val *)
let rsi_update_val_test
    (name : string)
    (prev_rsi : float)
    (price_close : float)
    (prev_price_close : float)
    (prev_avg_gain : float)
    (prev_avg_loss : float)
    (coin : string)
    (expected_output : float * float * float * float * float) =
  name >:: fun _ ->
  assert_equal (* ~printer:pp_five_float *) expected_output
    (Rsi.update_val prev_rsi price_close prev_price_close prev_avg_gain
       prev_avg_loss coin)

let rsi_update_val_tests =
  [ (* rsi_update_val_test "RSI [update_val] test for day 1" 70.53 46.28
       46. .24 .1 "ETH" (5., 5., 5., 5., 5.) *) ]

(* helper function to truncate floats *)
let truncatef x = snd (modf (x *. 1000.)) /. 1000.

(* helper function to truncate float tuples *)
let truncate4 (x1, x2, x3, x4) =
  (truncatef x1, truncatef x2, truncatef x3, truncatef x4)

(** [macd_update_val_test] tests Macd.update_val *)
let macd_update_val_test
    (name : string)
    (prev_close : float)
    (prev_macd : float)
    (ema_12 : float)
    (ema_26 : float)
    (coin : string)
    (expected_output : float * float * float * float) =
  name >:: fun _ ->
  assert_equal ~printer:pp_four_float expected_output
    (Macd.update_val prev_close prev_macd ema_12 ema_26 coin
    |> truncate4)

let macd_update_val_tests =
  [
    (* 15-day period MACD Test*)
    macd_update_val_test
      "MACD Day 1: Expected tuple of (-37., 0., 366., 404.)" 426.21
      (-5.108084059) 434.4544168 437.0762591 "ETH"
      (-37.492, 0., 366.829, 404.321);
    macd_update_val_test
      "MACD Day 2: Expected tuple of (-37.19, 0., 367.079, 404.269)"
      426.98 (-4.527494558) 434.6445065 436.9735732 "ETH"
      (-37.19, 0., 367.079, 404.269);
    macd_update_val_test
      "MACD Day 3: Expected tuple of (-36.958, 0., 367.213, 404.172)"
      435.69 (-3.387775176) 434.5961209 436.777753 "ETH"
      (-36.958, 0., 367.213, 404.172);
    macd_update_val_test
      "MACD Day 4: Expected tuple of (-37.041, 0., 366.712, 403.753)"
      434.33 (-2.59227244) 433.8582561 436.2608824 "ETH"
      (-37.041, 0., 366.712, 403.753);
    macd_update_val_test
      "MACD Day 5: Expected tuple of (-37.711, 0., 364.941, 402.652)"
      429.8 (-2.250613279) 431.7031398 435.0452615 "ETH"
      (-37.711, 0., 364.941, 402.652);
    macd_update_val_test
      "MACD Day 6: Expected tuple of (-37.843, 0., 364.183, 402.026)"
      419.85 (-2.55208695) 430.8626568 434.3930199 "ETH"
      (-37.843, 0., 364.183, 402.026);
    macd_update_val_test
      "MACD Day 7: Expected tuple of (-39.3, 0., 360.585, 399.886)"
      402.8 (-2.192262723) 426.5453249 432.0527962 "ETH"
      (-39.3, 0., 360.585, 399.886);
    macd_update_val_test
      "MACD Day 8: Expected tuple of (-41.138, 0., 355.919, 397.058)"
      392.05 (-3.335496669) 421.2383519 429.0896261 "ETH"
      (-41.138, 0., 355.919, 397.058);
    macd_update_val_test
      "MACD Day 9: Expected tuple of (-42.588, 0., 351.735, 394.323)"
      390.53 (-4.543439719) 416.51399 426.2333575 "ETH"
      (-42.588, 0., 351.735, 394.323);
    macd_update_val_test
      "MACD Day 10: Expected tuple of (-43.067, 0., 349.322, 392.39)"
      398.67 (-5.129226357) 413.7687608 424.1916273 "ETH"
      (-43.067, 0., 349.322, 392.39);
    macd_update_val_test
      "MACD Day 11: Expected tuple of (-42.785, 0., 348.399, 391.185)"
      406.13 (-4.666180327) 412.5935668 422.853729 "ETH"
      (-42.785, 0., 348.399, 391.185);
    macd_update_val_test
      "MACD Day 12: Expected tuple of (-42.436, 0., 347.634, 390.071)"
      408.38 (-3.602780783) 411.496095 421.5653046 "ETH"
      (-42.436, 0., 347.634, 390.071);
    macd_update_val_test
      "MACD Day 13: Expected tuple of (-41.868, 0., 347.363, 389.231)"
      417.2 (-2.729462587) 411.0166958 420.5886154 "ETH"
      (-41.868, 0., 347.363, 389.231);
    macd_update_val_test
      "MACD Day 14: Expected tuple of (-40.755, 0., 348.313, 389.069)"
      430.12 (-1.785738071) 411.9679734 420.3376068 "ETH"
      (-40.755, 0., 348.313, 389.069);
    macd_update_val_test
      "MACD Day 15: Expected tuple of (-38.958, 0., 350.879, 389.837)"
      442.78 (-0.466761561) 414.7605928 421.0622286 "ETH"
      (-38.958, 0., 350.879, 389.837);
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

let closes =
  Feeder.init_reader ();
  Feeder.lookback "ETH" 360
let crits = Trend.crit_points_days closes 
let filter_test name param expected = 
  (name >:: (fun _ ->
    assert_equal
    (Trend.filter_crit_points crits param 
  |> List.length)
  (expected)))

let filter_tests = [
  filter_test "filter everyting"
  99999. 1;
  filter_test "filter nothing"
  0. (List.length crits);
  filter_test "filter light"
  30. 79;
  filter_test "heavy filter"
  300. 32;
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

let bad_test_1 =
  [
    ( "bad test 1" >:: fun _ ->
      assert_equal (List.length @@ Feeder.lookback "ETH" 34) 34 );
  ]

let bad_test_2 =
  [
    ( "bad test 2" >:: fun _ ->
      assert_equal (List.length @@ Feeder.lookback "ETH" 52) 52 );
  ]

let suite =
  let _ = Feeder.init_reader () in
  "test suite for indicators"
  >::: List.flatten
         [
           ma_tests;
           obv_tests;
           macd_update_val_tests (* rsi_tests *);
           filter_tests;
         ]

let num_feedback_tests = ref 0

let feeder_lookback_test name n expected =
  let n = List.length @@ multiple_next_day n in
  num_feedback_tests := !num_feedback_tests + 1;
  if n <> expected then print_endline @@ "F" else print_string "."

let multiple_next_day_test name n expected =
  let n = List.length @@ multiple_next_day n in
  num_feedback_tests := !num_feedback_tests + 1;
  if n <> expected then print_endline @@ "F" else print_string "."

let multiple_next_day_test name n expected =
  let n = List.length @@ multiple_next_day n in
  if n <> expected then print_endline @@ "F"
  else num_feedback_tests := !num_feedback_tests + 1;
  print_string "."

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
      num_feedback_tests := !num_feedback_tests + 1;
      if low_valid && high_valid then print_string "."
      else print_endline @@ "F"

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
let delta_time = (time_final -. time_initial) *. 100.

let _ =
  "\nRan: "
  ^ string_of_int !num_feedback_tests
  ^ " feeder tests in: "
  ^ string_of_float delta_time
  ^ " seconds"
  |> print_endline

let _ = run_test_tt_main suite
