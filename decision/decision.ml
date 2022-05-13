(* Each indicator input into the decision formula should be normalized
into the range [-50, 50] where higher positive numbers
indicate the suggestion to buy and lower negative numbers 
indicate the suggestion to sell  *)

include State

(* ======= HYPERPARAMS ========= *)
let grid_up_hyperparam = 2.
let grid_down_hyperparam = Float.neg 20.
let grid_neutral_hyperparam = 0.
let spread_hyperparam = 7.0
let tanh_range_hyperparam = 50.0
let tanh_spread_hyperparam = 15.0

let trend_lines_weight = 3.
let trend_lines_bias = 1.
let grid_line_weight = 3.
let grid_line_bias = 1.
let rsi_weight = 0.1
let rsi_bias = 0.1
let macd_weight = 0.1
let macd_bias = 0.1
let obv_weight = 0.1
let obv_bias = 0.1
let so_weight = 0.1
let so_bias = 0.1
(* ============================= *)
(* helper function taking average based on pipeline ordering *)
let average den num = num /. den

(** [indication_naive indications acc ] takes in indication list
    [indications] average of all indicator measures. It is naive because
    no normalization is made, nor is there distinction between the type
    of indicator. *)
let rec accum_indication acc (indications : State.indicator_type list) =
  match indications with
  | [] -> acc
  | h :: t -> (
      match h with
      | RSI (rsi, _, _, _, _) -> indication_naive (acc +. rsi) t
      | MACD (macd, _, _, _) -> indication_naive (acc +. macd) t
      | OBV (obv, _) -> indication_naive (acc +. 0.) t)
(* place holder for now *)

(* heuristic taking simple average of indicators. *)
let weight_indicators st =
  State.indicator_values st
  |> indication_naive 0.0
  |> average @@ float_of_int (List.length indicators)

(* heuristic returning value of whether closing price has crossed any of
   upper or lower grid lines. returns float value similar to that of
   indicators between 0-100. *)
let grid_indicator price_close =
  if price_close > !grid_upper then grid_up_hyperparam
  else if price_close < !grid_lower then
    (* possible hyperparameter *)
    grid_down_hyperparam
  else grid_neutral_hyperparam

(* main function returning a combination of various indicators for a
   final decision *)
let indicator_comb st =
  let weights_biases = [
    (); (* trend lines *)
    (); (* grid indicators *)
  ]
  let price = State.price_close st "ETH" in
  [
    Trend.trend_line_indicator (State.crit_points st) price;
    grid_indicator (State.price_close st coin_name_const);
  ]
  |> List.fold_left (fun acc x -> acc +. x) 0.
  |> Maths.tanh tanh_range_hyperparam tanh_spread_hyperparam
  |> ( +. ) tanh_range_hyperparam
(* weight_indicators st +. grid_indicator (State.price_close st
   coin_name_const) *)

(* helper function receiving decision and taking corresponding action

   later need to make this smarter: don't sell when we don't have
   position sizes control the total budget it buys and sells -jun *)
let evaluate_indicators weight =
  if weight <= 30. then State.Buy
  else if weight <= 70. then State.Wait
  else State.Sell

let final_decision = State.Wait