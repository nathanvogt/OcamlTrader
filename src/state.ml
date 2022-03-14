(* sub type for raw data from feeder *)
type feeder_data = {
  op : float;
  high : float;
  low : float;
  close : float;
  volume : int;
}

(* indicator type *)
type indicator_type =
  | RSI of float
  | MACD of float

(* sub type containing record of all indicators *)
type indicator = {
  indic_list : indicator_type list;
      (* list so it's easier to just add new types*)
}

(* sub type containing information about trader itself *)
(* later: which of these might benefit from mutable? *)
type account = {
  market_value : float; (* current amount invested *)
  cash_balance : float; (* current amount held *)
  positions : (string * float) list; (* (coin, amnt) *)
  p_l : float; (* total profit/loss *)
}

type t = {
  data : feeder_data;
  indicators : indicator;
  acc_info : account;
}

(* ------ getting raw data ------*)
open Yojson.Basic.Util

let data_of_json j =
  {
    op = j |> member "open" |> to_float;
    high = j |> member "high" |> to_float;
    low = j |> member "low" |> to_float;
    close = j |> member "close" |> to_float;
    volume = j |> member "volume" |> to_int;
  }

(* ------ initializing indicators ------ *)
exception InvalidIndicator of string

(* "globaly" variables deciding beginning budget for ease of
   adjustment *)
let init_budget = 100000.0

(* recursive helpfer function to initiate indicators *)
let rec initiate_indicators_aux = function
  | [] -> []
  | h :: t ->
      if h = "RSI" then RSI 0.0 :: initiate_indicators_aux t
      else if h = "MACD" then MACD 0.0 :: initiate_indicators_aux t
      else raise (InvalidIndicator h)

(* helper function taking in string list of indicators and returning
   list of indicator_type *)
let initiate_indicators indic_names =
  { indic_list = initiate_indicators_aux indic_names }

(* ------ initializing account info ------ *)
(* helper function creating initial state of our trader *)
(* TODO: could be edited later to not be reset everytime program is
   run *)
let init_account budget =
  {
    market_value = 0.0;
    cash_balance = (if budget = -1.0 then init_budget else budget);
    positions = [];
    p_l = 0.0;
  }

(* ------ initalizing actual state *)
let init_state indic_names budget json =
  {
    data = data_of_json json;
    indicators = initiate_indicators indic_names;
    acc_info = init_account budget;
  }
