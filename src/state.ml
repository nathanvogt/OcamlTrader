include Feeder

type feeder_data = {
  op : float;
  high : float;
  low : float;
  close : float;
  volume : int;
}

type indicator_type =
  | RSI of float
  | MACD of float

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
  indicators : indicator_type list;
  acc_info : account;
}

(* ------ getting raw data ------*)
exception InvalidCoin of string

(* helper function initiating data in first step *)
let init_data =
  { op = 0.0; high = 0.0; low = 0.0; close = 0.0; volume = 0 }

(* helper function getting data for specific coin *)
let feeder_data f =
  {
    op = Feeder.open_price f;
    high = Feeder.close_price f;
    low = Feeder.low f;
    close = Feeder.high f;
    volume = Feeder.volume f;
  }

(* helper function making sure coin name is valid by checking length of
   filter list*)
let is_valid_coin coin_name j =
  if List.length j > 0 then j else raise (InvalidCoin coin_name)

(* TODO: could be buggy, should get coin_specfic data *)
(* let data_of_coin coin_name j = j |> member "coins" |> to_list |>
   filter_member coin_name |> is_valid_coin coin_name |> List.hd |>
   data_of_json *)

(* ------ initializing indicators ------ *)
exception InvalidIndicator of string

(* recursive helpfer function to initiate indicators *)
let rec initiate_indicators_aux = function
  | [] -> []
  | h :: t ->
      if h = "RSI" then RSI 80.0 :: initiate_indicators_aux t
      else if h = "MACD" then MACD 30.0 :: initiate_indicators_aux t
      else raise (InvalidIndicator h)

(* helper function taking in string list of indicators and returning
   list of indicator_type *)
let initiate_indicators indic_names =
  initiate_indicators_aux indic_names

(* ------ initializing account info ------ *)
(* helper function creating initial state of our trader *)
(* TODO: could be edited later to not be reset everytime program is
   run *)
let init_account budget =
  {
    market_value = 0.0;
    cash_balance = budget;
    positions = [];
    p_l = 0.0;
  }

(* ------ initalizing actual state ------*)
let init_state indic_names budget f =
  {
    data = feeder_data f;
    indicators = initiate_indicators indic_names;
    acc_info = init_account budget;
  }

(* ------- functions to be used by main ------- *)
(* helper function pattern matching against indic_list and calling
   expressions from other indicator modules *)
let new_indic_val data = function
  | RSI m -> RSI m (* (RSI Rsi.update_val data) *)
  | MACD m -> MACD m
(* (MACD Macd.update_val data) *)

(* helper function receiving new data and calling indicator functions to
   update indicator field *)
let rec update_indicators data = function
  | [] -> []
  | h :: t -> new_indic_val data h :: update_indicators data t

let update_data st f =
  let new_data = feeder_data f in
  {
    st with
    data = new_data;
    indicators = update_indicators new_data st.indicators;
  }
(* later can add update acc_info as we buy/sell*)

let indicator_values st = st.indicators

let data_print (st : t) =
  "Open: "
  ^ string_of_float st.data.op
  ^ "\n" ^ "High: "
  ^ string_of_float st.data.high
  ^ "\n" ^ "Low: "
  ^ string_of_float st.data.low
  ^ "\n" ^ "Close: "
  ^ string_of_float st.data.close
  ^ "\n" ^ "Volume: "
  ^ string_of_int st.data.volume
  ^ "\n"
