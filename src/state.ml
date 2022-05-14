include Feeder
include ANSITerminal
include Trend

let coin_name_const = "ETH"

(* subtype containing data from feeder *)
type feeder_data = {
  op : float;
  high : float;
  low : float;
  close : float;
  volume : float;
  date : string;
}

type indicator_type =
  | RSI of float * float * float * float * float
  | MACD of float * float * float * float
  | OBV of int * float
  | SO of float * float list

type decision =
  | Buy
  | Wait
  | Sell

(* sub type containing information about trading account *)
type account = {
  market_value : (string * float * float) list;
  (* list of (coin_name, average_value, positions held (may be
     fractional)) *)
  cash_balance : float; (* current amount held *)
  positions : (string * float) list; (* (coin, amnt) *)
  p_l : float; (* total profit/loss *)
}

(* price of coin at time state is initiated *)
let initial_coin_price = ref 0.

type trends = { crits : Trend.crit_point list }

type t = {
  data : (string * feeder_data) list;
  indicators : indicator_type list;
  acc_info : account;
  trends : trends;
}

exception InvalidCoin of string
exception NoSuchCoin of string

(* ------ getting raw data ------*)

(* helper function getting data for specific coin *)
let feeder_data f =
  {
    op = Feeder.open_price f;
    high = Feeder.high f;
    low = Feeder.low f;
    close = Feeder.close_price f;
    volume = Feeder.volume f;
    date = Feeder.date f;
  }

(* helper function making sure coin name is valid by checking length of
   filter list*)
let is_valid_coin coin_name j =
  if List.length j > 0 then j else raise (InvalidCoin coin_name)

(* ------ initializing indicators ------ *)
exception InvalidIndicator of string

(* initialize indicator values to prevent dependency between modules *)
let initialize = function
  | RSI (0., 0., 0., 0., 0.) ->
      let prev_avg_gain, prev_avg_loss =
        Feeder.lookback coin_name_const 14 |> Ma.gain_loss (0., 0.)
      in
      RSI (0., 0., 0., prev_avg_gain, prev_avg_loss)
  | MACD (0., 0., 0., 0.) ->
      let ema_26 = Feeder.lookback coin_name_const 26 |> Ma.avg in
      let ema_12 = Feeder.lookback coin_name_const 12 |> Ma.avg in
      MACD (0., 0., ema_12, ema_26)
  | OBV (0, 0.) as obv -> obv
  | SO (0., []) -> SO (0., Feeder.lookback coin_name_const 14)
  | _ -> raise (Failure "Indicator initialization error")

(* recursive helpfer function to initiate indicators *)
let rec initiate_indicators_aux = function
  | [] -> []
  | h :: t ->
      if h = "RSI" then
        initialize (RSI (0., 0., 0., 0., 0.))
        :: initiate_indicators_aux t
      else if h = "MACD" then
        initialize (MACD (0., 0., 0., 0.)) :: initiate_indicators_aux t
      else if h = "OBV" then
        initialize (OBV (0, 0.)) :: initiate_indicators_aux t
      else if h = "SO" then
        initialize (SO (0., [])) :: initiate_indicators_aux t
      else raise (InvalidIndicator h)

(* helper function taking in string list of indicators and returning
   list of indicator_type *)
let initiate_indicators indic_names =
  initiate_indicators_aux indic_names

(* ------ initializing account info ------ *)
(* helper function receiving new data and name of coin data is
   associated with. returns association list of previous state except
   with specific coin changed. *)
let rec update_specific_coin coin_name new_data = function
  | [] -> []
  | (k, v) :: t ->
      if k = coin_name then (k, new_data) :: t
      else (k, v) :: update_specific_coin coin_name new_data t

(* helper function creating initial state of our trader *)
let init_account budget =
  {
    market_value = [ (coin_name_const, 0.0, 0.0) ];
    cash_balance = budget;
    positions = [];
    p_l = 0.0;
  }

(* ------ initalizing actual state ------*)
let init_state indic_names budget f =
  let ret =
    {
      data = [ (Feeder.coin_name f, feeder_data f) ];
      indicators = initiate_indicators indic_names;
      acc_info = init_account budget;
      trends =
        { crits = Trend.crit_points_days @@ Feeder.lookback "ETH" 360 };
    }
  in
  initial_coin_price := Feeder.close_price f;
  ret

(* ------- functions to access data ------- *)
(* helper type to prevent typos when requesting specific data type *)
type data_float_type =
  | High
  | Low
  | Open
  | Close
  | Volume

(* helper function retrieving data to be passed to indicators *)
let rec get_data_aux coin (data_type : data_float_type) = function
  | [] -> raise (NoSuchCoin coin)
  | (cn, data) :: t ->
      if cn = coin then
        match data_type with
        | High -> data.high
        | Low -> data.low
        | Open -> data.op
        | Close -> data.close
        | Volume -> data.volume
      else get_data_aux coin data_type t

(* separate helper function for date due to type mistmatch *)
let rec get_curr_date_aux coin = function
  | [] -> raise (NoSuchCoin coin)
  | (cn, data) :: t ->
      if cn = coin then data.date else get_curr_date_aux coin t

(* helper function getting either average position value of coin_name if
   get_avg_pos is true, or position size if get_avg_pos is false *)
let rec get_market_val_aux coin (get_avg_pos : bool) = function
  | [] ->
      print_endline "Not a valid coin name";
      0.0
  | (cn, avg_pos, pos_val) :: t ->
      if coin = cn then if get_avg_pos then avg_pos else pos_val
      else get_market_val_aux coin get_avg_pos t

let crit_points st = st.trends.crits
let price_high st coin_name = get_data_aux coin_name High st.data
let price_low st coin_name = get_data_aux coin_name Low st.data
let price_open st coin_name = get_data_aux coin_name Open st.data
let price_close st coin_name = get_data_aux coin_name Close st.data
let price_vol st coin_name = get_data_aux coin_name Volume st.data
let curr_date st coin_name = get_curr_date_aux coin_name st.data

let avg_position_val st coin_name =
  get_market_val_aux coin_name true st.acc_info.market_value

let positions_held st coin_name =
  get_market_val_aux coin_name false st.acc_info.market_value

(* ------- functions to be used by main ------- *)
(* helper function to return updated RSI *)
let return_new_rsi
    st
    prev_rsi
    prev_price_close
    prev_avg_gain
    prev_avg_loss =
  let curr_rsi, curr_price, prev_price, curr_avg_gain, curr_avg_loss =
    Rsi.update_val prev_rsi
      (price_close st coin_name_const)
      prev_price_close prev_avg_gain prev_avg_loss coin_name_const
  in
  RSI (curr_rsi, curr_price, prev_price, curr_avg_gain, curr_avg_loss)

(* helper funciton to return updated MACD *)
let return_new_macd st prev_macd ema_12 ema_26 =
  let curr_macd, curr_price, curr_avg_gain, curr_avg_loss =
    Macd.update_val prev_macd
      (price_close st coin_name_const)
      ema_12 ema_26 coin_name_const
  in
  MACD (curr_macd, curr_price, curr_avg_gain, curr_avg_loss)

(* helper function returning updated obv *)
let return_new_obv st prev_obv prev_close =
  let vol = int_of_float (price_vol st coin_name_const) in
  let close = price_close st coin_name_const in
  let curr_obv, curr_close =
    Obv.update_val prev_obv prev_close vol close coin_name_const
  in
  OBV (curr_obv, curr_close)

(* helper function returning updated so *)
let return_new_so prev_so =
  let curr_so, _ =
    So.update_val prev_so
      (Feeder.lookback coin_name_const 14)
      coin_name_const
  in
  SO (curr_so, Feeder.lookback coin_name_const 14)

(* helper function pattern matching against indic_list and calling
   expressions from other indicator modules. Passes in state as
   parameter so getters and setters can be used. *)
let new_indic_val (st : t) = function
  | RSI (prev_rsi, prev_price_close, _, prev_avg_gain, prev_avg_loss) ->
      return_new_rsi st prev_rsi prev_price_close prev_avg_gain
        prev_avg_loss
  | MACD (prev_macd, _, ema_12, ema_26) ->
      return_new_macd st prev_macd ema_12 ema_26
  | OBV (prev_obv, prev_close) -> return_new_obv st prev_obv prev_close
  | SO (prev_so, _) -> return_new_so prev_so

(* helper function receiving new data and calling indicator functions to
   update indicator field *)
let rec update_indicators (state : t) = function
  | [] -> []
  | h :: t -> new_indic_val state h :: update_indicators state t

let update_data st f =
  let new_data = feeder_data f in
  {
    st with
    data = update_specific_coin (Feeder.coin_name f) new_data st.data;
    indicators = update_indicators st st.indicators;
  }
(* later can add update acc_info as we buy/sell*)

let indicator_values st = st.indicators

(* helper function matching on requested data type to return of type
   float *)
let req_float_data data = function
  | "Open" -> data.op
  | "High" -> data.high
  | "Low" -> data.low
  | "Close" -> data.close
  | "Volume" -> data.volume
  | _ -> 0.0

(* helper function returning float data from specific coin *)
let rec spec_data coin_name requested_data = function
  | [] -> raise @@ NoSuchCoin coin_name
  | (k, v) :: t ->
      if k = coin_name then req_float_data v requested_data
      else spec_data coin_name requested_data t

(* helper function returning string representation of date of coin *)
let rec get_date coin_name = function
  | [] -> raise @@ NoSuchCoin coin_name
  | (k, v) :: t ->
      if k = coin_name then v.date else get_date coin_name t

let data_print coin_name st =
  "Coin: " ^ coin_name ^ "\n" ^ "Date: "
  ^ get_date coin_name st.data
  ^ "\n" ^ "Open: "
  ^ string_of_float (spec_data coin_name "Open" st.data)
  ^ "\n" ^ "High: "
  ^ string_of_float (spec_data coin_name "High" st.data)
  ^ "\n" ^ "Low: "
  ^ string_of_float (spec_data coin_name "Low" st.data)
  ^ "\n" ^ "Close: "
  ^ string_of_float (spec_data coin_name "Close" st.data)
  ^ "\n" ^ "Volume: "
  ^ string_of_float (spec_data coin_name "Volume" st.data)
  ^ "\n"

(* helper function taking in association list of positions and returning
   string representation*)
let rec positions_to_string = function
  | [] -> ""
  | (name, price) :: t ->
      "Bought " ^ name ^ " at " ^ string_of_float price

(* helper function getting closing price from data list in state *)
let rec get_closing_price coin_name = function
  | [] -> 0.0
  | (k, v) :: t ->
      if k = coin_name then v.close else get_closing_price coin_name t

(* helper function updating market_value with buy order *)
let rec market_value_buy coin_name buy_price = function
  | [] -> []
  | ((cn, avg_price, positions_held) as h) :: t ->
      if coin_name = cn then
        ( cn,
          ((positions_held *. avg_price) +. buy_price)
          /. (positions_held +. 1.),
          positions_held +. 1. )
        :: t
      else h :: market_value_buy coin_name buy_price t

(* helper function return copy of state account with bought coin *)
let buy_account acc buy_price =
  {
    acc with
    positions = (coin_name_const, buy_price) :: acc.positions;
    cash_balance = acc.cash_balance -. buy_price;
    market_value =
      market_value_buy coin_name_const buy_price acc.market_value;
  }

(* helper funciton returning tuple of account sold and profit/loss from
   transaction *)
let rec sell_acc_aux sell_price cname pos =
  match pos with
  | [] -> ([], 0.0)
  | (name, bought_price) :: t ->
      if cname = name then (t, sell_price -. bought_price)
      else sell_acc_aux sell_price cname t

(* helper function getting last position of [coin_name] from positions
   list. makes no modifications to positions list *)
let rec pop_last_pos coin_name = function
  | [] -> 0.
  | (cn, pos_val) :: t ->
      if coin_name = cn then pos_val else pop_last_pos coin_name t

(* helper function updating market_value with sell order *)
let rec market_value_sell
    coin_name
    sell_price
    (position_list : (string * float) list) = function
  | [] -> []
  | ((cn, avg_price, positions_held) as h) :: t ->
      if coin_name = cn then
        if positions_held -. 1. = 0. || List.length position_list <= 0
        then (cn, 0., 0.) :: t
        else
          ( cn,
            ((positions_held *. avg_price)
            -. pop_last_pos coin_name position_list)
            /. (positions_held -. 1.),
            positions_held -. 1. )
          :: t
      else h :: market_value_buy coin_name sell_price t

(* helper funciton selling position in account *)
let sell_account acc sell_price =
  let pos_pl_tup =
    sell_acc_aux sell_price coin_name_const acc.positions
  in
  {
    market_value =
      market_value_sell coin_name_const sell_price acc.positions
        acc.market_value;
    positions = fst pos_pl_tup;
    p_l = acc.p_l +. snd pos_pl_tup;
    cash_balance = acc.cash_balance +. sell_price;
  }

(* helper function making sure enough money is in account to buy
   currency. can be adjusted to buy fractional shares *)
let valid_buy acc buy_price = acc.cash_balance >= buy_price

(* helper function making sure there is position to sell currency.*)
let valid_sell acc = acc.positions <> []

(* helper function making adjustments to [st] for when algorithm decides
   to buy. *)
let buy_decision st =
  if valid_buy st.acc_info (get_closing_price coin_name_const st.data)
  then
    let buy_state =
      {
        st with
        acc_info =
          buy_account st.acc_info
            (get_closing_price coin_name_const st.data);
      }
    in
    (buy_state, buy_state.acc_info.p_l)
  else begin
    ANSITerminal.print_string [ ANSITerminal.yellow ]
    @@ "Insufficient funds.\n";
    (st, st.acc_info.p_l)
  end

(* helper function making adjustments to [st] for when algorithm decides
   to sell. *)
let sell_decision st =
  if valid_sell st.acc_info then
    let sell_state =
      {
        st with
        acc_info =
          sell_account st.acc_info
            (get_closing_price coin_name_const st.data);
      }
    in
    (sell_state, sell_state.acc_info.p_l)
  else begin
    ANSITerminal.print_string [ ANSITerminal.yellow ]
    @@ "No positions to sell.\n";
    (st, st.acc_info.p_l)
  end

let decision_action st suppress_print = function
  | Buy -> buy_decision st
  | Sell -> sell_decision st
  | Wait -> (st, st.acc_info.p_l)

let all_time_profit st coin_name start_position_size =
  start_position_size
  *. (price_close st coin_name -. !initial_coin_price)

let rec get_rsi st =
  let indicators = indicator_values st in
  let rec helper = function
    | [] -> failwith "couldn't find RSI"
    | h :: t -> (
        match h with
        | RSI (v, _, _, _, _) -> v
        | _ -> helper t)
  in
  helper indicators

let rec get_macd st =
  let indicators = indicator_values st in
  let rec helper = function
    | [] -> failwith "couldn't find MACD"
    | h :: t -> (
        match h with
        | MACD (v, _, _, _) -> v
        | _ -> helper t)
  in
  helper indicators

let rec get_obv st =
  let indicators = indicator_values st in
  let rec helper = function
    | [] -> failwith "couldn't find OBV"
    | h :: t -> (
        match h with
        | OBV (v, _) -> float_of_int v
        | _ -> helper t)
  in
  helper indicators

let rec get_so st =
  let indicators = indicator_values st in
  let rec helper = function
    | [] -> failwith "couldn't find OBV"
    | h :: t -> (
        match h with
        | SO (v, _) -> v
        | _ -> helper t)
  in
  helper indicators

let get_held_profit st coin_name =
  avg_position_val st coin_name *. positions_held st coin_name
