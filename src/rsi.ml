(********************************************************************
   RSI computations for lists
 ********************************************************************)
let rs_to_rsi gain loss = 100. -. (100. /. (1. +. (gain /. loss)))

let rec rsi_recurse lst yesterday_gain yesterday_loss =
  match lst with
  | [] -> []
  | h :: t ->
      let adjusted =
        if h >= 0. then
          let avg_gain = ((yesterday_gain *. 13.) +. h) /. 14. in
          let avg_loss = yesterday_loss *. 13. /. 14. in
          (avg_gain, avg_loss)
        else
          let avg_gain = yesterday_gain *. 13. /. 14. in
          let avg_loss = ((yesterday_loss *. 13.) -. h) /. 14. in
          (avg_gain, avg_loss)
      in
      let today_rsi = rs_to_rsi (fst adjusted) (snd adjusted) in
      today_rsi :: rsi_recurse t (fst adjusted) (snd adjusted)

let rsi lst =
  let diff = Ma.diff lst in
  let fourteen = Ma.sublist 0 12 diff in
  let gain_loss = Ma.gain_loss fourteen (0., 0.) in
  let avg_loss = snd gain_loss /. -14. in
  let avg_gain = fst gain_loss /. 14. in
  let after_fourteen = Ma.sublist 13 (List.length diff - 1) diff in
  rsi_recurse after_fourteen avg_gain avg_loss

(********************************************************************
   Day to Day RSI computations
 ********************************************************************)

(** [past_fourteen] is a boolean value that returns [true] when the RSI
    module has received the first fourteen days of values.*)
let past_fourteen = ref false

(** [prev_avg_gain] is yesterday's average price gain from the past 14
    days *)
let prev_avg_gain = ref 1.0

(** [prev_avg_loss] is yesterday's average price loss from the past 14
    days *)
let prev_avg_loss = ref 0.0

(** [yesterday_price] is yesterday's closing price for a given coin *)
let yesterday_price = ref 0.0

(** [price_change] calculates the change in price from yesterday to
    today *)
let price_change st coin =
  let today_price = State.price_close st "ETH" in
  let diff = today_price -. !yesterday_price in
  yesterday_price := diff;
  diff

(** [rsi_today] calculates today's rsi value given state [st] and
    coin_name [coin] *)
let rsi_today st coin =
  let price_change = price_change st coin in
  let gain = if price_change >= 0. then price_change else 0. in
  let loss = if price_change <= 0. then price_change *. -1. else 0. in
  let avg_gain = ((!prev_avg_gain *. 13.) +. gain) /. 14. in
  let avg_loss = ((!prev_avg_loss *. 13.) +. loss) /. 14. in
  let rs = avg_gain /. avg_loss in
  let rsi = 100. -. (100. /. (1. +. rs)) in
  prev_avg_gain := avg_gain;
  prev_avg_loss := avg_loss;
  rsi

let update_val st prev_val coin =
  if !past_fourteen then rsi_today st coin
  else
    let lookback = Feeder.lookback coin 14 in
    let gain_loss = Ma.gain_loss lookback (0., 0.) in
    let gain = fst gain_loss in
    let loss = snd gain_loss *. -1. in
    prev_avg_gain := gain;
    prev_avg_loss := loss;
    past_fourteen := true;
    yesterday_price := List.rev lookback |> List.hd;
    50.
