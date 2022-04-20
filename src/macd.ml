(** [smoothing] takes in the integer [smoothing] value from module [Ma]*)
let smoothing = Ma.smoothing

(** [initialized] is a boolean value that returns [true] when the MACD
    module has received the first 26 days of values.*)
let initialized = ref false

(** [prev_ema_26] is the yesterday's 26-day EMA value *)
let prev_ema_26 = ref 0.

(** [prev_ema_12] is the yesterday's 12-day EMA value *)
let prev_ema_12 = ref 0.

(** [yesterday_price] is yesterday's closing price for a given coin *)
let yesterday_price = ref 0.0

let macd lst =
  let ema_twelve = Ma.ema lst 12 in
  let len = List.length ema_twelve in
  let twelve_day = Ma.sublist 14 (len - 1) ema_twelve in
  let twenty_six_day = Ma.ema lst 26 in
  List.map2 (fun x y -> x -. y) twelve_day twenty_six_day

(** [ema_multiplier observations] takes in [observations], the number of
    days for an EMA period, and returns the corresponding EMA multiplier *)
let ema_multiplier observations = 2. /. (float_of_int observations +. 1.)

let update_val prev_macd price_close prev_ema_12 prev_ema_26 coin =
  let mult_26 = ema_multiplier 26 in
  let mult_12 = ema_multiplier 12 in
  let ema_26 =
    (price_close *. mult_26) +. (prev_ema_26 *. (1. -. mult_26))
  in
  let ema_12 =
    (price_close *. mult_12) +. (prev_ema_12 *. (1. -. mult_12))
  in
  let new_macd = ema_12 -. ema_26 in
  (new_macd, 0., ema_12, ema_26)
(* if !initialized then ( let price = prev_close in let ema_12 =
   Ma.ema_today price 12 !yesterday_price in let ema_26 = Ma.ema_today
   price 26 !yesterday_price in prev_ema_12 := ema_12; prev_ema_26 :=
   ema_26; yesterday_price := price; ema_12 -. ema_26) else let ema_12 =
   Feeder.lookback coin 12 |> Ma.avg in let ema_26 = Feeder.lookback
   coin 26 |> Ma.avg in prev_ema_12 := ema_12; prev_ema_26 := ema_26;
   yesterday_price := Feeder.lookback coin 12 |> List.rev |> List.hd;
   initialized := true; *)
(* @Michael NEED TO RETURN TUPLE of float * float * float *)
(* Unused in [update_val]: [prev_macd], [coin] *)