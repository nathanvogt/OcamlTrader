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

let update_val st prev_val coin =
  if !initialized then (
    let price = State.price_close st coin in
    let ema_12 = Ma.ema_today price 12 !yesterday_price in
    let ema_26 = Ma.ema_today price 26 !yesterday_price in
    prev_ema_12 := ema_12;
    prev_ema_26 := ema_26;
    yesterday_price := price;
    ema_12 -. ema_26)
  else
    let ema_12 = Feeder.lookback coin 12 |> Ma.avg in
    let ema_26 = Feeder.lookback coin 26 |> Ma.avg in
    prev_ema_12 := ema_12;
    prev_ema_26 := ema_26;
    yesterday_price := Feeder.lookback coin 12 |> List.rev |> List.hd;
    initialized := true;
    ema_12 -. ema_26

let initialize () =
  let ema_26 = Feeder.lookback "RSI" 26 |> Ma.avg in
  let ema_12 = Feeder.lookback "RSI" 12 |> Ma.avg in
  ema_12 -. ema_26
