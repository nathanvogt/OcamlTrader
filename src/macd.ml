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

(* Unused in [update_val]: [prev_macd], [coin] *)