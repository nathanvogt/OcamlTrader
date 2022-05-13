let update_val
    (prev_rsi : float)
    (price_close : float)
    (prev_price_close : float)
    (prev_avg_gain : float)
    (prev_avg_loss : float)
    (coin : string) : float * float * float * float * float =
  let price_diff = price_close -. prev_price_close in
  let today_gain = if price_diff >= 0. then price_diff else 0. in
  let today_loss = if price_diff <= 0. then price_diff *. -1. else 0. in
  let avg_gain = ((13. *. prev_avg_gain) +. today_gain) /. 14. in
  let avg_loss = ((13. *. prev_avg_loss) +. today_loss) /. 14. in
  let rs = avg_gain /. avg_loss in
  let today_rsi = 100. -. (100. /. (1. +. rs)) in
  (today_rsi, price_close, 0., avg_gain, avg_loss)

(* Unused variables in [update_val]: [prev_rsi], [coin] *)