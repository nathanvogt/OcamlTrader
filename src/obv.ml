let update_val
    (prev_obv : int)
    (prev_close : float)
    (vol : int)
    (close : float)
    coin =
  if close > prev_close then (prev_obv + vol, close)
  else if close = prev_close then (prev_obv, close)
  else (prev_obv - vol, close)
