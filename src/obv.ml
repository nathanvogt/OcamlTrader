let update_val prev_obv prev_close vol close coin =
  if close > prev_close then (close, prev_obv + vol)
  else if close = prev_close then (close, prev_obv)
  else (prev_obv - vol, close)
