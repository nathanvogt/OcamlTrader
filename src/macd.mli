(** [macd] contains the implementation for the MACD moving indicator,
    which is [12 day EMA - 26 day EMA] *)

val update_val :
  float ->
  float ->
  float ->
  float ->
  string ->
  float * float * float * float
(** [update_val prev_close prev_macd ema_12 ema_26 coin] takes in state
    [st], yesterday's closing price [prev_val], and coin name [coin];
    returns today's MACD value *)
