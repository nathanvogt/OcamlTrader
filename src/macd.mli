(** [macd] contains the implementation for the MACD moving indicator,
    which is [12 day EMA - 26 day EMA] *)

val macd : float list -> float list
(** [macd lst] takes in a float list [lst] and returns the resulting
    list of MACD values; Precondition: requires [lst] to have at least
    26 elements *)

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
