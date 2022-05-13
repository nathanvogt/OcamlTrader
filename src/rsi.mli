(** [rsi] implements the RSI indicator, which has a range of 0 to 100.
    If < 30, stock is oversold and price may rise, while >70 indicates
    stock is overbought and price may dip. *)

val update_val :
  float ->
  float ->
  float ->
  float ->
  float ->
  string ->
  float * float * float * float * float
(** [updateval st prev_val prev_avg_gain prev_avg_loss coin] takes in
    state [st], yesterday's closing price [prev_val], and coin name
    [coin]; returns today's RSI value *)
