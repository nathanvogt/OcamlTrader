(** [rsi] implements the RSI indicator *)

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
