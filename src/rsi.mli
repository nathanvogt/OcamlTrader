(** [rsi] implements the RSI indicator *)

val rs_to_rsi : float -> float -> float
(** [rs_to_rsi gain loss] takes in [gain] and [loss] to compute the RS
    value, then uses the RS value to compute the RSI value *)

val rsi_recurse : float list -> float -> float -> float list
(** [rsi_recurse lst yesterday_gain yesterday_loss] takes in the changes
    between elements in list [lst], along with yesterday's average gain
    [yesterday_gain] and yesterday's average loss [yesterday_loss] to
    compute the RSI values for the given [lst]; this function computes
    the RSI values after 14 days *)

val rsi : float list -> float list
(** [rsi lst] takes in a float list [lst] and returns the RSI values for
    that period; for the first 14 days there are no RSI values as the
    lookback period is 14 days*)

<<<<<<< HEAD
val update_val :
  float -> float -> float -> float -> float -> string -> float
(** [updateval price_close prev_price_close prev_rsi prev_avg_gain prev_avg_loss coin]
    takes in state [st], yesterday's closing price [prev_val], and coin
    name [coin]; returns today's RSI value *)
=======
val update_val : float -> float -> float -> float -> string -> float
(** [updateval price_close prev_price_close prev_rsi prev_avg_gain prev_avg_loss coin]
    takes in computes today's RSI value based on today's price and
    parameters from yesterday, returns a tuple of today's RSI, today's
    average gain, and today's average loss in this order *)
>>>>>>> ecb1605cded4522cd141799805c316b9b0b20535
