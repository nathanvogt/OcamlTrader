(** Moving Average Convergence Divergence Indicator

    The MACD module contains the implementation for the MACD moving
    indicator, which is [12 day EMA - 26 day EMA]. Positive MACD value
    indicates increasing upward momentum, which indicates traders should
    refrain from taking short-term positions. Negative MACD value
    indicates that downward trend is stronger, hence it is not a good
    time to buy. Source: https://www.investopedia.com/trading/macd/*)

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
