open Indicator
open State 

val scale_values : (float * float) list -> float list -> float list
(** [scale_values wbs vs] scales each value in [vs]
by the corresponding weight and bias in [wbs] *)

val interp_rsi : float -> float 
(** [interp_rsi rsi] maps [rsi] to a buy/sell indicator
where a very negative value suggests to sell
and a very positive number suggests to buy. *)

val interp_macd : float -> float
(** [interp_macd macd] maps [macd] to a buy/sell indicator
where a very negative value suggests to sell
and a very positive number suggests to buy. *)

val interp_obv : float -> float
(** [intero_obv obv] maps [obv] to a buy/sell indicator
where a very negative value suggests to sell
and a very positive number suggests to buy. *)

val interp_so : float -> float
(** [interp_so so] maps [so] to a buy/sell indicator
where a very negative value suggests to sell
and a very positive number suggests to buy. *)