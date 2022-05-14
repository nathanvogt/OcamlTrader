(** Helps main decide whether to buy or sell.
    
    Takes in the raw values of all the factors that are
    used to decide whether to buy, sell, or wait on a cryptocurrency.
    Interprets these raw values and maps them into the range [-50, 50]
    where the more negative the number, the stronger the suggestion to sell,
    and the more positive the number, the stronger the suggestion to buy. Also
    combines and normalizes these mapped indicators into a final decision.
*)

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