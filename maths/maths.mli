(** Mathematical functionality utility library.

    Provides commonly used functions throughout 
    our trading algorithms.
*)

(* todo: add more funciton parameters *)
val trend_proximity : float -> float -> float
(** [trend_proximity s d] returns a float indicating
whether to buy (positive value) or sell (negative value)
from the difference [d] between price and trend line with
falloff range [s]. *)

val e : float
(** euler's constant *)

val sigmoid : float -> float 
(** [sigmoid x] applies sigmoid function to x *)

val tanh : float -> float -> float -> float
(** [tanh r s x] applies tanh to x
with range [r] and spread [s]. *)

val abs : float -> float
(** [abs x] takes the absolute value of x. *)