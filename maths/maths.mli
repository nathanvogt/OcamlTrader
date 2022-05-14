(** Mathematical functionality utility library.

    Provides commonly used functions throughout our trading algorithms.

    Note: We acknowledge that similar functionality for some included
    functions can be found in the Ocaml Float module. However, our
    implementations differ from the provided module in that it takes
    into consideration extra algorithm-specific parameters such as range
    and spread. *)

val trend_proximity : float -> float -> float
(** [trend_proximity s d] returns a float indicating whether to buy
    (positive value) or sell (negative value) from the difference [d]
    between price and trend line with falloff range [s]. *)

val e : float
(** euler's constant *)

val sigmoid : float -> float
(** [sigmoid x] applies sigmoid function to x *)

val tanh_off : float -> float -> float
(** [tanh_off r s] applies tanh to x with range [r] and spread [s] but
    considers an additional offset . *)

val tanh : float -> float -> float -> float
(** [tanh r s x] applies tanh to x with range [r] and spread [s]. *)
