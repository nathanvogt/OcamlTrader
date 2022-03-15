(** [macd] contains the implementation for the MACD moving indicator,
    which is [12 day EMA - 26 day EMA] *)

val macd : float list -> float list
(** [macd lst] takes in a float list [lst] and returns the resulting
    list of MACD values; Precondition: requires [lst] to have at least
    26 elements *)
