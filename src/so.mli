(** [so] represents Stochastic Oscillator and is bounded between 0 and
    100. If > 80, then the stock is overbought, which indicates that the
    price may drop. If < 20, then the stock is oversold, which indicates
    that the price may rise. *)

val update_val : float -> float list -> string -> float * float list
(** [update_val close past14 coin] takes in the closing price [clost] as
    a float along with the past 14 days of close prices as a float list
    [past14]. It also takes in coin name [coin] as a string. Returns a
    tuple of today's [SO] value and a float list of the past 14 close
    prices (including today) in chronological order. *)
