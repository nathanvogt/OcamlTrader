(** [ma] contains the moving average functions for Exponential Moving
    Average (EMA) and Simple Moving Average (SMA) *)

val smoothing : int
(** [smoothing] is the integer value 2; the commonly accepeted smoothing
    value in the finance industry *)

exception OutOfRange
(** exception [OutOfRange] is thrown when the [sublist] method attempts
    to access an element of a list that is out of bounds *)

val sublist : int -> int -> 'a list -> 'a list
(** [sublist start stop lst] takes in a list [lst] along with the
    [start] and [stop] index locations; returns the resulting sublist;
    method altered from {{:https://stackoverflow.com/a/2717815} here} *)

val avg : float list -> float
(** [avg lst] is the [float] average of a given [float list] *)

val sma : float list -> int -> float list
(** [sma lst n] is the simple moving average with groups of [n]
    consecutive days; it takes in a float list [lst] and returns the
    averages of [n] days as a float list *)

val ema_recurse : float list -> int -> float -> float list
(** [ema_recurse lst n yesterday] takes in a float list [lst] and
    applies a recursive function based on the interval [n] days and the
    EMA price of yesterday [yesterday]; formula for EMA can be found
    here {{:https://www.educba.com/exponential-moving-average-formula/}
    here}*)

val ema : float list -> int -> float list
(** [ema list n] applies the EMA function on a given data set; it skips
    the first value of the list as it does not have an EMA value for
    yesterday [yesterday], and applies the [ema_recurse] function for
    days after the first day; takes in a given time internal [n], which
    accounts for the specified time interval *)

val diff : float list -> float list
(** [diff lst] takes in a list [lst] of size [n] and returns the
    differences of each pair of elements, resulting in [n-1] elements *)

val gain_loss : float list -> float * float -> float * float
(** [gain_loss lst (gain, loss)] takes in list [lst]; for each element
    in [lst] it adds it to [gain] if it has a value >= 0.; eotherwise it
    adds it to [loss] *)
