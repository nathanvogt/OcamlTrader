(** Reads raw price data

    This module is in charge of receiving raw price data. It can
    retrieve the market data of the current day and can also retrieve
    market data of an arbitrary number of previous days. *)

type t
(** Representation of a day of raw historical data *)

exception UninitializedReader
(** Exception for when attempting to retrieve historical data before
    [init_reader] has been called *)

external init_reader : unit -> unit = "initReader"
(** Initializes the file reader that reads raw historical data *)

external reset_reader : unit -> unit = "resetReader"
(** [reset_reader] resets file reader back to the start of historical
    data. Checks: reader must already be initialized *)

val next_day : unit -> t option
(** Returuns the next day of historical data *)

val to_string : t -> string

val open_price : t -> float
(** [open_price d] returns the opening price from the day of historical
    data [d] as a float *)

val close_price : t -> float
(** [close_price d] returns the closing price from the day of historical
    data [d] as a float *)

val high : t -> float
(** [high d] returns the highest price from the day of historical data
    [d] as a float *)

val low : t -> float
(** [low d] returns the lowest price from the day of historical data [d]
    as a float *)

val volume : t -> float
(** [volume d] returns the volume of trades from the day of historical
    data [d] as an int*)

val date : t -> string
(** [date d] returns the date of the day of the historical data [d] *)

val coin_name : t -> string
(** [coin_name d] returns the name of the cryptocurrency associated with
    historical data [d] *)

val lookback : string -> int -> float list
(** [lookback coin days] takes in a coin name [coin] and number of days
    [days]; returns a [float list] of the given coin's closing prices
    for past number of days specified by [days]; required for indicator
    initialization *)