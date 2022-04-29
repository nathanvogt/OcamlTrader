(** Representation of dynamic indicator state.

    This module represents the current state of our trading algorithm
    based on the raw values provided by feeder and the returned values
    of the indicators calculated. It reads data from the C code in
    /feeder as JSON and calls the indicator calculators. *)

(** Type defining name of indicator with float value produced by
    indicator *)
type indicator_type =
  | RSI of float * float * float * float * float
  | MACD of float * float * float * float

(** Deicsion type to be passed after evaluation on training metrics, and
    for account to be updated accordingly. This is exposed by the state
    module so that it can be accessed by both main and state itself *)
type decision =
  | Buy
  | Wait
  | Sell

type t
(** The abstract type of values containing information on prices,
    indicators, and trader. *)

exception InvalidIndicator of string
(** Raised when an unknown indicator name is encountered. *)

exception NoSuchCoin of string
(** Raised when an unknown string name for coin is encountered *)

val init_state : string list -> float -> Feeder.t -> t
(** [init_state indic_names budget json] initializes state [t] based on
    the passed in parameters. In this state, no positions are held,
    profits are zero, and initial cash is equal to [budget]. *)

val update_data : t -> Feeder.t -> t
(** [update_data st json coin_name] returns a new copy of [st] with the
    updated data taken from [json] under [coin_name]*)

val indicator_values : t -> indicator_type list
(** [indicator_value st] returns the field of state containing list of
    indicators*)

val data_print : string -> t -> string
(** [data_print st] returns string value representation of the data
    record in [st]*)

val decision_action : t -> decision -> t * string
(** [decision_action st decision] takes in a type decision as decided by
    indicators from the main loops. Based on this decision, it returns a
    tuple with the first element being new state, and the second being a
    string representation of action executed *)

val price_high : t -> string -> float
(** [price_high st coin_name] returns high price of [coin_name] in [st]*)

val price_low : t -> string -> float
(** [price_low st coin_name] returns high price of [coin_name] in [st]*)

val price_open : t -> string -> float
(** [price_open st coin_name] returns high price of [coin_name] in [st]*)

val price_close : t -> string -> float
(** [price_close st coin_name] returns high price of [coin_name] in [st]*)

val price_vol : t -> string -> float
(** [price_volume st coin_name] returns high price of [coin_name] in
    [st]*)

val curr_date : t -> string -> string
(** [curr_date st coin_name] returns current date of [coin_name] in [st]*)
