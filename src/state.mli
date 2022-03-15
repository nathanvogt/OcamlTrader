(** Representation of dynamic indicator state.

    This module represents the current state of our trading algorithm
    based on the raw values provided by feeder and the returned values
    of the indicators calculated. It reads data from the C code in
    /feeder as JSON and calls the indicator calculators. *)

(** Type defining name of indicator with float value produced by
    indicator *)
type indicator_type =
  | RSI of float
  | MACD of float

type t
(** The abstract type of values containing information on prices,
    indicators, and trader. *)

exception InvalidIndicator of string
(** Raised when an unknown indicator name is encountered. *)

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

val data_print : t -> string
(** [data_print st] returns string value representation of the data
    record in [st]*)