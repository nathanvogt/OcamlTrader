(** Representation of dynamic indicator state.

    This module represents the current state of our trading algorithm
    based on the raw values provided by feeder and the returned values
    of the indicators calculated. It reads data from the C code in
    /feeder as JSON and calls the indicator calculators. *)

type t
(** The abstract type of values containing information on prices,
    indicators, and trader. *)

exception InvalidIndicator of string
(** Raised when an unknown indicator name is encountered. *)

val init_state : string list -> float -> Yojson.Basic.t -> string -> t
(** [init_state indic_names budget json] initializes state [t] based on
    the passed in parameters. In this state, no positions are held,
    profits are zero, and initial cash is equal to [budget]. *)
