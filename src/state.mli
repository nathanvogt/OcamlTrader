(** Representation of dynamic indicator state.

    This module represents the current state of our trading algorithm
    based on the raw values provided by feeder and the returned values
    of the indicators calculated. It reads data from the C code in
    /feeder as JSON and calls the indicator calculators. *)

type t
(** The abstract type of values representing the indicator state. *)

val from_json : Yojson.Basic.t -> string -> t
(** [from_json j coin] is the indicator record of [coin] from [j].
    Requires: [j] is a valid JSON adventure representation. *)
