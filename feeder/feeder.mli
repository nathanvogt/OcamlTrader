(** Representation of a day of raw historical data *)
type t
(** Initializes the file reader that reads raw historical data *)
external init_reader : unit -> unit = "initReader"
(** Returuns the next day of historical data *)
val next_day : unit -> t 
(** *)
val open_price : t -> float
val close_price : t -> float
val high : t -> float
val low : t -> float
val volume : t -> int