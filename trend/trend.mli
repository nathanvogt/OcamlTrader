open Feeder

type crit_point

val crit_points_days : float list -> crit_point list
(** [crit_points_days l ] takes list of floats and returns the critical points
that correspond to local minima and local maxima *)

val float_of_crit : crit_point -> float
(** [float_of_crit c] returns the float value of the critical point [c] *)