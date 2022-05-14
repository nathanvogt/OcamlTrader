(** Extracts possible trend lines from historical data
    
    Uses feeder to lookback at historical price data and 
    identify possible horizontal trend lines. Trend lines can 
    be used to predict price trends.
*)

open Feeder

type crit_point

val crit_points_days : float list -> crit_point list
(** [crit_points_days l ] takes list of floats and returns the critical
    points that correspond to local minima and local maxima *)

val filter_crit_points : crit_point list -> float -> crit_point list
(** [filter_crit_points cps p] takes in a list of critical points
[cps] and filters points clustered together with parameter [p].
Greater [p] is the more points filtered out. *)

val float_of_crit : crit_point -> float
(** [float_of_crit c] returns the float value of the critical point [c] *)

(* TODO: combine trend lines so each line has a weight. Factor
in the weight *)
val trend_line_indicator : crit_point list -> float -> float
(** [trend_line_indicator cps p] takes in a list of critical
points [cps] corresponding to horizontal trend lines and returns a
float between 0 and 1 corresponding to sell and buy respectively
at the current price [p]. *)