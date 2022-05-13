include Feeder

type crit_point = Maximum of float | Minimum of float


let rec crit_points_aux acc = function
| (prev :: curr :: next :: t) -> if prev > curr && curr < next then crit_points_aux (Minimum curr :: acc) (next :: t) else
  if prev < curr && curr > next then crit_points_aux (Maximum curr :: acc) (next :: t) else crit_points_aux acc (next :: t)
| _ -> acc

let crit_points_days lst = List.rev @@ crit_points_aux [] lst

let float_of_crit crit = match crit with 
| Maximum f | Minimum f -> f

