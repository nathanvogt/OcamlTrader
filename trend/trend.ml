include Feeder
include Maths

(* ==========HYPERPARAMETERS========== *)
let spread_hyperparam = 7.0

(* ========================== *)

type crit_point =
  | Maximum of float
  | Minimum of float

let float_of_crit crit =
  match crit with
  | Maximum f
  | Minimum f ->
      f

let rec crit_points_aux acc = function
  | prev :: curr :: next :: t ->
      if prev > curr && curr < next then
        crit_points_aux (Minimum curr :: acc) (curr :: next :: t)
      else if prev < curr && curr > next then
        crit_points_aux (Maximum curr :: acc) (curr :: next :: t)
      else crit_points_aux acc (next :: t)
  | _ -> acc

let crit_points_days lst = List.rev @@ crit_points_aux [] lst

let peak s =
  match s with
  | [] -> None
  | h :: t -> Some h

let pop s =
  match s with
  | [] -> None
  | h :: t -> Some t

let clean_pop s =
  match s with
  | [] -> []
  | h :: t -> t

let push s i = i :: s

let filter_crit_points cps p =
  let rec helper acc cps p =
    let prev_point =
      match peak acc with
      | Some point -> float_of_crit point
      | _ -> failwith "this is not supposed to happen"
    in
    match cps with
    | [] -> List.rev acc
    | h :: t ->
        let curr_point = float_of_crit h in
        let acc' =
          if Float.abs (curr_point -. prev_point) < p then
            push (clean_pop acc) h
          else push acc h
        in
        helper acc' t p
  in
  match cps with
  | [] -> []
  | h :: t -> helper [ h ] t p

(* these functions are seperated so we can add in momentum which will
   cause the formulas to differ *)
let compute_below crit price : float =
  match crit with
  | Maximum v -> Maths.trend_proximity spread_hyperparam (v -. price)
  | Minimum v -> Maths.trend_proximity spread_hyperparam (price -. v)

let compute_above crit price : float =
  match crit with
  | Maximum v -> Maths.trend_proximity spread_hyperparam (price -. v)
  | Minimum v -> Maths.trend_proximity spread_hyperparam (v -. price)

(* todo: factor in momentum of price to tell if it has crossed a trend
   line *)
let trend_line_indicator (points : crit_point list) (price : float) =
  let rec helper acc points price =
    match points with
    | h :: t ->
        let delta =
          if float_of_crit h > price then compute_above h price
          else compute_below h price
        in
        helper (acc +. delta) t price
    | [] -> acc
  in
  helper 0.0 points price
