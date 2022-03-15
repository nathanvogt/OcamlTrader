(** [smoothing] is the integer value 2; the commonly accepeted smoothing
    value in the finance industry *)
let smoothing = 2

exception OutOfRange
(** exception [OutOfRange] is thrown when the [sublist] method attempts
    to access an element of a list that is out of bounds *)

(** [sublist start stop lst] takes in a list [lst] along with the
    [start] and [stop] index locations; returns the resulting sublist;
    method altered from {{:https://stackoverflow.com/a/2717815} here} *)
let rec sublist start stop lst =
  match lst with
  | [] -> raise OutOfRange
  | h :: t ->
      let tail =
        if stop = 0 then [] else sublist (start - 1) (stop - 1) t
      in
      if start > 0 then tail else h :: tail

(** [avg lst] is the [float] average of a given [float list] *)
let avg lst =
  let sum = List.fold_right (fun total x -> total +. x) lst 0. in
  sum /. float_of_int (List.length lst)

(** [sma lst n] is the simple point average with groups of [n]
    consecutive days; it takes in a float list and returns the averages
    of [n] days as a float list *)
let rec sma (lst : float list) (n : int) : float list =
  match lst with
  | [] -> []
  | h :: t ->
      if List.length lst < n then []
      else avg (sublist 0 (n - 1) lst) :: sma t n

let rec ema_recurse (lst : float list) (n : int) (yesterday : float) :
    float list =
  match lst with
  | [] -> []
  | h :: t ->
      let alpha = float_of_int smoothing /. float_of_int (n + 1) in
      let ema_today = (alpha *. h) +. ((1. -. alpha) *. yesterday) in
      ema_today :: ema_recurse t n ema_today

(** [ema lst n] is the *)
let ema lst n =
  match lst with
  | [] -> []
  | h :: t -> h :: ema_recurse t n h