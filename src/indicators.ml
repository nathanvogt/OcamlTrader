type day = {
  date : int * int * int;
  open_price : float;
  high : float;
  low : float;
  close : float;
  volume : int;
}

(** [smoothing] is the integer value 2; the commonly accepeted smoothing
    value in the finance industry *)
let smoothing = 2

exception OutOfRange
(** exception [OutOfRange] is thrown when the [sublist] method attempts
    to access an element of a list that is out of bounds *)

(** [sublist start stop lst] takes in a float list [lst] along with the
    [start] and [stop] index locations; returns the resulting sublist *)
let rec sublist start stop lst =
  match lst with
  | [] -> raise OutOfRange
  | h :: t ->
      let tail =
        if stop = 0. then [] else sublist (start -. 1.) (stop -. 1.) t
      in
      if start > 0. then tail else h :: tail

(** [sma lst] is the simple point average; it takes in a float list and
    returns the average as a float *)
let sma lst =
  let sum = List.fold_right (fun total x -> total +. x) lst 0. in
  sum /. float_of_int (List.length lst)

(** [ema lst n] is the *)