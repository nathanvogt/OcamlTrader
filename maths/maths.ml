let epsilon = 0.0001
(* avoid dividing by zero *)

let trend_proximity spread diff =
  1. /. (((diff /. spread) ** 5.) +. epsilon)

let e = 2.7182818284
let sigmoid x = 1. /. (1. +. (1. /. (e ** x)))

let tanh_off o x =
  ((e ** x) -. ((1. +. epsilon) /. ((e ** x) +. epsilon)) +. epsilon)
  /. ((e ** x) +. ((1. +. epsilon) /. ((e ** x) +. epsilon)) +. epsilon)
  +. o

let tanh r s x =
  r
  *. ((e ** (x /. s)) -. (e ** Float.neg (x /. s)))
  /. ((e ** (x /. s)) +. (e ** Float.neg (x /. s)))
