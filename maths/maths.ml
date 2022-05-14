(* avoid dividing by zero *)
let epsilon = 0.0001

let trend_proximity spread diff =
  let diff_spread_raised = (diff /. spread) ** 5. in
  1. /. (diff_spread_raised +. epsilon)

let e = 2.7182818284

let sigmoid x =
  let epsilon_reciprocal = 1. /. (e ** x) in
  1. /. (1. +. epsilon_reciprocal)

let tanh_off range spread =
  let epsilon_raised_spread = e ** spread in
  let epsilon_raised_sum = epsilon_raised_spread +. epsilon in
  (epsilon_raised_spread
  -. ((1. +. epsilon) /. epsilon_raised_sum)
  +. epsilon)
  /. (epsilon_raised_spread
     +. ((1. +. epsilon) /. epsilon_raised_sum)
     +. epsilon)
  +. range

let tanh range spread x =
  let x_div_spread = x /. spread in
  range
  *. ((e ** x_div_spread) -. (e ** Float.neg x_div_spread))
  /. ((e ** x_div_spread) +. (e ** Float.neg x_div_spread))
