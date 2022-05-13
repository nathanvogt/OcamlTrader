(* Each indicator input into the decision formula should be normalized
into the range [-50, 50] where higher positive numbers
indicate the suggestion to buy and lower negative numbers 
indicate the suggestion to sell  *)

include Indicator
include State
include Maths

(* ======= HYPERPARAMS ========= *)

(* ============================= *)

let package_weights_values weights_biases values =
  if (List.length weights_biases) <> (List.length values)
    then failwith "weights and values must map one to one"
else List.rev_map2 
(fun (weight, bias) value -> (weight, bias, value))
 weights_biases values

let scale_values weights_biases values =
  let packaged = package_weights_values weights_biases values in 
  List.map (fun (weight, bias, value) ->
    (weight *. value) +. bias) packaged

(* TODO: improve momentum in state *)
let interp_rsi rsi = 
  (rsi -. 50.) ** (1. /. 3.)
let interp_macd macd = 
  (Maths.tanh 50. 50. macd)
let interp_obv obv = 
  let momentum = 1. in 
  obv *. momentum
let interp_so so = 
  (so -. 50.) ** 3.