(* Stochastic Oscillator *)
let sort_low lst =
  List.sort (fun a b -> (a -. b) *. 100000. |> int_of_float) lst
  |> List.hd

let sort_high lst =
  List.sort (fun a b -> (b -. a) *. 100000. |> int_of_float) lst
  |> List.hd

let update_val close past14 =
  let l14 = sort_low past14 in
  let h14 = sort_high past14 in
  let k = (close -. l14) /. (h14 -. l14) *. 100. in
  let next_14 = List.rev (close :: List.rev (List.tl past14)) in
  (k, next_14)
