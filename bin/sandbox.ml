include Feeder
include Trend

let closes = Feeder.init_reader (); Feeder.lookback "ETH" 360
let closes2 = Feeder.lookback "ETH" 112
let crits = Trend.crit_points_days closes

(* let _ = let Some d = Feeder.next_day () in d |> Feeder.to_string
|> print_endline *)

(* print number of close points and number of crit points *)
(* let _ = closes |> List.length |> string_of_int |> print_endline;
crits |> List.length |> string_of_int|> print_endline *)

(* print crit points as pythonic array *)
(* let _ = List.fold_right 
(fun x acc -> (string_of_float @@ float_of_crit x) :: acc) crits []
|> List.rev |>  String.concat ", " |> fun s -> "["^s^"]" |> print_endline *)


let multiple_next_day n = let _ = Feeder.reset_reader () in 
  let rec aux acc count = if count = 0 then acc else aux (Feeder.next_day () :: acc) (count - 1)
in aux [] n |> List.filter (fun x -> match x with | None -> false | _ -> true)

(* let _ = Feeder.init_reader ()
let _ = List.length @@ multiple_next_day 12 |> string_of_int |> print_endline
let _ = List.length @@ multiple_next_day 200 |> string_of_int |> print_endline
let _ = List.length @@ multiple_next_day 366 |> string_of_int |> print_endline
let _ = List.length @@ multiple_next_day 400 |> string_of_int |> print_endline
let _ = List.length @@ multiple_next_day 13 |> string_of_int |> print_endline
let _ = List.length @@ multiple_next_day 301 |> string_of_int |> print_endline

let _ = Feeder.lookback "ETH" 12 |> List.length |> string_of_int |> print_endline
let _ = Feeder.lookback "ETH" 114 |> List.length |> string_of_int |> print_endline
let _ = Feeder.lookback "ETH" 200 |> List.length |> string_of_int |> print_endline
let _ = Feeder.lookback "ETH" 312 |> List.length |> string_of_int |> print_endline *)








