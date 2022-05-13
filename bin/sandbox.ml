include Feeder
include Trend

let closes = Feeder.init_reader (); Feeder.lookback "ETH" 360
let closes2 = Feeder.lookback "ETH" 112
let crits = Trend.crit_points_days closes

let multiple_next_day n = let _ = Feeder.reset_reader () in 
  let rec aux acc count = if count = 0 then acc else aux (Feeder.next_day () :: acc) (count - 1)
in aux [] n |> List.filter (fun x -> match x with | None -> false | _ -> true)

(* print trendline indicator for different price values *)
(* let _ = let rec bruh price = Trend.trend_line_indicator crits price |> string_of_float
|> fun s -> print_endline @@ "price: "^(string_of_float price)^" ||  trend indicator: "^s; if price = 0. then () else bruh (price -. 10.)
in bruh 4000.0  *)

let _ = List.length crits |> string_of_int |> fun s -> "crits: "^s |> print_endline
let filtered = Trend.filter_crit_points crits 30. 
let _ = List.length filtered |> string_of_int |> fun s -> "filtered: "^s |> print_endline