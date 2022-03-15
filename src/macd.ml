let macd lst =
  let twelve_day = Ma.ema lst 12 in
  let twenty_six_day = Ma.ema lst 26 in
  List.map2 (fun x y -> x -. y) twelve_day twenty_six_day
