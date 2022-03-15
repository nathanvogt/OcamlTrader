let macd lst =
  let ema_twelve = Ma.ema lst 12 in
  let len = List.length ema_twelve in
  let twelve_day = Ma.sublist 14 (len - 1) ema_twelve in
  let twenty_six_day = Ma.ema lst 26 in
  List.map2 (fun x y -> x -. y) twelve_day twenty_six_day