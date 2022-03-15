let rs_to_rsi gain loss = 100. -. (100. /. (1. +. (gain /. loss)))

let rec rsi_recurse lst yesterday_gain yesterday_loss =
  match lst with
  | [] -> []
  | h :: t ->
      let adjusted =
        if h >= 0. then
          let avg_gain = ((yesterday_gain *. 13.) +. h) /. 14. in
          let avg_loss = yesterday_loss *. 13. /. 14. in
          (avg_gain, avg_loss)
        else
          let avg_gain = yesterday_gain *. 13. /. 14. in
          let avg_loss = ((yesterday_loss *. 13.) -. h) /. 14. in
          (avg_gain, avg_loss)
      in
      let today_rsi = rs_to_rsi (fst adjusted) (snd adjusted) in
      today_rsi :: rsi_recurse t (fst adjusted) (snd adjusted)

let rsi lst =
  let diff = Ma.diff lst in
  let fourteen = Ma.sublist 0 12 diff in
  let gain_loss = Ma.gain_loss fourteen (0., 0.) in
  let avg_loss = snd gain_loss /. -14. in
  let avg_gain = fst gain_loss /. 14. in
  let after_fourteen = Ma.sublist 13 (List.length diff - 1) diff in
  rsi_recurse after_fourteen avg_gain avg_loss