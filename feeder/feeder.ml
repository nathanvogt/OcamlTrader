
type t = {open_price : float ; close_price : float ; 
  high : float ; low : float ; volume : int}

external init_reader : unit -> unit = "initReader"
external read_next_day : unit -> string = "nextDay"

let next_day () = (*can optimize into single pass function*)
  let l = String.split_on_char ',' (read_next_day ()) in
    {
      open_price = Float.of_string @@ List.nth l 1;
      close_price = Float.of_string @@ List.nth l 4;
      high = Float.of_string @@ List.nth l 2;
      low = Float.of_string @@ List.nth l 3;
      volume = int_of_string @@ List.nth l 6
    }

let open_price d = d.open_price 
let close_price d = d.close_price
let high d = d.high 
let low d = d.low 
let volume d = d.volume