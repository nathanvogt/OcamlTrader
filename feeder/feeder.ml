include Yojson

external init_reader : unit -> unit = "initReader"
external read_next_day : unit -> string = "nextDay"

let next_day () = 
  let raw = read_next_day () in 
    String.split_on_char ',' raw