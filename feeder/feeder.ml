include Yojson

external init_reader : unit -> unit = "initReader"
external read_next_day : unit -> string = "nextDay"
let next_day : unit -> Yojson.t = 
  let 
  let json = Yojson.Basic.from_string 