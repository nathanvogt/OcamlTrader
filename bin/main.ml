include Indicator
include Feeder

let main () = 
  let data = Feeder.next_day () in 
    print_endline @@ Yojson.Basic.pretty_to_string data
  

let start () = 
  Feeder.init_reader ();
  main ()
  

let () = start ()