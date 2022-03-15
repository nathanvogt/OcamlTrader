include Feeder 

let main () = 
  let data = Feeder.next_day () in 
    data |> Float.to_string |> print_endline
  
let start () = 
  Feeder.init_reader ();
  main ()

let () = start ()