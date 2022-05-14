type t = {
  open_price : float;
  close_price : float;
  high : float;
  low : float;
  volume : float;
  date : string;
}

exception UninitializedReader

external init_reader : unit -> unit = "initReader"
external read_next_day : unit -> string = "nextDay"
external reset_reader : unit -> unit = "resetReader"
external lookback_raw : int -> string = "lookback"

let to_string x =
  [ x.open_price; x.close_price; x.high; x.low; x.volume ]
  |> List.map (fun x -> string_of_float x)
  |> String.concat ", "
  |> fun s -> s ^ x.date

let construct_t s =
  let l = String.split_on_char ',' s in
  {
    open_price = Float.of_string @@ List.nth l 1;
    close_price = Float.of_string @@ List.nth l 4;
    high = Float.of_string @@ List.nth l 2;
    low = Float.of_string @@ List.nth l 3;
    volume = Float.of_string @@ List.nth l 6;
    date = List.nth l 0;
  }

(* let next_day () = (*can optimize into single pass function*) let l =
   String.split_on_char ',' (read_next_day ()) in { open_price =
   Float.of_string @@ List.nth l 1; close_price = Float.of_string @@
   List.nth l 4; high = Float.of_string @@ List.nth l 2; low =
   Float.of_string @@ List.nth l 3; volume = Float.of_string @@ List.nth
   l 6; date = List.nth l 0; } *)
let next_day () =
  let s = read_next_day () in
  if s = "UI" then raise UninitializedReader
  else if s = "EOF" then None
  else
    let l = String.split_on_char ',' s in
    Some
      {
        open_price = Float.of_string @@ List.nth l 1;
        close_price = Float.of_string @@ List.nth l 4;
        high = Float.of_string @@ List.nth l 2;
        low = Float.of_string @@ List.nth l 3;
        volume = Float.of_string @@ List.nth l 6;
        date = List.nth l 0;
      }

let open_price d = d.open_price
let close_price d = d.close_price
let high d = d.high
let low d = d.low
let volume d = d.volume
let date d = d.date
let coin_name d = "ETH"

(* helper function assiting main lookback function. returns desired list
   of length [days]*)
let lookback_g (days : int) : t list =
  lookback_raw days
  |> String.split_on_char ' '
  |> List.map (fun x -> construct_t x)

(* set to empty list for now *)
let lookback (coin : string) (days : int) : float list =
  lookback_g days |> List.map (fun x -> close_price x)
