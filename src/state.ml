(* sub type for raw data from feeder *)
type raw_data = {
  op : float;
  high : float;
  low : float;
  close : float;
  volume : int;
}

(* sub type containing record of all indicators *)
type indicator = {
  rsi : float;
  macd : float;
}

type t = {
  data : raw_data;
  indicators : indicator;
}

open Yojson.Basic.Util

(* helper function getting time frame data from json *)
let data_of_json j =
  {
    op = j |> member "open" |> to_float;
    high = j |> member "high" |> to_float;
    low = j |> member "low" |> to_float;
    close = j |> member "close" |> to_float;
    volume = j |> member "volume" |> to_int;
  }

let from_json json coin_name =
  {
    data = json |> member "coins" |> member coin_name |> data_of_json;
    indicators = { rsi = 0.0; macd = 0.0 };
  }