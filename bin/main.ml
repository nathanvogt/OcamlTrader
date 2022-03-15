include Indicator
include Feeder

(* ------ global variables ------ *)

(** Global list of indicators. Requires that indicators spelled
    correctly *)
let indicators = [ ("RSI", "MACD") ]

let budget = 10000.00

(* ------ connect with feeder ------ *)
(* TODO: insert any things conncting to C feeder here *)

(* ------ helper funcs ------ *)
(* helper function taking average based on pipeline ordering *)
let average den num = num /. den

(** [indication_naive indications acc ] takes in indication list
    [indications] average of all indicator measures. It is naive because
    no normalization is made, nor is there distinction between the type
    of indicator. *)
let rec indication_naive acc (indications : indicator_type list) =
  match indications with
  | [] -> acc
  | h :: t -> begin
      match h with
      | RSI m -> indication_naive (acc +. m) t
      | MACD m -> indication_naive (acc +. m) t
    end

(* hueristic indicator weighting just taking average *)
let weight_indicators st =
  indicator_values st |> indication_naive 0.0
  |> average @@ float_of_int (List.length indicators)

(* helper function receiving decision and taking corresponding action

   later need to make this smarter: don't sell when we don't have
   position sizes control the total budget it buys and sells -jun *)
let evaluate_indicators weight =
  if weight <= 30. then print_string "sell!"
  else if weight <= 70. then print_string "wait!"
  else print_string "buy!"

(* TODO: late need to add helper function taking in record of indicators
   and outputing normalized metric to decide whether to buy or sell
   returns [0..100].

   later on, we can make this smarter by having the computer computer
   can the amount of money it invests based on its confidence level*)

(* ------ main loop ------ *)

(** [main_loop state] is the repeating loop of our program that takes
    [state] from previous timestep and makes a decision, then receives
    new data from feeder and passes new state *)
let main_loop st =
  weight_indicators st |> evaluate_indicators
  (* can add buy/sell action later *);
  get_feeder_data
  |> update_data st (* param = json file from feeder *)
  (* param = coin name from feeder *) |> main_loop

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  Feeder.init_reader ();
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nStarting crypto trader \n";
  let starting_sate =
    get_feeder_data |> init_state indicators budget
    (* param = json file from feeder *)
    (* param = coin name from feeder *)
  in
  main_loop starting_state

(* Execute the glet () = main ()
