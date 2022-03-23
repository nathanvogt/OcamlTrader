include Indicator
include Feeder

(* ------ global variables ------ *)

(** Global list of indicators. Requires that indicators spelled
    correctly. Some, like coin_name, are hard coded for now *)
let indicators = [ "RSI"; "MACD" ]

let coin_name = "ETH"
let budget = 10000.00
let report = ref @@ "Report of " ^ coin_name ^ " purchases: \n"
(* string representation of report meant to be printed to file *)

(* ------ connect with feeder ------ *)
(* TODO: insert any things conncting to C feeder here *)

(* ------ helper funcs ------ *)
(* helper function taking average based on pipeline ordering *)
let average den num = num /. den

(** [indication_naive indications acc ] takes in indication list
    [indications] average of all indicator measures. It is naive because
    no normalization is made, nor is there distinction between the type
    of indicator. *)
let rec indication_naive acc (indications : State.indicator_type list) =
  match indications with
  | [] -> acc
  | h :: t -> begin
      match h with
      | RSI m -> indication_naive (acc +. m) t
      | MACD m -> indication_naive (acc +. m) t
    end

(* hueristic indicator weighting just taking average *)
let weight_indicators st =
  State.indicator_values st
  |> indication_naive 0.0
  |> average @@ float_of_int (List.length indicators)

(* helper function receiving decision and taking corresponding action

   later need to make this smarter: don't sell when we don't have
   position sizes control the total budget it buys and sells -jun *)
let evaluate_indicators weight =
  if weight <= 30. then "sell!"
  else if weight <= 70. then "wait!"
  else "buy!"

(* helper function printing ansiterminal color unit of decision *)
let ansiterminal_print decision =
  if decision = "sell!" then
    ANSITerminal.print_string [ ANSITerminal.red ] "sell!\n"
  else if decision = "buy!" then
    ANSITerminal.print_string [ ANSITerminal.green ] "buy!\n"
  else print_string "wait!\n"

(* TODO: late need to add helper function taking in record of indicators
   and outputing normalized metric to decide whether to buy or sell
   returns [0..100].

   later on, we can make this smarter by having the computer computer
   can the amount of money it invests based on its confidence level*)

(* ------ main loop ------ *)

(** [main_loop state] is the repeating loop of our program that takes
    [state] from previous timestep and makes a decision, then receives
    new data from feeder and passes new state *)
let rec main_loop st =
  print_string "\n\n";
  let data = State.data_print coin_name st in
  print_string @@ data;
  let decision = evaluate_indicators @@ weight_indicators st in
  ansiterminal_print decision (* can add buy/sell action later *);
  report := !report ^ data ^ decision ^ "\n\n";
  (* update report *)
  match Feeder.next_day () with
  | None ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "This is the end of the file. \n";
      Printf.fprintf (open_out "report.txt") "%s" !report;
      exit 0
  | Some new_data -> State.update_data st new_data |> main_loop

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nStarting crypto trader \n";
  Feeder.init_reader ();
  match Feeder.next_day () with
  | None ->
      print_string "This is the end of the file.";
      exit 0
  | Some new_data ->
      State.init_state indicators budget new_data |> main_loop

(* Execute the game engine. *)
let () = main ()
