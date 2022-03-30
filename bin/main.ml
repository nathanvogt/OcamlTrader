include Indicator
include Feeder
include State
include Unix

(* ------ global variables ------ *)

(** Global list of indicators. Requires that indicators spelled
    correctly. Some, like coin_name, are hard coded for now *)
let indicators = [ "RSI"; "MACD" ]

let coin_name = "ETH"
let budget = 10000.00

(* string representation of report meant to be printed to file *)
let report = ref @@ "Report of " ^ coin_name ^ " purchases: \n"

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
  if weight <= 30. then State.Sell
  else if weight <= 70. then State.Wait
  else State.Buy

(* helper function printing ansiterminal color unit of decision *)
let ansiterminal_print decision =
  if decision = State.Sell then
    ANSITerminal.print_string [ ANSITerminal.red ] "SELL!\n"
  else if decision = State.Buy then
    ANSITerminal.print_string [ ANSITerminal.green ] "BUY!\n"
  else ANSITerminal.print_string [ ANSITerminal.magenta ] "WAIT!\n"

(* helper function turning decision type to string *)
let deicision_to_string = function
  | State.Sell -> "SELL!\n"
  | State.Buy -> "BUY!\n"
  | State.Wait -> "WAIT!\n"

let is_valid_input input =
  match float_of_string input with
  | exception Failure _ -> false
  | _ -> true

(* ------ main loop ------ *)

(** [main_loop state] is the repeating loop of our program that takes
    [state] from previous timestep and makes a decision, then receives
    new data from feeder and passes new state *)
let rec main_loop wait_period st =
  (* formatting *)
  print_string "\n\n";

  (* storing data in value to be later concatenated in report *)
  let data = State.data_print coin_name st in
  print_string @@ data;

  (* storing decision in value to be later passed into State *)
  let decision = evaluate_indicators @@ weight_indicators st in
  ansiterminal_print decision (* can add buy/sell action later *);

  (* tuple with first element being new state, second being string
     representation of action executed *)
  let state_action_tup = State.decision_action st decision in
  report :=
    !report ^ data
    ^ deicision_to_string decision
    ^ snd state_action_tup ^ "\n\n";
  print_string @@ snd state_action_tup ^ "\n";
  (* update report *)
  match Feeder.next_day () with
  | None ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "This is the end of the file. \n";
      Printf.fprintf (open_out "report.txt") "%s" !report;
      exit 0
  | Some new_data ->
      Unix.sleepf wait_period;
      flush Stdlib.stdout;
      State.update_data (fst state_action_tup) new_data
      |> main_loop wait_period

(* helper function setting parameters for user input *)
let rec set_parameters () =
  match read_line () with
  | exception End_of_file -> ()
  | input ->
      if is_valid_input input then (
        let wait_period = float_of_string input in
        Feeder.init_reader ();
        match Feeder.next_day () with
        | None ->
            print_string "This is the end of the file.";
            exit 0
        | Some new_data ->
            ANSITerminal.print_string [ ANSITerminal.green ]
              "Starting crypto trader.\n\n";
            Unix.sleepf 1.5;
            flush Stdlib.stdout;
            State.init_state indicators budget new_data
            |> main_loop wait_period)
      else begin
        print_string "Not a valid float. Please try again.\n";
        print_string ">";
        set_parameters ()
      end

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to crypto trader \n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "How long would you like to pause between each day? (Enter float \
     in seconds) \n";
  print_string ">";
  set_parameters ()

(* Execute the game engine. *)
let () = main ()