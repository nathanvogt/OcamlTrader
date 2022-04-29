include Indicator
include Feeder
include State
include Lwt_io

(* ------ global variables ------ *)

(** Global list of indicators. Requires that indicators spelled
    correctly. Some, like coin_name, are hard coded for now *)
let indicators = [ "RSI"; "MACD" ]

let coin_name = "ETH"
let budget = 10000.00

(* string representation of report meant to be printed to file *)
let report = ref @@ "Report of " ^ coin_name ^ " purchases: \n"
let report_file = "report.txt"
let num_splits = 8
let grid_upper_limit = ref 0.
let grid_lower_limit = ref 0.
let grid_size = ref 0.

(* formatting *)
(* grid line of 77 characters*)
let grid_price_line =
  ":     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - \
   - - - - -"

let grid_linebreak = "\n\n"

(* space before price point: 26 spaces *)
let space_place_holder = "                          "

(********************************************************************
    Helper Functions
 ********************************************************************)
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
      | RSI (rsi, _, _, _, _) -> indication_naive (acc +. rsi) t
      | MACD (macd, _, _, _) -> indication_naive (acc +. macd) t
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

(* from: https://ilyasergey.net/YSC2229/week-10-reading-files.html
   [update_file filename str] updates [filename] with real time
   information about the crypto price, action at this time frame,
   profit/loss. *)
let update_file filename str =
  let outc = Core.Out_channel.create ~append:false filename in
  Core.protect
    ~f:(fun () -> Core.fprintf outc "%s\n" str)
    ~finally:(fun () -> Core.Out_channel.close outc)

(* updating the upper and lower limits based on price. recursive in
   order to ensure graph resizes until fit *)
let rec resize_graph curr_price =
  let margin = 2. *. !grid_size in
  (* first check if bounds are too small, should increase to 30% margin
     on each side *)
  if !grid_upper_limit -. !grid_lower_limit < 0.2 *. curr_price then (
    ANSITerminal.print_string [ ANSITerminal.magenta ] "Bound ratios.\n";
    grid_lower_limit := curr_price -. (0.4 *. curr_price);
    grid_upper_limit := curr_price +. (0.4 *. curr_price);
    grid_size :=
      (!grid_upper_limit -. !grid_lower_limit)
      /. float_of_int num_splits);
  if curr_price -. margin < !grid_lower_limit then (
    grid_lower_limit :=
      !grid_lower_limit -. (!grid_size *. float_of_int (num_splits / 2));
    grid_upper_limit :=
      !grid_upper_limit -. (!grid_size *. float_of_int (num_splits / 2));
    resize_graph curr_price)
  else if curr_price +. margin > !grid_upper_limit then (
    grid_lower_limit :=
      !grid_lower_limit +. (!grid_size *. float_of_int (num_splits / 2));
    grid_upper_limit :=
      !grid_upper_limit +. (!grid_size *. float_of_int (num_splits / 2));
    resize_graph curr_price)
  else ()
(* resizing complete *)

(* [graph_grid] prints out nicely the upper and lower bounds of the
   price, current price, and resizes if needed *)
let rec graph_grid
    (curr_price : float)
    (counter_price : float)
    (price_below : bool) : unit =
  if counter_price < !grid_lower_limit then print_newline ()
  else begin
    (* print red (signaling sell) for every price line [curr_price] is
       below and green (signaling buy) for every price line [curr_price]
       is above *)
    if price_below then
      ANSITerminal.print_string [ ANSITerminal.red ]
      @@ string_of_float counter_price
      ^ grid_price_line
    else
      ANSITerminal.print_string [ ANSITerminal.green ]
      @@ string_of_float counter_price
      ^ grid_price_line;
    (* if [curr_price] within the bounds of [counter_price] and
       succeeding counter_price, print out curr_price *)
    if
      curr_price < counter_price
      && curr_price > counter_price -. !grid_size
    then
      ANSITerminal.print_string [ ANSITerminal.green ]
      @@ "\n" ^ space_place_holder
      ^ string_of_float curr_price
      ^ ":  x" ^ "\n"
    else print_endline grid_linebreak;
    let next_lower_price = counter_price -. !grid_size in
    graph_grid curr_price next_lower_price
      (curr_price < next_lower_price)
  end

(********************************************************************
    Main Loop
 ********************************************************************)

(** [main_loop state] is the repeating loop of our program that takes
    [state] from previous timestep and makes a decision, then receives
    new data from feeder and passes new state *)
let rec main_loop wait_period st =
  (* formatting *)
  print_string "\n\n";

  (* storing data in value to be later concatenated in report *)
  let data = State.data_print coin_name st in
  print_endline @@ "Date: " ^ State.curr_date st coin_name;

  (* print_string @@ data; *)

  (* storing decision in value to be later passed into State *)
  let decision = evaluate_indicators @@ weight_indicators st in
  ansiterminal_print decision;

  (* tuple with first element being new state, second being string
     representation of action executed *)
  let state_action_tup = State.decision_action st decision in
  let step_data =
    data ^ deicision_to_string decision ^ snd state_action_tup ^ "\n\n"
  in

  (* reverse order so newest printed on top *)
  report := step_data ^ !report;
  print_string @@ snd state_action_tup ^ "\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
  @@ "Real time data is sent to " ^ report_file ^ "\n";
  update_file report_file !report;
  resize_graph (State.price_close st coin_name);
  graph_grid
    (State.price_close st coin_name)
    !grid_upper_limit
    (State.price_close st coin_name < !grid_upper_limit);
  (* update report *)
  match Feeder.next_day () with
  | None ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "This is the end of the file. \n";
      exit 0
  | Some new_data ->
      Stdlib.flush Stdlib.stdout;
      Unix.sleepf wait_period;
      State.update_data (fst state_action_tup) new_data
      |> main_loop wait_period

(* helper function setting parameters for user input *)
let rec set_wait_time_parameter () =
  match Stdlib.read_line () with
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
              "Starting crypto trader...\n\n";
            Stdlib.flush Stdlib.stdout;
            Unix.sleepf 1.5;
            State.init_state indicators budget new_data
            |> main_loop wait_period)
      else begin
        print_string "Not a valid float. Please try again.\n";
        print_string ">";
        set_wait_time_parameter ()
      end

let rec set_grid_bound_parameter (set_upper : bool) =
  match Stdlib.read_line () with
  | exception End_of_file -> ()
  | input ->
      if is_valid_input input then
        if set_upper then grid_upper_limit := float_of_string input
        else grid_lower_limit := float_of_string input
      else begin
        print_string "Not a valid float. Please try again.\n";
        print_string ">";
        set_grid_bound_parameter set_upper
      end

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to crypto trader \n";
  (* ask for user input parameters for grid search algorithm *)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
     What is the upper price bound of your grid? (Enter price as float) \n";
  print_string ">";
  set_grid_bound_parameter true;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
     What is the lower price bound of your grid? (Enter price as float) \n";
  print_string ">";
  set_grid_bound_parameter false;
  grid_size :=
    (!grid_upper_limit -. !grid_lower_limit) /. float_of_int num_splits;
  (* ask for user input parameters for stop time between each trade on
     historical data *)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
     How long would you like to pause between each day? (Enter float \
     in seconds) \n";
  print_string ">";
  set_wait_time_parameter ()

(* Execute the game engine. *)
let () = main ()