include Indicator
include Feeder
include State
include Lwt_io
include Trend
include Maths

(* ========== HYPERPARAMS ============= *)
let grid_up_hyperparam = 10.
let grid_down_hyperparam = Float.neg 10.
let grid_neutral_hyperparam = 0.
let spread_hyperparam = 7.0
let tanh_range_hyperparam = 50.0
let tanh_spread_hyperparam = 15.0
(* ==================================== *)

(* ------ initializing global variables ------ *)

(** Global list of indicators. Requires that indicators spelled
    correctly. Some, like coin_name, are hard coded for now *)
let indicators = [ "RSI"; "MACD" ]

let coin_name_const = "ETH"
let budget = 10000.00

(* mock number of positions that naive heuristic can hold, based on
   floor of [budget]/starting_coin_price *)
let starting_pos = ref 0.

(* global reference to the price value of grid line above price *)
let grid_upper = ref 0.

(* global reference to the price value of grid line below price *)
let grid_lower = ref 0.

(* string representation of report meant to be printed to file *)
let report = ref @@ "Report of " ^ coin_name_const ^ " purchases: \n"
let report_file = "report.txt"
let num_splits = 14
let grid_upper_limit = ref 0.
let grid_lower_limit = ref 0.
let grid_size = ref 0.

(* formatting *)
(* grid line of 77 characters*)
let grid_price_line =
  ":     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - \
   - - - - -"

(* space before price point: 30 spaces *)
let space_place_holder = "                              "

(* messages to be printed at start of game prompting user input *)
let intro_message =
  "\n\n\
   Welcome to crypto trader! If you would like an introduction to our \
   trader, please type 'y' or 'yes'. Else, type 'n', 'no', or \
   'continue' to begin trading! \n\
   >"

let upper_message =
  "\n\
   What is the upper price bound of your grid? As the price of the \
   coin changes, our algorithm with automatically resize this bound \
   for you in order to provide a reasonable ratio for the upper and \
   lower bounds. (Enter price as float) \n\
   >"

let lower_message =
  "\n\
   What is the lower price bound of your grid? (Enter price as float) \n\
   >"

let pause_message =
  "\n\
   How long would you like to pause between each day? (Enter float in \
   seconds) \n\
   >"

(********************************************************************
    Indicator/Decision Functions
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
  | h :: t -> (
      match h with
      | RSI (rsi, _, _, _, _) -> indication_naive (acc +. rsi) t
      | MACD (macd, _, _, _) -> indication_naive (acc +. macd) t
      | OBV (obv, _) -> indication_naive (acc +. 0.) t)
(* place holder for now *)

(* heuristic taking simple average of indicators. @nathan: here is
   somewhere that could use a hyperparameter to be tuned *)
let weight_indicators st =
  State.indicator_values st
  |> indication_naive 0.0
  |> average @@ float_of_int (List.length indicators)

(* heuristic returning value of whether closing price has crossed any of
   upper or lower grid lines. returns float value similar to that of
   indicators between 0-100. *)
let grid_indicator price_close =
  if price_close > !grid_upper then grid_up_hyperparam
  else if price_close < !grid_lower then
    (* possible hyperparameter *)
    grid_down_hyperparam
  else grid_neutral_hyperparam

(* main function returning a combination of various indicators for a
   final decision *)
let indicator_comb st =
  let price = State.price_close st "ETH" in
  [
    Trend.trend_line_indicator (State.crit_points st) price;
    grid_indicator (State.price_close st coin_name_const);
  ]
  |> List.fold_left (fun acc x -> acc +. x) 0.
  |> Maths.tanh tanh_range_hyperparam tanh_spread_hyperparam
  |> ( +. ) tanh_range_hyperparam
(* weight_indicators st +. grid_indicator (State.price_close st
   coin_name_const) *)

(* helper function receiving decision and taking corresponding action

   later need to make this smarter: don't sell when we don't have
   position sizes control the total budget it buys and sells -jun *)
let evaluate_indicators weight =
  if weight <= 30. then State.Buy
  else if weight <= 70. then State.Wait
  else State.Sell

(********************************************************************
    Basic Formatted Print Helper Functions
 ********************************************************************)

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

(* helper function printing average market value and positions held of
   given coin *)
let market_positions_tostring st =
  "Coin: " ^ coin_name_const ^ "; Average position value: "
  ^ (string_of_float @@ State.avg_position_val st coin_name_const)
  ^ "; Number of positions: "
  ^ string_of_float (State.positions_held st coin_name_const)
  ^ "\n"

(* helper function ensuring user input is valid float *)
let is_valid_input input =
  match float_of_string input with
  | exception Failure _ -> false
  | _ -> true

(* helper function ensuring user command is within list of commands *)
let is_valid_command command =
  let input_lower = String.lowercase_ascii command in
  match input_lower with
  | "y"
  | "yes"
  | "n"
  | "no"
  | "continue" ->
      true
  | _ -> false

(* colorfying the profit that is printed in terminal *)
let profit_pretty_print prof_header prof_loss : unit =
  print_string prof_header;
  if prof_loss > 0. then
    ANSITerminal.print_string [ ANSITerminal.green ]
      (string_of_float prof_loss)
  else if prof_loss < 0. then
    ANSITerminal.print_string [ ANSITerminal.red ]
      (string_of_float prof_loss)
  else
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      (string_of_float prof_loss);
  print_string "\n"

(* helper function to update the position size held by the baseline
   algorithm who buys as many coins as it can based on budget and
   starting price at initialization, and just holds. Assumes budget >
   initial price *)
let update_naive_pos f =
  starting_pos := floor (budget /. Feeder.close_price f)

(* from: https://ilyasergey.net/YSC2229/week-10-reading-files.html
   [update_file filename str] updates [filename] with real time
   information about the crypto price, action at this time frame,
   profit/loss. *)
let update_file filename str =
  let outc = Core.Out_channel.create ~append:false filename in
  Core.protect
    ~f:(fun () -> Core.fprintf outc "%s\n" str)
    ~finally:(fun () -> Core.Out_channel.close outc)

(********************************************************************
    Graph Helper Functions
 ********************************************************************)

(* updating the upper and lower limits based on price. recursive in
   order to ensure graph resizes until fit *)
let rec resize_graph curr_price =
  let margin = 2. *. !grid_size in
  if !grid_upper_limit -. (!grid_size *. float_of_int num_splits) < 0.
  then grid_lower_limit := 0.
  else ();
  if
    !grid_upper_limit -. !grid_lower_limit < 0.2 *. curr_price
    || !grid_upper_limit -. !grid_lower_limit > 0.8 *. curr_price
  then (
    ANSITerminal.print_string
      [ ANSITerminal.magenta ]
      "Adjust bound ratios.\n";
    adjust_bounds curr_price num_splits);
  if curr_price -. margin < !grid_lower_limit then
    adjust_margin_down curr_price num_splits
  else if curr_price +. margin > !grid_upper_limit then
    adjust_margin_up curr_price num_splits
  else ()

(* helper function to adjust bounds of graph *)
and adjust_bounds curr_price num_splits =
  grid_lower_limit := curr_price -. (0.2 *. curr_price);
  grid_upper_limit := curr_price +. (0.2 *. curr_price);
  grid_size :=
    (!grid_upper_limit -. !grid_lower_limit) /. float_of_int num_splits

(* helper function subtracting from margin *)
and adjust_margin_down curr_price num_splits =
  grid_lower_limit :=
    !grid_lower_limit -. (!grid_size *. float_of_int (num_splits / 2));
  grid_upper_limit :=
    !grid_upper_limit -. (!grid_size *. float_of_int (num_splits / 2));
  resize_graph curr_price

(* helper function adding to margin *)
and adjust_margin_up curr_price num_splits =
  grid_lower_limit :=
    !grid_lower_limit +. (!grid_size *. float_of_int (num_splits / 2));
  grid_upper_limit :=
    !grid_upper_limit +. (!grid_size *. float_of_int (num_splits / 2));
  resize_graph curr_price

(* helper function mutating global variables for upper and lower
   bounds *)
let update_global_bounds upper lower =
  grid_upper := upper;
  grid_lower := lower

(* helper function pretty printing grid above the current price as
   red *)
let print_grid_red counter_price =
  ANSITerminal.print_string [ ANSITerminal.red ]
  @@ string_of_float counter_price
  ^ grid_price_line

(* helper funciton pretty printing grid below the current price as
   green *)
let print_grid_green counter_price =
  ANSITerminal.print_string [ ANSITerminal.green ]
  @@ string_of_float counter_price
  ^ grid_price_line

(* helper function prining the current price within grid represented as
   'x' *)
let print_curr_price curr_price =
  ANSITerminal.print_string [ ANSITerminal.green ]
  @@ "\n" ^ space_place_holder
  ^ string_of_float curr_price
  ^ ":  x" ^ "\n"

(* [graph_grid] prints out nicely the upper and lower bounds of the
   price, current price, and resizes if needed *)
let rec graph_grid
    (curr_price : float)
    (counter_price : float)
    (price_below : bool) : unit =
  if counter_price < !grid_lower_limit then print_newline ()
  else (
    if price_below then print_grid_red counter_price
    else print_grid_green counter_price;
    let upper = counter_price in
    let lower = counter_price -. !grid_size in
    if curr_price <= upper && curr_price >= lower then (
      update_global_bounds upper lower;
      print_curr_price curr_price)
    else print_endline "\n";
    let next_lower_price = counter_price -. !grid_size in
    graph_grid curr_price next_lower_price
      (curr_price < next_lower_price))

(********************************************************************
    Main Loop
 ********************************************************************)
(* helper function taking in a state instance and pretty printing out
   the price of the currency for that day. It also computes the decision
   of the algorithm and stores it in mutable global variables. Finally,
   it is responsible for sending live text-version updates to
   report.txt. *)
let print_report_in_loop st =
  let data = State.data_print coin_name_const st in
  print_endline @@ "Date: " ^ State.curr_date st coin_name_const;
  let indic_decision = evaluate_indicators @@ indicator_comb st in
  ansiterminal_print indic_decision;
  let all_time_profits =
    "All time profit: "
    ^ string_of_float
        (State.all_time_profit st coin_name_const !starting_pos)
  in
  let state_action_tup = State.decision_action st indic_decision in
  let algo_profits =
    "Algorithm profit: " ^ string_of_float (snd state_action_tup)
  in
  let market_info_string =
    market_positions_tostring (fst state_action_tup)
  in
  let step_data =
    data
    ^ deicision_to_string indic_decision
    ^ market_info_string ^ algo_profits ^ all_time_profits ^ "\n\n"
  in
  (step_data, market_info_string, state_action_tup)

(* helper function taking in input from currency and algorithm decision
   to pretty print comparison between algorithm and naive trades, as
   well as a live grid in the terminal *)
let pretty_print_grid st market_info_string state_action_tup =
  ANSITerminal.print_string [ ANSITerminal.blue ] market_info_string;
  profit_pretty_print "Algorithm profit: " (snd state_action_tup);
  profit_pretty_print "All time profit: "
    (State.all_time_profit st coin_name_const !starting_pos);
  ANSITerminal.print_string [ ANSITerminal.yellow ]
  @@ "Real time data is sent to " ^ report_file ^ "\n";
  update_file report_file !report;
  resize_graph (State.price_close st coin_name_const);
  graph_grid
    (State.price_close st coin_name_const)
    !grid_upper_limit
    (State.price_close st coin_name_const < !grid_upper_limit)

(** [main_loop state] is the repeating loop of our program that takes
    [state] from previous timestep and makes a decision, then receives
    new data from feeder and passes new state *)
let rec main_loop wait_period st =
  print_string "\n\n";
  let step_data, market_info_string, state_action_tup =
    print_report_in_loop st
  in
  report := step_data ^ !report;
  pretty_print_grid st market_info_string state_action_tup;
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

(********************************************************************
  Starting Trading Bot Program
  ********************************************************************)

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
            update_naive_pos new_data;
            State.init_state indicators budget new_data
            |> main_loop wait_period)
      else (
        print_string "Not a valid float. Please try again.\n>";
        set_wait_time_parameter ())

(* helper function updating the mutable upper and lower bounds to the
   bot's grid based on user input *)
let rec set_grid_bound_parameter (set_upper : bool) =
  match Stdlib.read_line () with
  | exception End_of_file -> ()
  | input ->
      if is_valid_input input then
        if set_upper then grid_upper_limit := float_of_string input
        else grid_lower_limit := float_of_string input
      else (
        ANSITerminal.print_string
          [ ANSITerminal.magenta ]
          "Not a valid float. Please try again.\n>";
        set_grid_bound_parameter set_upper)

(* reading from introduction.txt to print helpful information *)
let get_instruction_text () =
  Core.In_channel.read_lines "./introduction.txt"

(* helper function printing instructions to crypto trader if
   necessary *)
let rec print_instructions () =
  match Stdlib.read_line () with
  | exception End_of_file -> ()
  | command ->
      if is_valid_command command then
        match command with
        | "y"
        | "yes" ->
            ANSITerminal.print_string [ ANSITerminal.cyan ]
            @@ List.fold_left
                 (fun acc x -> acc ^ x)
                 ""
                 (get_instruction_text ())
        | _ -> print_newline ()
      else (
        ANSITerminal.print_string
          [ ANSITerminal.magenta ]
          "Not a valid command. Please try again.\n>";
        print_instructions ())

(** [main ()] asks users for parameters for grid trading algorithm, as
    well as stop time between each trade on historical data. Then starts
    the crypto trading bot *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ] intro_message;
  print_instructions ();
  ANSITerminal.print_string [ ANSITerminal.red ] upper_message;
  set_grid_bound_parameter true;
  ANSITerminal.print_string [ ANSITerminal.red ] lower_message;
  set_grid_bound_parameter false;
  grid_size :=
    (!grid_upper_limit -. !grid_lower_limit) /. float_of_int num_splits;
  ANSITerminal.print_string [ ANSITerminal.red ] pause_message;
  set_wait_time_parameter ()

(* Execute the game engine. *)
let () = main ()