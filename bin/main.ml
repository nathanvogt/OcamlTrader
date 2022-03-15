(* open Indicator open State

   (* global variable for now focusing on single coin *) let coin_name =
   "ETH"

   (* helper function to concatenate input into valid file location *)
   let data_dir_prefix = "data" ^ Filename.dir_sep

   (* helper class that continues to recursively call play_game until
   valid file is entered *) let rec get_valid_file f = if
   Sys.file_exists f then f else begin print_string "The data file does
   not exist. Please try again:\n"; get_valid_file (data_dir_prefix ^
   read_line () ^ ".json") end

   (* helper function taking in record of indicators and outputing
   normalized metric to decide whether to buy or sell returns [0..100].

   later on, we can make this smarter by having the computer computer
   can the amount of money it invests based on its confidence level*)
   let evaluate_decision ind_rec = indicator_record_list ind_rec |>
   evaluate_decision_aux

   (* helper function receiving decision and taking corresponding action

   later need to make this smarter: don't sell when we don't have
   position sizes control the total budget it buys and sells -jun *) let
   execute_decision (range : int) = if range <= 30 then print_string
   "sell!" else if range <= 70 the print_string "wait!" else
   print_string "buy!"

   (* loop infinitely, waiting for periodic update from feeder and
   giving new indicators *) let rec ongoing_trade state coin_name
   data_file = let indicator_record = from_json data_file coin_name |>
   update_indicators in evaluate_decision indicator_record |>
   execute_decision; read_line () |> ongoing_trade indicator_record
   coin_name

   (** [start_game f] starts the trade with file [f]. *) let start_trade
   f = let init_state = get_valid_file f |> Yojson.Basic.from_file |>
   from_json coin_name in ongoing_trade init_state coin_name

   (* main method *) let main () = ANSITerminal.print_string [
   ANSITerminal.red ] "\n\nStarting crypto trader \n"; match read_line
   () with | exception End_of_file -> () | file_name -> start_trade
   (data_dir_prefix ^ file_name ^ ".json")

   (* Execute the game engine. *) let () = main () *)
