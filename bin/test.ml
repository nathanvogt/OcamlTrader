(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Go of object_phrase
  | Take of object_phrase
  | Drop of object_phrase
  | Score
  | Inventory
  | Quit

exception Empty
exception Malformed

(* helper function to get command from stirng list, returns command *)
let grab_command = function
  | [] -> raise Empty
  | [ x ] ->
      if x = "quit" then Quit
      else if x = "score" then Score
      else if x = "inventory" then Inventory
      else raise Malformed
  | h :: t ->
      if h = "go" then Go t
      else if h = "take" then Take t
      else if h = "drop" then Drop t
      else raise Malformed

let parse str =
  String.split_on_char ' ' (String.trim str)
  |> List.filter (fun s -> s <> "")
  |> grab_command
