(** Parsing of player commands. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

type object_phrase = string list
(** The type [object_phrase] represents the object phrase that can be
    part of a player command. Each element of the list represents a word
    of the object phrase, where a {i word} is defined as a consecutive
    sequence of non-space characters. Thus, no element of the list
    should contain any leading, internal, or trailing spaces. The list
    is in the same order as the words in the original player command.
    For example:

    - If the player command is ["go clock tower"], then the object
      phrase is [\["clock"; "tower"\]].

    - If the player command is ["go clock     tower"], then the object
      phrase is again [\["clock"; "tower"\]].

    An [object_phrase] is not permitted to be the empty list. *)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Go of object_phrase
  | Take of object_phrase
  | Drop of object_phrase
  | Score
  | Inventory
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase. Examples:

    - [parse "    go   clock   tower   "] is [Go \["clock"; "tower"\]]
    - [parse "quit"] is [Quit]
    - [parse "score"] is [Score]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is neither "quit" nor "go", or if the verb
    is "quit" and there is a non-empty object phrase, or if the verb is
    "go" and there is an empty object phrase.*)
