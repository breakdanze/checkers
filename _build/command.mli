(** 
    Parses commands. 
*)

(** The type [coordinate] represents a position on the board in the format 
    [(row * column)]. The first element of the tuple must be a letter and the 
    second element of the tuple must be a digit.*)
type coordinate = (string * string)

(** The type move_phrase represents a change in position on the board from one 
    coordinate to another. Each element is a word. *)
type move_phrase = string list

(** The type [command] represents a player command in the form of a verb or a 
    verb and a coordinate pair. *)
type command = 
  | Move of move_phrase
  | Help
  | Quit 

(** Raised when a command is empty *)
exception Empty 

(** Raised when a command is malformed *)
exception Malformed

(** [parse str] parses a player input into a [command]. The first word is the 
    verb, and any remaining words are a coordinate pair.
    Requires: The contents of [str] are alphanumeric or spaces. 
    Raises: [Empty] if [str] is the empty string or only spaces. 
    Raises: Malformed if the command is malformed (i.e. the verb is not a valid 
    choice or if "move" is given without a move_phrase or with an invalid 
    move_phrase. 
    - A move phrase is invalid if it is not two elements, each representing a 
      coordinate
*)
val parse : string -> command