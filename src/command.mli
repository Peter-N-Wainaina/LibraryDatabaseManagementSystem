(** A representation of a command This module represents all available commands*)

type command
(**The abstract representation of a command*)

val logging : string -> command
(***)

val user_type : string -> command
(***)

val options : string -> command
(***)
