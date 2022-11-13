(** A representation of a command This module represents all available commands*)

type command =
  | Login
  | Quit
  | UnknownInput
  | Student
  | Librarian
  | Options
  | Borrowed_books
  | Favorite_books
  | Help
  | Logout  (**The abstract representation of a command*)

val logging : string -> command
(**[logging t] parses the user input and returns a [command]. Raises
   [UnkownInput] if the command [t] is invalid.*)

val user_type : string -> command
(**[user_type t] is [Command.Student] or [Command.Librarian]. Raises:
   [UnknownInput] if the command [t] is invalid.*)

val options : string -> command
(**[options t] is command [t] if [t] is a valid command. Raises: [UnknownInput]
   if the command [t] is invalid.*)

val student_options : unit -> string list
(** [student_options ()] is the list of valid student commands.*)

val student_help : unit -> string list
(** [student_help ()] is the list of valid student commands and their
    descriptions.*)

val librarian_options : unit -> string list
(** [librarian_options ()] is the list of valid librarian commands.*)

val librarian_help : unit -> string list
(** [librarian_help ()] is the list of valid librarian commands and their
    descriptions.*)

val parse_commands : string -> string
(**[parse_commands s] is a string with all the words in [s] separated by a
   single space converted to lowecase Example:
   [parse_commands  "    Borrowed  Books "] is ["borrowed books"]*)
