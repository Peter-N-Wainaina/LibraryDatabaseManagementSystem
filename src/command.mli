(** A representation of a command This module represents all available commands*)

type command =
  | Sign_up
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
(***)

val user_type : string -> command
(***)

val options : string -> command
(***)
