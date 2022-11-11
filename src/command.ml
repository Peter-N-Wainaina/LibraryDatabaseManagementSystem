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
  | Logout

let student_options () = [ "Borrowed Books"; "Favorite Books" ]

let student_help () =
  [
    "\n\
    \         Borrowed books - see the name of the books you have borrowed from \n\
    \        Cornell Libraries";
    "Favorite books - see the name of the books you have marked as favorite \n\
    \        before.";
  ]

let librarian_options () = []
let librarian_help () = []

let logging t =
  match t with
  | exception End_of_file -> Quit
  | "QUIT" | "Quit" | "quit" -> Quit
  | "Login" | "Log in" | "Log In" -> Login
  | "Log out" -> Logout
  | _ -> UnknownInput

let user_type t =
  match t with
  | exception End_of_file -> Quit
  | exception a -> UnknownInput
  | "QUIT" | "Quit" | "quit" | "Log out" -> Quit
  | "student" | "Student" -> Student
  | "librarian" | "Librarian" -> Librarian
  | _ -> UnknownInput

let options t =
  match t with
  | "QUIT" | "Quit" | "quit" | "Log out" -> Quit
  | "Options" | "options" -> Options
  | "Borrowed books" | "Borrowed Books" -> Borrowed_books
  | "Favorite books" | "Favorite Books" -> Favorite_books
  | "Help" | "HELP" -> Help
  | _ -> UnknownInput
