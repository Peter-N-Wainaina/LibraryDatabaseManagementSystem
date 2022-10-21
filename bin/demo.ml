(*For use in the demo. All functionality will be moved to Command*)

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

let logging t =
  match t with
  | exception End_of_file -> Quit
  | "QUIT" -> Quit
  | "Quit" -> Quit
  | "quit" -> Quit
  | "Sign Up" -> Sign_up
  | "Sign up" -> Sign_up
  | "Login" -> Login
  | "Log in" -> Login
  | "Log In" -> Login
  | _ -> UnknownInput

let user_type t =
  match t with
  | exception End_of_file -> Quit
  | "QUIT" -> Quit
  | "Quit" -> Quit
  | "quit" -> Quit
  | "student" -> Student
  | "Student" -> Student
  | "librarian" -> Librarian
  | "Librarian" -> Librarian
  | _ -> UnknownInput

let options t =
  match t with
  | "QUIT" -> Quit
  | "Quit" -> Quit
  | "quit" -> Quit
  | "Options" -> Options
  | "options" -> Options
  | "Borrowed books" -> Borrowed_books
  | "Borrowed Books" -> Borrowed_books
  | "Favorite books" -> Favorite_books
  | "Favorite Books" -> Favorite_books
  | "Help" -> Help
  | "HELP" -> Help
  | _ -> UnknownInput
