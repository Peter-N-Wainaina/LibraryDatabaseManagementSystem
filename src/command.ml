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
  | "QUIT" | "Quit" | "quit" -> Quit
  | "Sign Up" | "Sign up" -> Sign_up
  | "Login" | "Log in" | "Log In" -> Login
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
  | "QUIT" | "Quit" | "quit" -> Quit
  | "Options" | "options" -> Options
  | "Borrowed books" | "Borrowed Books" -> Borrowed_books
  | "Favorite books" | "Favorite Books" -> Favorite_books
  | "Help" | "HELP" -> Help
  | _ -> UnknownInput
