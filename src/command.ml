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

let parse_commands s =
  let str_lst =
    List.filter (fun x -> String.length x > 0) (String.split_on_char ' ' s)
  in
  List.fold_left (fun acc x -> acc ^ " " ^ String.lowercase_ascii x) "" str_lst
  |> String.trim

let logging t =
  match parse_commands t with
  | exception End_of_file -> Quit
  | "quit" -> Quit
  | "log in" -> Login
  | "log out" -> Logout
  | _ -> UnknownInput

let user_type t =
  match parse_commands t with
  | exception End_of_file -> Quit
  | exception a -> UnknownInput
  | "quit" -> Quit
  | "student" -> Student
  | "librarian" -> Librarian
  | _ -> UnknownInput

let options t =
  match parse_commands t with
  | "quit" | "log out" -> Quit
  | "options" -> Options
  | "borrowed books" -> Borrowed_books
  | "favorite books" -> Favorite_books
  | "help" -> Help
  | _ -> UnknownInput
