type command =
  | Back
  | Login
  | Quit
  | Log_out
  | UnknownInput
  | Student
  | Librarian
  | Options
  | Borrowed_books
  | Favorite_books
  | Genre of Library.genre
  | Help
  | Logout

let student_options () =
  [
    "Borrowed Books";
    "Favorite Books";
    "Fiction";
    "Autobiography";
    "Biography";
    "Fantasy";
    " HistoricalFiction";
    "Memoir";
    "Mystery";
    "NonFiction";
    "Novel";
    "Philosophy";
    "Religion";
    "ScienceFiction";
    "Thriller";
  ]

let student_help () =
  [
    "\n\
    \         Borrowed books - see the name of the books you have borrowed from \n\
    \        Cornell Libraries";
    "Favorite books - see the name of the books you have marked as favorite \n\
    \        before.";
    "You can access books by any of the following genre by typing :  \
     Autobiography,  Biography,  Fantasy , Fiction , HistoricalFiction , Memoir\n\
    \    , Mystery , NonFiction , Novel , Philosophy , Religion , \
     ScienceFiction ,\n\
    \    Thriller. ";
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
  | exception End_of_file -> Quit
  | "back" -> Back
  | "quit" -> Quit
  | "log out" -> Log_out
  | "options" -> Options
  | "borrowed books" -> Borrowed_books
  | "favorite books" -> Favorite_books
  | "fiction" as g -> Genre (Library.create_genre g)
  | "autobiography" as g -> Genre (Library.create_genre g)
  | "biography" as g -> Genre (Library.create_genre g)
  | "fantasy" as g -> Genre (Library.create_genre g)
  | "historicalfiction " as g -> Genre (Library.create_genre g)
  | "nonfiction" as g -> Genre (Library.create_genre g)
  | "mystery" as g -> Genre (Library.create_genre g)
  | "novel" as g -> Genre (Library.create_genre g)
  | "memoir" as g -> Genre (Library.create_genre g)
  | "philosophy" as g -> Genre (Library.create_genre g)
  | "religion" as g -> Genre (Library.create_genre g)
  | "sciencefiction" as g -> Genre (Library.create_genre g)
  | "thriller" as g -> Genre (Library.create_genre g)
  | "help" -> Help
  | _ -> UnknownInput

(** Philosophy | Religion | ScienceFiction | Thriller*)
