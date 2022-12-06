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
  | Author of string
  | Help
  | Logout
  | All_Books

let book_categories =
  "1. The following are the commands to access your book categories: \n\
   \t\t * Borrowed Books\n\
  \ \t\t * Favorite Books\n"

let genres =
  "2. To access books by genre: \n\
  \ \t\t* Fiction * Autobiography * Biography * Philosophy\n\
  \   \t\t* Fantasy * HistoricalFiction * Memoir  * Mystery \n\
  \   \t\t* NonFiction  * Novel * Religion  * ScienceFiction \n\
  \    \t\t* Thriller\n\
  \     "

let author_books =
  "3. To access books by a specific author, type in the author's name prefaced \
   by Author e.g \n\
   \t\t Author Alex Trebek\n"

let all_books = "To access all books in the database, type All Books."

let student_options () = [ book_categories; genres; author_books; "4. "^all_books ]

let student_help () =
  [
    "1. Borrowed books: Lists the names of all the books you have borrowed\n";
    "2. Favorite books: Lists the names of the books you have marked as \
     favorite \n";
    "3. Author Name: Lists the books by witten by Name\n";
    "4. All books : Lists all the books in the database"
  ]

let librarian_options () = ["1. "^all_books]
let librarian_help () = ["4. All books : Lists all the books in the database"]

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

(*[parse_author_name s] is command Author (name) where name is the author's name
  in [s] or command UnknownInput if [s] is not a valid author command*)
let parse_author_name s =
  match String.split_on_char ' ' s with
  | h1 :: h2 :: t ->
      if h1 = "author" then
        let name =
          List.fold_left
            (fun acc x -> acc ^ " " ^ String.lowercase_ascii x)
            "" (h2 :: t)
          |> String.trim
        in
        Author name
      else UnknownInput
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
  | "all books" -> All_Books
  | x -> parse_author_name x