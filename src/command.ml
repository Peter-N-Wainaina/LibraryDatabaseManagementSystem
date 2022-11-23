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

let book_categories =
  "1. To access book categories: \n\t\t Borrowed Books\n \t\t Favorite Books\n"

let genres =
  "2. To access books be genre: \n\
  \ \t\tFiction \n\
  \  \t\tAutobiography\n\
  \ \t\tBiography \n\
   \t\tPhilosophy;\n\
  \     \t\tFantasy\n\
  \ \t\tHistoricalFiction \n\
  \ \t\tMemoir\n\
  \ \t\tMystery \n\
  \ \t\tNonFiction\n\
  \ \t\tNovel\n\
  \ \t\tReligion\n\
  \ \t\tScienceFiction \n\
  \ \t\tThriller\n\n\
  \   "

let author_books =
  "3. To access books by a specific author, type in the author's name prefaced \
   by Author e.g \n\
   \t\t Author Alex Trebek"

let student_options () = [ book_categories; genres; author_books ]

let student_help () =
  [
    "1. Borrowed books: Lists the names of all the books you have borrowed";
    "2. Favorite books: Lists the names of the books you have marked as \
     favorite ";
    "3. You can access books by any of the following genre by typing :\n\
    \  \t\tAutobiography\n\
     \t\tBiography\n\
     \t\tFantasy \n\
     \t\tFiction\n\
    \  \t\tHistoricalFiction\n\
    \  \t\tMemoir\n\
    \     \t\tMystery\n\
    \  \t\tNonFiction\n\
    \  \t\tNovel\n\
    \  \t\tPhilosophy\n\
    \  \t\tReligion \n\
     \t\tScienceFiction\n\
     \t\tThriller\n\
    \     ";
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
  | x -> parse_author_name x