open Yojson.Basic.Util

type genre =
  | Autobiography
  | Biography
  | Fantasy
  | Fiction
  | HistoricalFiction
  | Memoir
  | Mystery
  | NonFiction
  | Novel
  | Philosophy
  | Religion
  | ScienceFiction
  | Textbook
  | Thriller

type book = {
  name : string;
  genre : genre;
  author : string;
  pages : int;
  description : string;
}

let create_genre s =
  match String.lowercase_ascii s with
  | "nonfiction" -> NonFiction
  | "fiction" -> Fiction
  | "memoir" -> Memoir
  | "philosophy" -> Philosophy
  | "autobiography" | "Autobiography" -> Autobiography
  | "religion" -> Religion
  | "sciencefiction" -> ScienceFiction
  | "mystery" -> Mystery
  | "thriller" -> Thriller
  | "biography" -> Biography
  | "historicalfiction" -> HistoricalFiction
  | "fantasy" -> Fantasy
  | "novel" -> Novel
  | "textbook" -> Textbook
  | _ -> raise (Failure "invalid")

exception UnknownBook of book
exception InvalidAuthor of string

(*name is the name of the library all_books is the list of all the books in the
  library*)
type library = {
  name : string;
  all_books : book list;
}

let create_library n = { name = n; all_books = [] }

let create_book n g a p d =
  { name = n; genre = g; author = a; pages = p; description = d }

let book_name (bk : book) = bk.name
let book_author (bk : book) = bk.author
let book_description (bk : book) = bk.description
let book_length (bk : book) = bk.pages
let book_genre (bk : book) = bk.genre

let to_book j =
  {
    name = j |> member "name" |> to_string;
    genre = j |> member "genre" |> to_string |> create_genre;
    author = j |> member "author" |> to_string;
    pages = j |> member "pages" |> to_int;
    description = j |> member "description" |> to_string;
  }

let to_library j =
  {
    name = j |> member "libraryname" |> to_string;
    all_books = j |> member "all_books" |> to_list |> List.map to_book;
  }

let add_book l b =
  let books = b :: l.all_books in
  { l with all_books = List.sort_uniq Stdlib.compare books }

let view_books l = l.all_books

let remove_book l bk =
  if not (List.mem bk l.all_books) then raise (UnknownBook bk)
  else
    let books = List.filter (fun x -> x <> bk) l.all_books in
    { l with all_books = books }

let genre_to_int = function
  | Autobiography -> 1
  | Biography -> 2
  | Fantasy -> 3
  | Fiction -> 4
  | HistoricalFiction -> 5
  | Memoir -> 6
  | Mystery -> 7
  | NonFiction -> 8
  | Novel -> 9
  | Philosophy -> 10
  | Religion -> 11
  | ScienceFiction -> 12
  | Textbook -> 13
  | Thriller -> 14

let compare_genre g1 g2 =
  let i1, i2 = (genre_to_int g1, genre_to_int g2) in
  i1 - i2

let compare_books b1 b2 =
  match (b1, b2) with
  | ( { name; genre = g1; author; pages; description },
      { name = n; genre = g2; author = a2; pages = p; description = d } ) ->
      compare_genre g1 g2

let sort_books blst = List.sort_uniq compare_books blst
let subset_genre bl gen = List.filter (fun x -> x.genre = gen) bl
let subset_author bl auth = List.filter (fun x -> x.author = auth) bl

let parse_author_names l =
  let sort_pairs p1 p2 =
    match
      (String.compare (fst p1) (fst p2), String.compare (snd p1) (snd p2))
    with
    | 0, 0 -> 0
    | 0, k -> k
    | k, _ -> k
  in
  let rec create_pairs fn acc l =
    match l with
    | [] -> acc
    | h :: t -> create_pairs fn ((h, fn) :: acc) t
  in
  let parse_names acc n =
    let full_name = n |> String.trim in
    full_name |> String.split_on_char ' ' |> create_pairs full_name acc
  in
  l.all_books
  |> List.fold_left (fun acc bk -> parse_names acc bk.author) []
  |> List.sort_uniq sort_pairs
