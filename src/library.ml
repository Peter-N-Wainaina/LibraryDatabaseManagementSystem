type book = {
  name : string;
  genre : string;
  author : string;
  pages : int;
  description : string;
}

exception UnknownBook of book

(*name is the name of the library all_books is the list of all the books in the
  library*)
type library = {
  name : string;
  all_books : book list;
}

let create_book n g a p d =
  { name = n; genre = g; author = a; pages = p; description = d }

let book_name (bk : book) = bk.name
let create_library n = { name = n; all_books = [] }

let add_book l b =
  let books = b :: l.all_books in
  { l with all_books = List.sort_uniq Stdlib.compare books }

let view_books l = l.all_books

let remove_book l bk =
  if not (List.mem bk l.all_books) then raise (UnknownBook bk)
  else
    let books = List.filter (fun x -> x <> bk) l.all_books in
    { l with all_books = books }
