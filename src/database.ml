exception UnknownID of Student.student_id

open Yojson.Basic.Util

type database = {
  name : string;
  libraries : Library.library list;
  librarians : Librarian.lib list;
  students : Student.student list;
}

let create_database n =
  { name = n; libraries = []; librarians = []; students = [] }

let to_database (json : Yojson.Basic.t) =
  {
    name = json |> member "name" |> to_string;
    libraries =
      json |> member "libraries" |> to_list |> List.map Library.to_library;
    librarians =
      json |> member "librarians" |> to_list |> List.map Librarian.from_json;
    students =
      json |> member "students" |> to_list |> List.map Student.to_student;
  }

let add_library d l =
  let new_libraries = l :: d.libraries in
  { d with libraries = List.sort_uniq Stdlib.compare new_libraries }

(*TODO:No two students can have the same username *)
let add_student_account d l =
  let new_students = l :: d.students in
  { d with students = List.sort_uniq Stdlib.compare new_students }

(*TODO:No two librarians can have the same username*)
let add_librarian_account d l =
  let new_librarians = l :: d.librarians in
  { d with librarians = List.sort_uniq Stdlib.compare new_librarians }

let view_libraries d = d.libraries
let view_student_accounts d = d.students
let view_librarian_accounts d = d.librarians

let students_login (d : database) =
  view_student_accounts d |> List.map Student.get_login_details

let librarians_login (d : database) =
  view_librarian_accounts d |> List.map Librarian.get_login_details

let get_student un d =
  List.find (fun x -> Student.get_username x = un) (view_student_accounts d)

let get_librarian un d =
  List.find (fun x -> Librarian.get_username x = un) (view_librarian_accounts d)

let rec sort_helper blst acc =
  match blst with
  | [] -> acc
  | h :: t -> sort_helper t (Library.sort_books (h @ acc))

let sort_all_books d =
  let bll = List.map (fun x -> Library.view_books x) d.libraries in
  sort_helper bll []

let subset_by_genre d g =
    List.fold_left
      (fun acc x -> Library.subset_genre (Library.view_books x) g @ acc)
      [] d.libraries
  
let subset_by_author d a =
  List.fold_left
    (fun acc x -> Library.subset_author (Library.view_books x) a @ acc)
    [] d.libraries

(*mapping of author firstname:[list of author full names ] and lastname:[list of
  author full names ]*)
module AuthorNames = Map.Make (String)

let author_names name d =
  (*[add_to_map binding acc] is a map with all the bindings of [acc] with the
    value of [fst(binding)] updated to contain [snd(binding)]*)
  let add_to_map binding acc =
    match AuthorNames.find_opt (fst binding |> String.lowercase_ascii) acc with
    | None ->
        AuthorNames.(
          add (fst binding |> String.lowercase_ascii) [ snd binding ] acc
          |> add (snd binding |> String.lowercase_ascii) [ snd binding ])
    | Some k ->
        AuthorNames.add
          (fst binding |> String.lowercase_ascii)
          (snd binding :: k |> List.sort_uniq String.compare)
          acc
  in
  let create_map l =
    List.fold_left (fun acc x -> add_to_map x acc) AuthorNames.empty l
  in
  d.libraries
  |> List.fold_left (fun acc l -> Library.parse_author_names l @ acc) []
  |> create_map |> AuthorNames.find_opt name


let borrowed_categories (d:database) =
  let get_borrowed_books  list = List.map (fun x -> fst x)  list in 
  let books = List.fold_left (fun acc s -> acc @ ((Student.get_borrowed s)|>get_borrowed_books) ) []  d.students in 
  let add_genre (g:Library.genre) (acc:(Library.genre*int) list) = 
        match List.assoc_opt g acc with 
        | None -> (g,1)::acc
        | Some i-> let new_lst=(List.remove_assoc g acc)in (g,i+1)::new_lst in 
  let genres = List.map (fun bk -> Library.book_genre bk ) books in
  let result =  List.fold_left (fun acc g -> add_genre g acc) [] genres in 
  List.sort_uniq (fun x y -> Library.compare_genre (fst x) (fst y) ) result

    


