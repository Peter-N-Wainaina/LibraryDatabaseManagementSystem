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

(**TODO:DELETE after changing main to get usernames and passwords at the same
   time*)
let student_user_names d =
  view_student_accounts d |> List.map Student.get_username

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
  let list_with_dup =
    List.fold_left
      (fun acc x -> (Library.subset_genre (Library.view_books x)) g @ acc)
      [] d.libraries
  in
  Library.sort_books list_with_dup

let subset_by_author d a =
  let list_with_dup =
    List.fold_left
      (fun acc x -> Library.subset_author (Library.view_books x) a @ acc)
      [] d.libraries
  in
  Library.sort_books list_with_dup
