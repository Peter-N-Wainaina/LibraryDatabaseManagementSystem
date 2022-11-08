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
    libraries = [];
    librarians = [];
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

let get_student un d =
  List.find (fun x -> Student.get_username x = un) (view_student_accounts d)
