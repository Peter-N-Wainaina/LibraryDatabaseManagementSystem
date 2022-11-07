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

let get_student d id =
  let student =
    List.filter (fun x -> Student.get_id x = id) (view_student_accounts d)
  in
  if List.length student = 0 then raise (UnknownID id) else List.nth student 0
