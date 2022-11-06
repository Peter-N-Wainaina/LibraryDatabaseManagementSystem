open Yojson.Basic.Util
open Library
open Student

type lib = {
  username : string;
  password : string;
  staff_id : int;
}

exception UnknownStudentID of Student.student_id
exception UnknownBook of book

let create_lib un pw sid = { username = un; password = pw; staff_id = sid }
let add_book l (bk : Library.book) = view_books (Library.add_book l bk)
let get_username lib = lib.username
let get_password lib = lib.password
let get_staff_id lib = lib.staff_id

let rec remove_book l (bk : Library.book) =
  view_books (Library.remove_book l bk)

let get_first = function
  | x, y -> x

let get_borrowed (s : Student.student) =
  List.map get_first (Student.get_borrowed s)

let view_books l = Library.view_books l

let from_json j =
  {
    username = j |> member "username" |> to_string;
    password = j |> member "password" |> to_string;
    staff_id = j |> member "staff_id" |> to_int;
  }
