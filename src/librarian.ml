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

let rec remove_book l (bk : Library.book) =
  view_books (Library.remove_book l bk)

let get_first = function
  | x, y -> x

let get_borrowed (sid : Student.student) =
  List.map get_first (Student.get_borrowed sid)

let view_books l = Library.view_books l
