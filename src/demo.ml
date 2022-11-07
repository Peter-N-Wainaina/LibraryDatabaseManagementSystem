open Student
open Database
open Yojson.Basic.Util

type student = Student.student

let from_json json = json |> member "students" |> to_list |> List.map to_student
let data_dir_prefix = "data" ^ Filename.dir_sep

let database =
  Database.to_database
    (Yojson.Basic.from_file (data_dir_prefix ^ "database.json"))

let students_lst = Database.view_student_accounts database
let librarians_lst = Database.view_librarian_accounts database
let username_lst = students_lst |> List.map get_username

let student_db =
  Database.to_database
    (Yojson.Basic.from_file (data_dir_prefix ^ "database.json"))

let find_a_user a =
  let y = username_lst in
  List.find_opt (fun x -> a = x) y

let add_student un pw id =
  let v = create_student un pw id in
  v |> add_student_account student_db
