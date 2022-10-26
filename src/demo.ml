open Student
open Database
open Yojson.Basic.Util

type student = Student.student

let from_json json = json |> member "students" |> to_list |> List.map to_student
let data_dir_prefix = "data" ^ Filename.dir_sep

let student_accounts =
  Yojson.Basic.from_file (data_dir_prefix ^ "student_accounts.json")

let students_lst = student_accounts |> from_json
let username_lst = students_lst |> List.map get_username
let student_db = create_database "student_db"

let find_a_user a =
  let y = username_lst in
  List.find_opt (fun x -> a = x) y

let add_student un pw id =
  let v = create_student un pw id in
  v |> add_student_account student_db
