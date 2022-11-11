open Database
open Yojson.Basic.Util

exception UserNameNotFound of string
exception IncorrectPassword of string

(**TODO:Delete once bin/main.ml is updated to use get_student*)
let find_a_user a d =
  let user_names = Database.student_user_names d in
  List.find_opt (fun x -> a = x) user_names

let get_student un pw db =
  let logins = Database.students_login db in
  match List.assoc un logins with
  | exception Not_found -> raise (UserNameNotFound un)
  | pw' ->
      if pw = pw' then Database.get_student un db
      else raise (IncorrectPassword pw)

let get_librarian un pw db =
  let logins = Database.librarians_login db in
  match List.assoc un logins with
  | exception Not_found -> raise (UserNameNotFound un)
  | pw' ->
      if pw = pw' then Database.get_librarian un db
      else raise (IncorrectPassword pw)
