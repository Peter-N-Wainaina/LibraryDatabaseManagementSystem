open Database
open Yojson.Basic.Util

exception UserNameNotFound
exception IncorrectPassword

let get_student un pw db =
  let logins = Database.students_login db in
  match List.assoc un logins with
  | exception Not_found -> raise UserNameNotFound
  | pw' ->
      if pw = pw' then Database.get_student un db else raise IncorrectPassword

let get_librarian un pw db =
  let logins = Database.librarians_login db in
  match List.assoc un logins with
  | exception Not_found -> raise UserNameNotFound
  | pw' ->
      if pw = pw' then Database.get_librarian un db else raise IncorrectPassword
