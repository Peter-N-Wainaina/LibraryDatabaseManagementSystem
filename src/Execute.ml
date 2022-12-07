open Database
open Yojson.Basic.Util

exception UserNameNotFound
exception IncorrectPassword
exception UnknownAuthor of string
exception MultipleAuthors of string list
exception NoBorrowedBooks


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

let get_author_books d n =
  match Database.author_names n d with
  | None -> raise (UnknownAuthor n)
  | Some k ->
      if List.length k > 1 then raise (MultipleAuthors k)
      else
        let fn = List.nth k 0 in
        (fn, Database.subset_by_author d fn)

let get_borrowed_categories d = 
  let bg_books = Database.borrowed_categories d  in 
  let bs_books = (
     match bg_books with 
    |[] -> raise (NoBorrowedBooks)
    | l -> List.map (fun (g,n) -> (Library.string_of_genre g,n) ) bg_books )
in List.sort (fun (_,i1) (_,i2) -> (if i1=i2 then 0 else if i1<i2 then -1 else 1) ) bs_books|>List.rev