(** Representation of a librarian.

    This module represents the account for the librarian. It handles adding
    books, viewing all available books, and accessing information from student
    account.*)

type lib
(** The abstract type representing the librarian*)

exception UnknownStudentID of Student.student_id
(** Raised when an unknown studentID is encountered. It carries the unknown
    student ID*)

exception UnknownBook of Library.book
(** Raise when an unknown book is encountered. It carries the name of the
    unknown book*)

val create_lib : string -> string -> int -> lib
(** [create_lib un pw sid] is a librarian with username [un] password [pw] and
    staff_id [sid]*)

val add_book : Library.library -> Library.book -> Library.book list
(** [add_book blst bk] is a new book list with all of blst and bk added.
    Requires [bk] is a valid book *)

val get_username : lib -> string 
(** [get_username l] is the username of librarian [l]*)
val get_password : lib -> string 
(** [get_password l] is the password of librarian [l]*)
val get_staff_id : lib -> int 
(** [get_staff_id l] is the staff id of librarian [l]*)

val remove_book : Library.library -> Library.book -> Library.book list
(** [remove_book blst bk] is a new blst with all elements except bk Requires
    [bk] is a valid book Riases [UnknownBook bk] if there is no book b in blst*)

val get_borrowed : Student.student -> Library.book list
(** [get_borrowed s] is a list of books this student is currently borrowing with
    no duplicates. Requires [s] is a student in the database.*)

val get_login_details : lib -> string * string
(**[get_login_details lib] is a pair of the username and password of librarian
   [std]*)
val view_books : Library.library -> Library.book list
(** [view_books] is a list of all the books from the module Library*)

val from_json : Yojson.Basic.t -> lib
(**[from_json j] is a librarian that [j] represents. Requires: [j] is a valid
   JSON database representation. *)