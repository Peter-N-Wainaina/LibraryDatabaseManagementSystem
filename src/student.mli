(**This module represent the account of a student.

  It handles borrowing books, viewing the list of borrowed books, save books as
  favorite, remove books from favorite list, see the average number of pages
  they read in the week, and their progress compared to previous weeks. *)

exception UnknownStudent
(** Raised when a student with a given student id does not exist in the
    database. *)

type student
(**The abstract type representing a student.*)

type student_id
(** The abstract type representing the StudentID of a student*)

val to_student : Yojson.Basic.t -> student
(**[to_student j] is a student that [j] represents. Requires: [j] is a valid
   JSON database representation. *)

val create_student : string -> string -> int -> student
(**[create_student un pw id] is a student with username un, password pw,
   student_id id, and empty borrowed_books and favorite_books list.*)

val favorite_books : student -> string list
(** [get_favorite std] is the list of the favorite books of student std.*)

val get_borrowed : student -> (Library.book * int) list
(** [get_borrowed std] is a list of books this student is currently borrowing
    with no duplicates, with the deadline.*)

val get_favorites : student -> Library.book list
(** [ get_favorite std] is the list of books the student has favorited*)

val borrowed_books : student -> string list
(** [borrowed_books std] is a list of the names of books the student is
    currently borrowing.*)

val get_username : student -> string
(** [get_username std] is the username of the student std.*)

val get_id : student -> student_id
(** [get_id std] is the student id of the student std*)

val borrow_book : student -> Library.book * int -> student
(** [borrow_book bk blst] is a new book list with all of blst and bk added.
    Requires [bk] is a valid book *)

val add_favorite : student -> Library.book -> student
(** [add_favorite std bk] is a new book list with all of favorite books of std
    and bk.*)

val find_pw : student list -> string -> string
(** [find_pw username] returns the password of the student with the given
    username.*)

val find_student : student list -> string -> student
(** [find_student username] returns the student with the given username.*)

val mean : float list -> float
(** [mean week_lst] is the average of the numbers in pages this student has read
    in the week. *)

val progress : float -> float -> float
(** [progress pst nw] is the percentage increase or decrease in the number of
    pages from pst to nw*)

val get_login_details : student -> string * string
(**[get_login_details std] is a pair of the username and password of student
   [std]*)
