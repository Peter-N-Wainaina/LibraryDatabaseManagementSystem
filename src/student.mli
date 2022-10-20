(*This module represent the account of a student. 

It handles borrowing books, viewing the list of borrowed books,
save books as favorite, remove books from favorite list, see the average 
number of pages they read in the week, and their progress compared to 
previous weeks.  *)

exception UnknownStudent 
(** exception raised when a student with a given student id does not exist in 
    the database. *)
type student
(**The abstract type representing a student.*)

val to_student_list : Yojson.Basic.t -> student

val create_student : string -> string -> int -> student
(**[create_student un pw id] is a student with username un, password pw, 
    student_id id, and empty borrowed_books and favorite_books list.*)

val get_favorite : student -> Library.book list 
(** [get_favorite std]  is the list of the favorite books of student std.*)
val get_borrowed : student -> (Library.book * int) list
(** [get_borrowed std] is a list of books this student is currently borrowing 
    with no duplicates.*)

val get_username : student -> string 
(** [get_username std] is the username of the student std.*)

val get_id : student -> int
(** [get_id std] is the student id of the student std*)
  
val borrow_book: student -> (Library.book * int) -> student
(** [borrow_book bk blst] is a new book list with all of blst and bk added. 
    Requires [bk] is a valid book *)

val add_favorite : student -> Library.book -> student 
(** [add_favorite std bk] is a new book list with all of favorite books of std
    and bk.*)
    
val mean: float list -> float 
(** [mean week_lst] is the average of the numbers in pages this student 
    has read in the week. *)

val progress: float -> float -> float 
(** [progress pst nw] is the percentage increase or decrease 
in the number of pages from pst to nw*)
