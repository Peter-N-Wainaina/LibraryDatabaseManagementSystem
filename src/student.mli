(*This module represent the account of a student. 

It handles borrowing books, viewing the list of borrowed books,
save books as favorite, remove books from favorite list, see the average 
number of pages they read in the week, and their progress compared to 
previous weeks.  *)

type student
(**The abstract type representing a student.*)

val create_student : string -> string -> int -> student
(**[create_student un pw id] is a student with username un, password pw, 
    student_id id, and empty borrowed_books and favorite_books list.*)

val get_borrowed : student -> (Library.book * int) list
(** [get_borrowed std] is a list of books this student is currently borrowing 
    with no duplicates.*)
  
val borrow_book: student -> (Library.book * int) -> student
(** [borrow_book bk blst] is a new book list with all of blst and bk added. 
    Requires [bk] is a valid book *)

val mean: float list -> float 
(** [mean week_lst] is the average of the numbers in pages this student 
    has read in the week. *)

val from_json: Yojson.Basic.t -> student list

val username_list: student list-> string list 
(** [username json] is the list of usernames of *)
val progress: float -> float -> float 
(** [progress pst nw] is the percentage increase or decrease 
in the number of pages from pst to nw*)

