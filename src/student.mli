(*This module represent the account of a student. 

It handles borrowing books, viewing the list of borrowed books,
save books as favorite, remove books from favorite list, see the average 
number of pages they read in the week, and their progress compared to 
previous weeks.  *)

type student
(**The abstract type representing a student.*)

type book 
(** The abstract type representing a book*)

type studentID
(** The abstract type representing the id number of a student*)

val get_borrowed : student -> book list 
(** [get_borrowed std] is a list of books this student is currently borrowing 
    with no duplicates.*)
  
val borrow_book: student -> book list
(** [borrow_book bk blst] is a new book list with all of blst and bk added. 
    Requires [bk] is a valid book *)

val mean: float list -> float 
(** [mean week_lst] is the average of the numbers in pages this student 
    has read in the week. *)

val progress: float -> float -> float 
(** [progress pst nw] is the percentage increase or decrease 
in the number of pages from pst to nw*)

