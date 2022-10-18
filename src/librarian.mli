(** Representation of a librarian. 

This module represents the account for the librarian. It handles adding books, 
viewing all available books, and accessing information from student account.*)

type lib 
(** The abstract type representing the librarian*)

type studentID
(** The abstract type representing the id number of a student*)

exception UnknownStudentID of studentID
(** Raised when an unknown studentID is encountered. It carries the unknown 
    student ID*)

exception UnknownBook of Library.book
(** Raise when an unknonw book is encountered. It carries the name of the 
    unknown book*)

val add_book : Library.library -> Library.book -> Library.book list
(** [add_book blst bk] is a new book list with all of blst and bk added. 
    Requires [bk] is a valid book *)

val remove_book : Library.library -> Library.book -> Library.book list
(** [remove_book blst bk] is a new blst with all elements except bk
    Requires [bk] is a valid book*)

val get_borrowed : studentID -> Student.book list
(** [get_borrowed sid] is a list of books this student is currently borrowing 
    with no duplicates.
    Requires [sid] is a valid student
    Raises [UnknownStudentID sid] if there is no student with that ID *)

val view_books : Library.library -> Library.book list 
(** [view_books] is a list of all the books from the module Library*)