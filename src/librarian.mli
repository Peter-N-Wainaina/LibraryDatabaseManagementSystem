(** Representation of a librarian. 

This module represents the account for the librarian. It handles adding books, 
viewing all available books, and accessing information from student accounts.*)

type lib 
(** The abstract type representing the librarian*)

type book 
(** The abstract type representing a book*)

type studentID
(** The abstract type representing the id number of a student*)

(*(TODO: Write a better spec)*)
val create_book: string -> string -> string  -> int->string-> book
(**Creates a new book given*)

val add_book : book list -> book -> book list 
(** [add_book blst bk] is a new book list with all of blst and bk added. 
    Requires [bk] is a valid book *)

val get_borrowed : studentID -> book list
(** [get_borrowed sid] is a list of books this student is currently borrowing 
    with no duplicates.
    Requires [sid] is a valid student ID*)

val view_books : unit -> book list 
(** [view_books] is a list of all the books in the library*)