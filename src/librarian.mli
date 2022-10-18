(** Representation of a librarian. 

This module represents the account for the librarian. It handles adding books, 
viewing all available books, and accessing information from student account.*)

type lib 
(** The abstract type representing the librarian*)

type book 
(** The abstract type representing a book*)

(*TODO:Change this to Staff ID*)
type studentID
(** The abstract type representing the id number of a student*)

val create_book: string -> string -> string  -> int->string-> book
(**Creates a new book given book name, genre, author, pages, and description *)

val add_book : book list -> book -> book list 
(** [add_book blst bk] is a new book list with all of blst and bk added. 
    Requires [bk] is a valid book *)

val get_borrowed : Student.student -> Student.book list
(** [get_borrowed sid] is a list of books this student is currently borrowing 
    with no duplicates.
    Requires [sid] is a valid student*)

val view_books : unit -> book list 
(** [view_books] is a list of all the books from the module Library*)