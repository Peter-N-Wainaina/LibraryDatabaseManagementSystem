(** This module is acts as an API that defines how the user interface interacts with the database *)


exception UserNameNotFound
exception IncorrectPassword
exception UnknownAuthor of string
exception MultipleAuthors of string list
exception NoBorrowedBooks

val get_student : string -> string -> Database.database -> Student.student
(**[get_student un pw db] is a student with username [un] and password [pw] in
   database [db]. Requires:[db] is a valid database Raises [UserNameNotFound] un
   if username [un] is not in [d] Raises [IncorrectPassword pw] if [pw] does not
   match [un]*)

val get_librarian : string -> string -> Database.database -> Librarian.lib
(**[get_librarian un pw db] is a librarian with username [un] and password [pw]
   in database [db]. Requires:[db] is a valid database Raises [UserNameNotFound]
   un if username [un] is not in [d] Raises [IncorrectPassword pw] if [pw] does
   not match [un]*)

val get_author_books : Database.database -> string -> string * Library.book list
(**[get_author_books d n] is an association list of the full name of author [n]
   and list of all books by author [n] Raises:[UnknownAuthor n] if [n] is not a
   valid first, last or full name of an author in [d] Raises:
   [MultipleAuthors(fullnames)] where fullnames is a list of all authors in [d]
   with name [n]*)


val get_borrowed_categories: Database.database -> (string * int) list
(**[get_borrowed_categories d] is a set-like association list of genres and the number of times a book of 
   that genre has been borrowed. 
   Raises:[ NoBorrowedBooks ] if no book has been borrowed from any library in the database.
    *)