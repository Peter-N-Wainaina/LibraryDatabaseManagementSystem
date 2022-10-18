(** Representation of a library. 

This module represents a library. It contains a book type. It also contains a list with elements of type book *)

type library 
(**The abstract type representing a library*)

type book
(**The abstract type representing a book*)

exception UnknownBook of book
(** Raise when an unknown book is encountered. It carries the name of the 
    unknown book.*)

val create_book: string -> string -> string  -> int->string-> book
(*[create_book n g a p d] is a book with name n, genre g ,author a , pages p, description d*)

val create_library: string  -> library
(**[create_library n ] is a library with name[n] and no  books*) 

val add_book: library -> book ->library
(**[add_book l b] is library [l] with book [b] added to it's list of books*)

val view_books: library -> book list
(**[view_books l ] is the set-like list of all books in the library *)

val remove_book : library -> book -> library 
(**[remove_book l bk] is the library with all books except bk 
    Raises [UnknownBook b] if there is no book b in the library l*)
