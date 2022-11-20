(** Representation of a library.

    This module represents a library. It contains a book type. It also contains
    a list with elements of type book *)

type library
(**The abstract type representing a library*)

type book
(**The abstract type representing a book*)

type genre
(**The abstract type representing a genre*)

exception UnknownBook of book
(** Raise when an unknown book is encountered. It carries the name of the
    unknown book.*)

val create_genre : string -> genre
(** [create_genre s] is the genre of string s. If [s] is not a valid genre,
    [create_genre s] fails with invalid genre*)

val create_book : string -> genre -> string -> int -> string -> book
(**[create_book n g a p d] is a book with name n, genre g ,author a , pages p,
   description d. *)

val book_name : book -> string
(**[book_name bk] returns the name of the book*)

val book_author : book -> string
(**[book_author bk] returns the author of book [bk]*)

val book_description : book -> string
(**[book_description bk] returns the description of book [bk]*)

val book_genre : book -> genre
(**[book_genre bk] returns the genre of book [bk]*)

val book_length : book -> int
(**[book_length bk] returns the number of pages of book [bk]*)

val create_library : string -> library
(**[create_library n] is a library with name [n]*)

val to_library : Yojson.Basic.t -> library
(**[to_library n ] is a library with name[n]*)

val add_book : library -> book -> library
(**[add_book l b] is library [l] with book [b] added to it's list of books*)

val view_books : library -> book list
(**[view_books l ] is the set-like list of all books in the library *)

val remove_book : library -> book -> library
(**[remove_book l bk] is the library with all books except bk Raises
   [UnknownBook b] if there is no book b in the library l*)

val genre_to_int : genre -> int
(**[genre_to_int g2] assigns an int i based on the alphabetical order of genres.
   Starts at i = 1*)

val compare_genre : genre -> genre -> int
(**[compare_genre g1 g2] compares genres based on Stdlib compare function.
   Orders genres in alphabetical order.*)

val compare_books : book -> book -> int
(**[compare_books b1 b2] compares books based on their genre.*)

val sort_books : book list -> book list
(**[sort_books b] is list of books without any duplicates, in sorted order with
   respect to the alphabetical ordering of their genre.*)

val subset_genre : book list -> genre -> book list
(**[subset_genre blst gen] is a list of books in booklist blst with the genre
   gen. Returns the empty list if there are no books with genre [gen] in book
   list [blst]*)

val subset_author : book list -> string -> book list
(**[subset_author blst auth] is a list of books in the booklist with author
   auth. Returns the empty list if there are no books with author [auth] in book
   list [blst]*)

val parse_author_names : library -> (string * string) list
(**[parse_author_names l] is a set-like association list mapping firstnames to
   fullnames and lastnames to fullnames of the authors of the books in library
   [l] *)
