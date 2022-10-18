open OUnit2
open Dbms
open Librarian
open Library

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)

let cmp_set_like_lists lst1 lst2 =
    let uniq1 = List.sort_uniq compare lst1 in
       let uniq2 = List.sort_uniq compare lst2 in
          List.length lst1 = List.length uniq1
             && List.length lst2 = List.length uniq2
                && uniq1 = uniq2

let add_book_test (name :string) (l : library) (bk : Library.book) (expected_output) : test = 
  name >:: fun _ ->
    assert_equal true (cmp_set_like_lists expected_output (Librarian.add_book l bk))

(*let remove_book_test (name :string) (l : library) (bk : Library.book) (expected_output) : test = 
  name >:: fun _ ->
    assert_equal true (cmp_set_like_lists expected_output (Librarian.remove_book l bk))*)

let book1 = Library.create_book "book1" "fiction" "First Last" 100 "This is book1 written by First Last. It has 100 pages and has the genre of fiction"
let book2 = Library.create_book "book2" "fiction" "First2 Last2" 100 "This is book2 written by First2 Last2. It has 100 pages and has the genre of fiction"
let book3 = Library.create_book "book3"  "fantasy" "Greatest Author" 2 "Author was so great, he only needed 2 pages"

let library1 = Library.create_library "Empty Library" 
let library2 = add_book (Library.create_library "One book Library") book1
let uris = Library.create_library "Uris"

let librarian_tests=
[
  add_book_test "add a book to an empty book list" library1 book1 [book1];
  add_book_test "add a book to a book list with one element" library2 book2 ([book1; book2]);
  add_book_test "add a book to a book list with the book already in it" library2 book1 [book1];
  (*remove_book_test "remove a book from a book list with one element" library2 book1 [];
  remove_book_test "remove a book from a two element book list" (add_book library2 book2) book2 [book1];*)
]

let add_book_test_list (name:string) (l:Library.library)(b:Library.book)(expected_output) : test =
  name >:: fun _ -> 
    assert_equal true (cmp_set_like_lists expected_output (Library.add_book l b|>Library.view_books))

let library_tests=[
  add_book_test_list "Add a book to an empty library" uris book1 [book1];
  add_book_test_list "Add two books to a library and test set-like properties" (Library.add_book uris book1) book2 [book1;book2];
  add_book_test_list "Add a book already in library" (Library.add_book uris book1) book1 [book1];
  add_book_test_list "Add more than two books"(Library.add_book  (Library.add_book uris book1) book2) book3 [book2;book3;book1]

]

(*TODO: Add tests for student.ml*)
let student_tests=[]

let suite = 
  "test suite for final project"
  >::: List.flatten [librarian_tests;student_tests;library_tests]

let _ = run_test_tt_main suite
