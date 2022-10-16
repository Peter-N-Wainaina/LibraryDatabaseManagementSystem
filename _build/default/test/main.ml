open OUnit2
open Dbms
open Librarian 


let add_book_test (name :string) (bl : book list) (bk : book) (expected_output : book list) : test = 
  name >:: fun _ ->
    assert_equal true (cmp_set_like_lists expected_output (add_book bl bk))

let librarian_tests=[

let book1 = {name = "book1"; genre = "fiction"; author = "First Last"; pages = 100; description = "This is book1 written by First Last. It has 100 pages and has the genre of fiction."}
let book2 = {name = "book2"; genre = "fiction"; author = "First2 Last2"; pages = 100; description = "This is book2 written by First2 Last2. It has 100 pages and has the genre of fiction."}

add_book_test "add a book to an empty book list" [] book1  [book1];
add_book_test "add a book to a book list with one element" [book1] book2  ([book1; book2]);
add_book_test "add a book to a book list with the book already in it" [book1] book1 [book1];
]

let suite = 
  "test suite for final project"
  >::: List.flatten [librarian_tests]

let _ = run_test_tt_main suite

