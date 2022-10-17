open OUnit2
open Dbms
open Librarian 


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



let add_book_test (name :string) (bl : Librarian.book list) (bk : Librarian.book) (expected_output : Librarian.book list) : test = 
  name >:: fun _ ->
    assert_equal true (cmp_set_like_lists expected_output (add_book bl bk))


let book1 = create_book "book1" "fiction" "First Last" 100 "This is book1 written by First Last. It has 100 pages and has the genre of fiction"
  
let book2 = create_book "book2" "fiction" "First2 Last2" 100 "This is book2 written by First2 Last2. It has 100 pages and has the genre of fiction"

let librarian_tests=
[
  add_book_test "add a book to an empty book list" [] book1  [book1];
  add_book_test "add a book to a book list with one element" [book1] book2  ([book1; book2]);
  add_book_test "add a book to a book list with the book already in it" [book1] book1 [book1];
]

let suite = 
  "test suite for final project"
  >::: List.flatten [librarian_tests]

let _ = run_test_tt_main suite

