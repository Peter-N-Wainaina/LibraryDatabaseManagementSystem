open OUnit2
open Dbms
open Librarian
open Library
open Student
open Database

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

let add_book_test (name : string) (l : library) (bk : Library.book)
    expected_output : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output (Librarian.add_book l bk))

let remove_book_test (name : string) (l : library) (bk : Library.book)
    expected_output : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output (Librarian.remove_book l bk))

let get_borrowed_test (name : string) (input : Student.student)
    (expected_output : book list) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output (Librarian.get_borrowed input))

let book1 =
  Library.create_book "book1" "fiction" "First Last" 100
    "This is book1 written by First Last. It has 100 pages and has the genre \
     of fiction"

let book2 =
  Library.create_book "book2" "fiction" "First2 Last2" 100
    "This is book2 written by First2 Last2. It has 100 page fiction book"

let book3 =
  Library.create_book "book3" "fantasy" "Greatest Author" 2
    "Author was so great, he only needed 2 pages"

let library1 = Library.create_library "Empty Library"
let library2 = Library.add_book library1 book1
let uris = Library.create_library "Uris"
let random_student = Student.create_student "user1" "abc123" 123

let librarian_tests =
  [
    add_book_test "add a book to an empty book list" library1 book1 [ book1 ];
    add_book_test "add a book to a book list with one element" library2 book2
      [ book1; book2 ];
    add_book_test "add a book to a book list with the book already in it"
      library2 book1 [ book1 ];
    remove_book_test "Remove a book from from a library with one" library2 book1
      [];
    remove_book_test "Remove a book from a library with two books"
      (Library.add_book library2 book2)
      book2 [ book1 ];
    get_borrowed_test "Get borrowed list of random student" random_student [];
  ]

let add_book_test_list (name : string) (l : Library.library) (b : Library.book)
    expected_output : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output
       (Library.add_book l b |> Library.view_books))

let remove_book_test (name : string) (l : Library.library) (bk : Library.book)
    expected_output : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output
       (Library.remove_book l bk |> Library.view_books))

let remove_book_test_raises (name : string) (l : Library.library)
    (bk : Library.book) : test =
  name >:: fun _ ->
  assert_raises (Library.UnknownBook bk) (fun _ -> Library.remove_book l bk)

(*Example libraries for testing. The integer value corresponds to the number of
  books in the library*)
let library0 = Library.create_library "Library Name"
let library1 = Library.add_book library0 book1
let library2 = Library.add_book library1 book2
let library3 = Library.add_book library2 book3

let library_tests =
  [
    add_book_test_list "Add a book to an empty library" uris book1 [ book1 ];
    add_book_test_list "Add two books to a library and test set-like properties"
      (Library.add_book uris book1)
      book2 [ book1; book2 ];
    add_book_test_list "Add a book already in library"
      (Library.add_book uris book1)
      book1 [ book1 ];
    add_book_test_list "Add more than two books"
      (Library.add_book (Library.add_book uris book1) book2)
      book3 [ book2; book3; book1 ];
    remove_book_test "Remove a book from from a library with one" library1 book1
      [];
    remove_book_test "Remove a book from a library with two books" library2
      book2 [ book1 ];
    remove_book_test
      "Remove a book from a library with more that two books to test set \
       properties"
      library3 book3 [ book2; book1 ];
    remove_book_test_raises "Test for an exception" library1 book2;
    remove_book_test_raises "Test for an exception" library2 book3;
  ]

(*Example databases for testing.*)
let db0 = Database.create_database "Empty Database"
let db1 = Database.add_library db0 library0
let db2 = Database.add_library db1 library1

(*Example student accounts*)
let student1 = Student.create_student "Eman" "Abdu" 29062003
let student2 = Student.create_student "Peter" "W" 20022002
let student3 = Student.create_student "Eman" "W" 29062003

(*Example librarian accounts*)

let librarian1 = Librarian.create_lib "Iqra" "Yousof" 123456
let librarian2 = Librarian.create_lib "Name" "Name2" 00000

let add_library_test (name : string) (d : Database.database)
    (l : Library.library) (expected_output : Library.library list) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output
       (l |> Database.add_library d |> Database.view_libraries))

let add_student_account_test (name : string) (d : Database.database)
    (s : Student.student) (expected_output : Student.student list) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output
       (s |> Database.add_student_account d |> Database.view_student_accounts))

(*TODO: Add tests using this *)
let add_librarian_account_test (name : string) (d : Database.database)
    (l : Librarian.lib) (expected_output : Librarian.lib list) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output
       (l
       |> Database.add_librarian_account d
       |> Database.view_librarian_accounts))

let database_tests =
  [
    add_library_test "Add a library to an empty database" db0 library0
      [ library0 ];
    add_library_test "Add a library to an a database with 1 library" db1
      library1 [ library0; library1 ];
    add_library_test "Add an existing library" db2 library1
      [ library0; library1 ];
    add_student_account_test "Add a student to an empty database" db0 student1
      [ student1 ];
    add_student_account_test "Add a student to a database with 1 student"
      (Database.add_student_account db0 student1)
      student2 [ student1; student2 ];
    add_student_account_test "Add an existing student"
      ((Database.add_student_account
          (Database.add_student_account db0 student1))
         student2)
      student2 [ student2; student1 ];
    add_librarian_account_test "Add a librarian to an empty database" db0
      librarian1 [ librarian1 ];
    add_librarian_account_test "Add a librarian to a database with 1 librarian"
      (Database.add_librarian_account db0 librarian1)
      librarian2 [ librarian1; librarian2 ];
    add_librarian_account_test "Add an existing librarian"
      (Database.add_librarian_account db0 librarian1)
      librarian1 [ librarian1 ];
  ]

let random_student_2 =
  Student.add_favorite (Student.create_student "user1" "abc123" 123) book1

let random_student_3 =
  Student.borrow_book (Student.create_student "user1" "abc123" 123) (book1, 1)

let random_student_4 = Student.add_favorite random_student_2 book2
let random_student_5 = Student.borrow_book random_student_3 (book2, 4)
let one_student_lst = [ random_student ]
let student_lst = [ random_student; student1; student2 ]

let favorite_book_tests (name : string) (input : student)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal true (cmp_set_like_lists expected_output (favorite_books input))

let get_borrowed_tests (name : string) (input : student)
    (expected_output : (book * int) list) : test =
  name >:: fun _ ->
  assert_equal true (cmp_set_like_lists expected_output (get_borrowed input))

let borrowed_books_tests (name : string) (input : student)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal true (cmp_set_like_lists expected_output (borrowed_books input))

let find_pw_tests (name : string) (lst : student list) (username : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (find_pw lst username)

let find_student_tests (name : string) (lst : student list) (username : string)
    (expected_output : student) : test =
  name >:: fun _ -> assert_equal expected_output (find_student lst username)

let mean_tests (name : string) (input : float list) (expected_output : float) :
    test =
  name >:: fun _ -> assert_equal expected_output (Student.mean input)

let student_tests =
  [
    favorite_book_tests "Favorites of empty list" random_student [];
    favorite_book_tests "Favorites of student with a non-empty favorites list"
      random_student_2 [ "book1" ];
    favorite_book_tests
      "Favorite books of student with more than one element favorite book list"
      random_student_4 [ "book1"; "book2" ];
    get_borrowed_tests "Borrowed books with deadline of empty list"
      random_student [];
    get_borrowed_tests
      "Borrowed books with deadline of student with single element borrowed \
       book list"
      random_student_3
      [ (book1, 1) ];
    get_borrowed_tests
      "Borrowed book with deadline of student with more than one element \
       borrowed book list"
      random_student_5
      [ (book1, 1); (book2, 4) ];
    borrowed_books_tests
      "Borrowed books of student with empty borrowed book list" random_student
      [];
    borrowed_books_tests
      "Borrowed books of student wtih single element borrowed book list"
      random_student_3 [ "book1" ];
    borrowed_books_tests
      "Borrowed books of student with multiple element borrowed book list"
      random_student_5 [ "book1"; "book2" ];
    find_pw_tests "Password of student in single element list" one_student_lst
      "user1" "abc123";
    find_pw_tests "Password of student in multiple element list" student_lst
      "Eman" "Abdu";
    find_student_tests "Find student given username in single element list"
      one_student_lst "user1" random_student;
    find_student_tests "Find student given username in multiple element list"
      student_lst "Eman" student1;
    mean_tests "Get mean of empty list" [] 0.0;
    mean_tests "Get mean of single element list" [ 1. ] 1.;
    mean_tests "Get mean of multiple element list" [ 1.; 3.; 5. ] 3.;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [ librarian_tests; student_tests; library_tests; database_tests ]

let _ = run_test_tt_main suite
