open OUnit2
open Dbms
open Librarian
open Library
open Student
open Database
open Command
open Yojson.Basic.Util

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let data_dir_prefix = "data" ^ Filename.dir_sep

let database_json = Yojson.Basic.from_file (data_dir_prefix ^ "/database.json")
let accounts = Yojson.Basic.from_file (data_dir_prefix ^ "database.json")

let from_json_librarians json =
  json |> member "librarians" |> to_list |> List.map Librarian.from_json

let librarians_lst = accounts |> from_json_librarians

let from_json_students json =
  json |> member "students" |> to_list |> List.map to_student

let students_lst = accounts |> from_json_students

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

let get_username_tests (name : string) (input : Librarian.lib)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Librarian.get_username input)

let fiction = Library.create_genre "fiction"
let nonfiction = Library.create_genre "nonfiction"
let mystery = Library.create_genre "mystery"
let autobiography = Library.create_genre "autobiography"
let biography = Library.create_genre "biography"
let fantasy = Library.create_genre "fantasy"
let philosophy = Library.create_genre "philosophy"
let memoir = Library.create_genre "memoir"
let book1 = Library.create_book "book1" fiction "First Last" 100 "This is book1"

let book2 =
  Library.create_book "book2" nonfiction "First2 Last2" 100 "This is book2"

let book3 =
  Library.create_book "book3" mystery "Greatest Author" 2
    "Author was so great, he only needed 2 pages"

let book4 = Library.create_book "book4" autobiography "a4" 100 "This is book4"
let book5 = Library.create_book "book5" biography "a4" 100 "This is book5"
let book6 = Library.create_book "book6" fantasy "a6" 100 "This is book6"
let library1 = Library.create_library "Empty Library"
let library2 = Library.add_book library1 book1
let uris = Library.create_library "Uris"
let random_student = Student.create_student "user1" "abc123" 123
let json_lib = create_lib "librarian1" "password1" 1111

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
    get_username_tests "Username of first librarian in database is librarian1"
      (List.hd librarians_lst) "librarian1";
    get_username_tests "Username of second librarian in database is librarian2"
      (librarians_lst |> List.tl |> List.hd)
      "librarian2";
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

let sort_books_tests (name : string) (b : book list)
    (expected_output : book list) : test =
  name >:: fun _ -> assert_equal expected_output (Library.sort_books b)

(*Example libraries for testing. The integer value corresponds to the number of
  books in the library*)
let library0 = Library.create_library "Library Name"
let library1 = Library.add_book library0 book1
let library2 = Library.add_book library1 book2
let library3 = Library.add_book library2 book3
let six_bk_lst = [ book1; book2; book3; book4; book5; book6 ]
let library4 = Library.add_book library3 book4
let library5 = Library.add_book library4 book5

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
    sort_books_tests "Sort empty list of books" [] [];
    sort_books_tests "Sort single element list of books" [ book1 ] [ book1 ];
    sort_books_tests "Sort list of 6 books" six_bk_lst
      [ book4; book5; book6; book1; book3; book2 ];
  ]

(*Example databases for testing.*)
let db0 = Database.create_database "Database0"
let db1 = Database.add_library db0 library0
let db2 = Database.add_library db1 library1
let db_test = Database.add_library db2 library5

let db3 =
  Database.to_database
    (Yojson.Basic.from_file (data_dir_prefix ^ "database.json"))

(*Example student accounts*)
let student1 = Student.create_student "Eman" "Abdu" 29062003
let student2 = Student.create_student "Peter" "W" 20022002
let student3 = Student.create_student "Eman" "W" 29062003

(*Example librarian accounts*)

let librarian1 = Librarian.create_lib "Iqra" "Yousuf" 123456
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

let std_username_tests (name : string) (input : Student.student)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Student.get_username input)

let database_book1 =
  Library.create_book "Righteous Mind" philosophy "Jonathan Haidt" 419 ""

let database_book2 =
  Library.create_book "Outliers" philosophy "Jonathan Haidt" 419
    "Why can't our political leaders work together as threats loom and \
     problems mount? Why do people so readily assume the worst about the \
     motives of their fellow citizens? In The Righteous Mind, social \
     psychologist Jonathan Haidt explores the origins of our divisions and \
     points the way forward to mutual understanding."

let db_std2 =
  match students_lst with
  | h :: h2 :: t -> h2
  | _ -> failwith "error"

let database_book3 =
  Library.create_book "The answer is" autobiography "Alex Trebek" 419
    "The book combines illuminating personal anecdotes with Trebek’s thoughts \
     on a range of topics, including marriage, parenthood, education, success, \
     spirituality, and philanthropy. Trebek also addresses the questions he \
     gets asked most often by Jeopardy! fans, such as what prompted him to \
     shave his signature mustache, his insights on legendary players like Ken \
     Jennings and James Holzhauer, and his opinion of Will Ferrell’s Saturday \
     Night Live impersonation. The book uses a novel structure inspired by \
     Jeopardy!, with each chapter title in the form of a question, and \
     features dozens of never-before-seen photos that candidly capture Trebek \
     over the years."

let database_book4 =
  Library.create_book "The shoe dog" memoir "Phil Knight" 386
    "Shoe Dog is a memoir by Nike co-founder Phil Knight. The memoir \
     chronicles the history of Nike from its founding as Blue Ribbon Sports \
     and its early challenges to its evolution into one of the world’s most \
     recognized and profitable companies"

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
    std_username_tests "Username of first student in database"
      (List.hd students_lst) "eman";
  ]

let favorite_sorted_tests (name : string) (input : student)
    (expected_output : book list) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output
       (Library.sort_books (get_favorites input)))

let subset_genre_tests (name : string) (input : book list) (gen : genre)
    (expected_output : book list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Library.subset_genre input gen)

let subset_author_tests (name : string) (input : book list) (auth : string)
    (expected_output : book list) : test =
  name >:: fun _ -> assert_equal expected_output (subset_author input auth)

let sort_all_books_tests (name : string) (input : Database.database)
    (expected_output : book list) : test =
  name >:: fun _ -> assert_equal expected_output (Database.sort_all_books input)

let subset_by_genre_tests (name : string) (input : Database.database)
    (gen : Library.genre) (expected_output : book list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Database.subset_by_genre input gen)

let subset_by_author_tests (name : string) (input : Database.database)
    (auth : string) (expected_output : book list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Database.subset_by_author input auth)

let library2_tests =
  [
    favorite_sorted_tests "Favorite books of second student in database sorted"
      db_std2
      [ database_book3; database_book4 ];
    subset_genre_tests "List of autobiographies in three element book list"
      [ database_book2; database_book4; database_book3 ]
      autobiography [ database_book3 ];
    subset_genre_tests "List of memoirs in list with no memoirs"
      [ database_book1; database_book2; database_book3 ]
      memoir [];
    subset_author_tests "List of books written by Alex Trebek"
      [ database_book2; database_book4; database_book3 ]
      "Alex Trebek" [ database_book3 ];
    subset_author_tests "List of books written by Charles Darwin"
      [ database_book2; database_book4; database_book3 ]
      "Charles Darwin" [];
    sort_all_books_tests "List of books sorted in a database with one book" db2
      [ book1 ];
    sort_all_books_tests "List of books sorted in database with 5 unique books"
      db_test
      [ book4; book5; book1; book3; book2 ];
    subset_by_genre_tests "List of books with genre fiction in database" db_test
      fiction [ book1 ];
    subset_by_author_tests "List of books written by a4 in database" db_test
      "a4" [ book4; book5 ];
  ]

  let parse_commands_test (name:string)(input:string)(expected_output:string):test=
  name >:: fun _ -> assert_equal expected_output (Command.parse_commands input)~printer:Fun.id

  let command_tests=[
    parse_commands_test "Test empty string" "        " "";
    parse_commands_test "Test string with one word all lowercase" "    quit " "quit";
    parse_commands_test "Test string with one word mixed case " "   QuIt " "quit";
    parse_commands_test "Test string with two words mixed case "  "    Borrowed  Books " "borrowed books";
    parse_commands_test "Test string with three words mixed case "  "  GeT  Borrowed  Books  " "get borrowed books";


  ]


let suite =
  "test suite for final project"
  >::: List.flatten
         [
command_tests;
           librarian_tests;
           student_tests;
           library_tests;
           database_tests;
           library2_tests;
         ]

let _ = run_test_tt_main suite