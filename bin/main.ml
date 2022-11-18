open Dbms
open Database
open Student
open Command
open Execute
open Yojson.Basic.Util

exception UnknownInput

let data_dir_prefix = "data" ^ Filename.dir_sep

let database =
  Database.to_database
    (Yojson.Basic.from_file (data_dir_prefix ^ "database.json"))

let exit_db () =
  print_endline "Goodbye!";
  exit 0

let show_options () = print_endline "\n\t\tBorrowed Books\n\t\tFavorite Books\n"

(** [printer lst] takes in a string list and prints out its contents. *)
let rec printer lst =
  match lst with
  | [] -> print_endline "\n"
  | h :: t ->
      ANSITerminal.(print_string [ blue ] ("\n\t" ^ h));
      printer t;
      ()

(** [print_books lst s] prints out the books in [lst] in section [s].*)
let print_books s lst =
  if lst = [] then
    print_endline ("\tYou currently don't have books in " ^ s ^ " section.\n")
  else printer lst

(**[print_book_details_helper] prints out title [t] and description [d] in the
   format [t] :[d]*)
let print_book_details_helper t d =
  ANSITerminal.(print_string [ blue ] ("\n\t" ^ t ^ " : " ^ d))

(**[print_book_details] prints all [books] in [l]*)
let print_book_details (l : Library.book list) =
  match l with
  | [] ->
      ANSITerminal.(
        print_string [ red ]
          ("\n\t" ^ " There are currently no books of this genre"))
  | h :: t ->
      let map_helper x =
        print_book_details_helper "Name" (Library.book_name x);
        print_book_details_helper "Author" (Library.book_author x);
        print_book_details_helper "Description" (Library.book_description x);
        print_book_details_helper "Number of pages"
          (Library.book_length x |> string_of_int)
      in
      List.map (fun x -> map_helper x) t |> ignore;
      ()

(** [student_browse s] allows student [s] to browse through the database.*)
let student_browse s =
  ANSITerminal.(
    print_string [ magenta ]
      "\n\
       \tYou can now start browsing through the Cornell University Database.\n");
  let rec rec_student_browse s =
    print_endline
      "\n\
       \t\tType 'Options' to see the options \n\
       \tTo learn about the command options, please type 'HELP'\n\
       \tTo log out, type 'Log out' or 'Quit'\n";
    print_string "\t> ";
    match Command.options (read_line ()) with
    | Quit -> exit_db ()
    | Options ->
        Command.student_options () |> printer;
        rec_student_browse s
    | Help ->
        Command.student_help () |> printer;
        rec_student_browse s
    | Favorite_books ->
        s |> Student.favorite_books |> print_books "favorite";
        rec_student_browse s
    | Borrowed_books ->
        s |> Student.borrowed_books |> print_books "borrowed";
        rec_student_browse s
    | Genre g ->
        Database.subset_by_genre database g |> print_book_details;
        rec_student_browse s
    | _ ->
        print_endline "Please type a valid command";
        rec_student_browse s
  in
  rec_student_browse s

(** [student_login ()] prompts the student for username and password and allows
    them to browse if the login details are valid.*)
let rec student_login () =
  print_endline "\n\tUsername:";
  let username = read_line () in
  print_endline "\n\tPassword:";
  let password = read_line () in
  match Execute.get_student username password database with
  | exception UserNameNotFound ->
      print_endline "Incorrect Username";
      student_login ()
  | exception IncorrectPassword ->
      print_endline "Incorrect Password";
      student_login ()
  | student -> student_browse student

(** [librarian_browse l] allows librarian [l] to browse through the database.*)
let librarian_browse l =
  ANSITerminal.(
    print_string [ magenta ]
      "\n\
       \tYou can now start browsing through the Cornell University Database.\n");
  let rec rec_librarian_browse l =
    print_endline
      "\n\
       \t\tType 'Options' to see the options \n\
       \tTo learn about the command options, please type 'HELP'\n\
       \tTo log out, type 'Log out' or 'Quit'\n";
    print_string "\t> ";
    match Command.options (read_line ()) with
    | Quit -> exit_db ()
    | Options ->
        Command.librarian_options () |> printer;
        rec_librarian_browse l
    | Help ->
        Command.librarian_help () |> printer;
        rec_librarian_browse l;
        rec_librarian_browse l
    | _ ->
        print_endline "Please type a valid command";
        rec_librarian_browse l
  in
  rec_librarian_browse l

(** [librarians_login ()] prompts the librarian for username and password and
    allows them to browse if the login details are valid.*)
let rec librarian_login () =
  print_endline "\n\tUsername:";
  let username = read_line () in
  print_endline "\n\tPassword:";
  let password = read_line () in
  match Execute.get_librarian username password database with
  | exception UserNameNotFound ->
      print_endline "Incorrect Username";
      librarian_login ()
  | exception IncorrectPassword ->
      print_endline "Incorrect Password";
      librarian_login ()
  | librarian -> librarian_browse librarian

(** [identify_user ()] parses a user type.*)
let rec identify_user () =
  match Command.user_type (read_line ()) with
  | exception End_of_file -> exit_db ()
  | Quit -> exit_db ()
  | Student -> student_login ()
  | Librarian -> librarian_login ()
  | _ ->
      ANSITerminal.(print_string [ red ] "Please input a valid command\n\t>");
      identify_user ();
      ()

(** [read login ()] prints out the instruction on how to log in to the system.*)
let rec read_login () =
  ANSITerminal.(
    print_string [ cyan ] "'Are you a Student or a Librarian'\n\n";
    print_string [] "\tTo exit, type 'QUIT' or press 'Ctrl' + 'D'.\n\t> ");
  identify_user ();
  ()

(* [main ()] prompts the user to log in to their account or create an account if
   its their first time. And then calls read_login to check if their input is
   valid*)
let main () =
  ANSITerminal.resize 150 100;
  ANSITerminal.(
    print_string [ green ]
      "\n\
       \t\tWelcome to Cornell Library! \n\
       \t\tType 'HELP' for help with commands\n");
  read_login ()

(* Execute the dbms. *)
let () = main ()