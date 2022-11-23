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
  ANSITerminal.(print_string [ magenta;Bold ] "Goodbye!\n");
  exit 0

let print_invalid_command ()= 
  ANSITerminal.(
    print_string [ red ] "\n\tPlease input a valid command\n")

let get_input ()= ANSITerminal.(print_string [Bold;Blink]"\n\t> ") 

(** [printer lst] takes in a string list and prints out its contents. *)
let rec printer lst =
  match lst with
  | [] -> print_endline "\n"
  | h :: t ->
      ANSITerminal.(print_string [ cyan ] ("\n\t" ^ h));
      printer t;
      ()

(** [print_books lst s] prints out the books in [lst] in section [s].*)
let print_books s lst =
  if lst = [] then
    print_endline ("\tYou currently don't have books in " ^ s ^ " section.\n")
  else printer lst
(**[print_book_details] prints all [books] in [l]*)
let print_book_details (l : Library.book list) =
  let print_book_details_helper t d =
    ANSITerminal.(print_string [ yellow ] ("\n\t" ^ t ^ " : " ^ d))
  in
  let rec print_all_books = function
    | [] -> print_endline ""
    | h :: t ->
        print_book_details_helper "Name" (Library.book_name h);
        print_book_details_helper "Author" (Library.book_author h);
        print_book_details_helper "Description" (h|>Library.book_description);
        print_book_details_helper "Number of pages"
          (Library.book_length h |> string_of_int);
        print_endline "\n";
        print_all_books t
  in
  match l with
  | [] ->
      ANSITerminal.(
        print_string [ red ]
          ("\n\t" ^ " There are currently no books of this genre\n"))
  | h :: t as all -> print_all_books all

let parse_author_command n =
  match Execute.get_author_books database n with
  | exception UnknownAuthor n ->
      ANSITerminal.(print_string [ red ] ("\n\t There are no books from " ^ n^"\n"))
  | exception MultipleAuthors authors ->
      ANSITerminal.(
        print_string [ red ]
          ("\n\t There are multiple authors named " ^ n
         ^ " please select one from the list\n"));
      printer authors
  | l ->
      ANSITerminal.(
        print_string [ green ] ("\n\t Here are the books by " ^ fst l^"\n"));
      print_book_details (snd l)

(** [student_browse s] allows student [s] to browse through the database.*)
let rec student_browse s =
  try
    ANSITerminal.(
      print_string [ green;Bold] 
        "\n\
         \tYou can now start browsing through the Cornell University Database.\n");
         ANSITerminal.(print_string [magenta;Bold]  "\n\
         \tType 'Options' to see the options \n\
         \tTo learn about the command options, please type 'HELP'\n\
         \tTo log out, type 'Log out' or 'Quit'\n")
          ;
    let rec rec_student_browse s =
      get_input () ;    
      match Command.options (read_line ()) with
      | Quit -> exit_db ()
      | Log_out ->
        ANSITerminal.(print_string [ magenta;Bold ] "Goodbye!\n");
        read_login ()
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
      | Author n ->
          parse_author_command n;
          rec_student_browse s
      | _ ->
          print_invalid_command ();
          rec_student_browse s
    in
    rec_student_browse s
  with end_of_file -> exit_db ()

(** [student_login ()] prompts the student for username and password and allows
    them to browse if the login details are valid.*)
and student_login () =
    ANSITerminal.(print_string [cyan] "\n\tUsername:");
  try
    let username = read_line () in
    match options username with
    | Back -> read_login ()
    | _ -> (
      ANSITerminal.(print_string [cyan] "\n\tPassword:");
      let password = read_line () in
        match options password with
        | Back -> read_login ()
        | _ -> (
            match Execute.get_student username password database with
            | exception UserNameNotFound ->
                ANSITerminal.(print_string [ red ] "\n\tIncorrect Username\n");
                student_login ()
            | exception IncorrectPassword ->
                ANSITerminal.(print_string [ red ] "\n\tIncorrect Password\n");
                student_login ()
            | student -> student_browse student))
  with end_of_file -> exit_db ()

(** [librarian_browse l] allows librarian [l] to browse through the database.*)
and librarian_browse l =
  try
    ANSITerminal.(
      print_string [ green ]
        "\n\
         \tYou can now start browsing through the Cornell University Database.\n");
    let rec rec_librarian_browse l =
      ANSITerminal.(print_string [green]  "\n\
      \tType 'Options' to see the options \n\
      \tTo learn about the command options, please type 'HELP'\n\
      \tTo log out, type 'Log out' or 'Quit'\n")
       ;
       get_input () ;
      match Command.options (read_line ()) with
      | Quit -> exit_db ()
      | Log_out ->
          print_endline "Goodbye!";
          read_login ()
      | Options ->
          Command.librarian_options () |> printer;
          rec_librarian_browse l
      | Help ->
          Command.librarian_help () |> printer;
          rec_librarian_browse l;
          rec_librarian_browse l
      | _ ->
          print_invalid_command ();
          rec_librarian_browse l
    in
    rec_librarian_browse l
  with end_of_file -> exit_db ()

(** [librarians_login ()] prompts the librarian for username and password and
    allows them to browse if the login details are valid.*)
and librarian_login () =
    ANSITerminal.(print_string [cyan] "\n\tUsername:");
  try
    let username = read_line () in
    match options username with
    | Back -> read_login ()
    | Quit -> exit_db ()
    | _ -> (
        print_endline "\n\tPassword:";
        let password = read_line () in
        match options password with
        | Quit -> exit_db ()
        | Back -> read_login ()
        | _ -> (
            match Execute.get_librarian username password database with
            | exception UserNameNotFound ->
                print_endline "\n\tIncorrect Username\n";
                librarian_login ()
            | exception IncorrectPassword ->
                print_endline "\n\tIncorrect Password\n";
                librarian_login ()
            | librarian -> librarian_browse librarian))
  with end_of_file -> exit_db ()

(** [identify_user ()] parses a user type.*)
and identify_user () =
get_input ();
  match Command.user_type (read_line ()) with
  | exception End_of_file -> exit_db ()
  | Quit | Back | Log_out -> exit_db ()
  | Student -> student_login ()
  | Librarian -> librarian_login ()
  | _ ->print_invalid_command ();
      identify_user ();
      ()

(** [read login ()] prints out the instruction on how to log in to the system.*)
and read_login () =
  ANSITerminal.(print_string [ cyan ] "\tAre you a Student or a Librarian\n\n");
  identify_user ();
  ()

(* [main ()] prompts the user to log in to their account or create an account if
   its their first time. And then calls read_login to check if their input is
   valid*)
let main () =
  ANSITerminal.resize 150 100;
  ANSITerminal.(print_string [ green;Bold ] "\n\t\tWelcome to Cornell Library! \n\n");
  read_login ()

(* Execute the dbms. *)
let () = main ()