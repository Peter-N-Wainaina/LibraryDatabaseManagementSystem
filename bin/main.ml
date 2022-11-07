open Dbms
open Database
open Student
open Command
open Demo
open Yojson.Basic.Util

exception UnknownInput

let data_dir_prefix = "data" ^ Filename.dir_sep

let database =
  Database.to_database
    (Yojson.Basic.from_file (data_dir_prefix ^ "database.json"))

let students_lst = Database.view_student_accounts database
let librarians_lst = Database.view_librarian_accounts database

(*Really needs to be fixed. Same with all the ones above. We don't need to store
  students in main.ml Also is there a way where we can print out written stuff
  from another file?*)
let show_options () = print_endline "\n\t\tBorrowed Books\n\t\tFavorite Books\n"

(* I don't want anything above this to be in main.ml*)

(** [printer lst] takes in a string list and prints out its contents. *)
let rec printer lst =
  match lst with
  | [] -> print_endline "\n"
  | h :: t ->
      ANSITerminal.(print_string [ blue ] ("\n\t" ^ h));
      printer t;
      ()

(** [printer_bw] takes in a string list and returns unit. It prints out the
    borrowed books of a user.*)
let printer_bw lst =
  if lst = [] then print_endline "\tYou currently have no borrowed books.\n"
  else printer lst

(** [printer_fav] takes in a string list and returns unit. It prints out the
    favorite books of a user.*)
let printer_fav lst =
  if lst = [] then
    print_endline "\tYou currently don't have books in the favorite section.\n"
  else printer lst

(** [parse_options t] takes in a string and returns unit. It allows the user
    with username t to browse through the database by typing one of the command
    options.*)
let rec parse_options (t : string) =
  print_string "\t> ";
  match options (read_line ()) with
  | Quit ->
      print_endline "Goodbye!\n";
      exit 0
  | Options ->
      show_options ();
      parse_options t
  | Help ->
      print_endline
        "\n\
         \tBorrowed books - see the name of the books you have borrowed from \
         Cornell Libraries\n\
         \tFavorite books - see the name of the books you have marked as \
         favorite before.\n";
      parse_options t
  | Borrowed_books ->
      t |> find_student students_lst |> borrowed_books |> printer_bw;
      (* Need a function that does [ t |> find_student students_lst |>
         borrowed_books]. But where should we put it? database.ml?*)
      parse_options t
  | Favorite_books ->
      t |> find_student students_lst |> favorite_books |> printer_fav;
      parse_options t
  | _ ->
      print_endline "\tPlease type a valid command.\n";
      parse_options t

(**[browse t] takes in string and returns unit. It allows user with username t
   browse through the database. It prints out instructions and wait for user inp*)
let browse t =
  ANSITerminal.(
    print_string [ magenta ]
      "\n\
       \tYou can now start browsing through the Cornell University Database.\n";
    print_endline
      "\n\
       \t\tType 'Options' to see the options \n\
       \tTo learn about the command options, please type 'HELP'\n\
       \tTo log out, type 'Log out' or 'Quit'\n");
  parse_options t

(** [verify_password un pw id] takes in a string un as the username, string pw
    as the password and int id as the studentID and creates a student account.
    It asks the user to re-enter their password to verify it is correct.*)
let rec verify_password un pw id =
  print_endline "\tRe-enter your Password:";
  let x = read_line () in
  match x = pw with
  | true ->
      ANSITerminal.(
        print_string [ green ] "\n\tYou have successfully created an account!\n");
      ();
      browse un
  | false -> raise UnknownInput

(** [add_password y t] takes in a string y and an integer t and returns unit. It
    takes y as the username of the new account, t as the studentID and the
    user's input as the password and allow them to proceed to verifying
    password. *)
let rec add_password (y : string) (t : int) =
  print_endline "\tChoose a Password:";
  let z = read_line () in
  match verify_password y z t with
  | "Quit" ->
      print_endline "Goodbye!\n";
      exit 0
  | exception UnknownInput ->
      ANSITerminal.(
        print_string [ red ]
          "\tThe password does not match. Please enter a new password.\n");
      add_password y t
  | _ -> ()

(** [find_password t] takes in a string and returns unit. If the user's input is
    the password for the account with username t, then logging in succeeds.
    Else, it prompts the user to re-enter their password.*)
let rec find_password t =
  match read_line () = find_pw students_lst t with
  | true ->
      ANSITerminal.(print_string [ green ] "\t\tWelcome");
      ignore (browse t)
  | _ ->
      ANSITerminal.(print_string [ red ] "\tIncorrect pasword. Try again!");
      print_endline "\n\tPassword:";
      find_password t

(** [find_user ()] takes in unit and returns unit. It verifies that there is no
    user in the database with username of the user's input*)
let rec adduser (t : int) =
  let a = read_line () in
  match find_a_user a with
  | None -> add_password a t
  | _ ->
      print_endline "\n\t\t";
      ANSITerminal.(
        print_string [ Background Red ] "A user with this username exists.";
        print_string [] "\n\n\tPlease choose a different username:");
      adduser t

(** [find_user ()] takes in unit and returns unit. It verifiess that there is a
    user in the database with username of the user's input.*)
let rec find_user () =
  let a = read_line () in
  match find_a_user a with
  | None ->
      print_endline
        "This username does not exist.Please enter a correct username.\n\
         Username:";
      find_user ()
  | _ -> find_password a

(** [take_username ()] takes in unit and returns unit. It asks for a new user's
    student ID when they log in for the first time.*)
let rec take_username () =
  print_endline "\tType your id:";
  let x = read_line () in
  match user_type x with
  | Quit ->
      print_endline "Goodbye!\n";
      exit 0
  | _ -> (
      match int_of_string x with
      | exception Failure t ->
          ANSITerminal.(
            print_string [ red ] "\n\tPlease type a valid Student ID");
          take_username ()
      | exception End_of_file ->
          print_endline "Goodbye!\n";
          exit 0
      | _ ->
          print_endline "\n\tChoose a Username:";
          adduser (int_of_string x))

(** [read_new ()] takes in unit and returns unit. It identifies a user as a
    student or a librarian and direct them into different sign up page. *)
let rec read_new () =
  ANSITerminal.(
    print_string [ green ] "\n\t\tAre you a Student or a Librarian?\n\n");
  print_string "\t> ";
  match user_type (read_line ()) with
  | Quit ->
      print_endline "Goodbye!\n";
      exit 0
  | Student -> take_username ()
  | Librarian -> take_username ()
  | _ ->
      ANSITerminal.(print_string [ red ] "\t\tPlease input a valid command\n");
      read_new ()

(** [read_old ()] takes in unit and returns unit. It identifies a user as a
    student or a librarian and direct them into different log in page. *)
let rec read_old () =
  ANSITerminal.(
    print_string [ green ] "\n\t\tAre you a Student or a Librarian?\n\n");
  print_string "\t> ";
  match user_type (read_line ()) with
  | Quit ->
      print_endline "Goodbye!\n";
      exit 0
  | Student ->
      print_endline "\n\tUsername:";
      find_user ()
  | Librarian ->
      print_endline "\n\tUsername:";
      find_user ()
  | _ ->
      ANSITerminal.(print_string [ red ] "\t\tPlease input a valid command\n");
      read_old ()

(** [identify_user ()] takes in unit and returns unit. It parses a user input
    (whether they are signing up or logging in) and directs them to the right
    location.*)
let rec identify_user () =
  match logging (read_line ()) with
  | exception End_of_file ->
      print_endline "Goodbye!\n";
      exit 0
  | Quit -> print_endline "Goodbye!\n"
  | Login ->
      ANSITerminal.(print_string [ green ] "\t\t\tWelcome back!\n");
      read_old ()
  | Sign_up ->
      ANSITerminal.(print_string [ green ] "\t\t\tWelcome new user!\n");
      read_new ()
  | _ ->
      ANSITerminal.(print_string [ red ] "please input a valid command\n\t>");
      identify_user ()

(** [read login ()] takes in unit and returns unit. It prints out the
    instruction on how to log in or sign up to the system.*)
let rec read_login () =
  ANSITerminal.(
    print_string [] "\n\tIf you have an account, type: ";
    print_string [ cyan ] "'Log in'\n\n";
    print_string [] "\tIf you are a new user, type: ";
    print_string [ cyan ] "'Sign up'\n\n";
    print_string [] "\tTo exit, type 'QUIT' or press 'Ctrl' + 'D'.\n\t> ");
  identify_user ();
  ()

(* [main ()] prompts the user to log in to their account or create an account if
   its their first time. And then calls read_input to check if their input is
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