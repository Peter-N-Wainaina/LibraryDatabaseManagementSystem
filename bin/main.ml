open Dbms
open Database
open Student
open Demo
open Yojson.Basic.Util

exception UnknownInput

let from_json json = json |> member "students" |> to_list |> List.map to_student
let data_dir_prefix = "data" ^ Filename.dir_sep

let student_accounts =
  Yojson.Basic.from_file (data_dir_prefix ^ "student_accounts.json")

let students_lst = student_accounts |> from_json
let username_lst = students_lst |> List.map get_username
let student_db = create_database "student_db"

let show_options () =
  ANSITerminal.(print_string [] "\n\t\tBorrowed Books\n\t\tFavorite Books\n")

let rec printer lst =
  match lst with
  | [] ->
      ANSITerminal.(print_string [] "\n");
      ()
  | h :: t ->
      ANSITerminal.(print_string [ blue ] ("\n\t" ^ h));
      printer t;
      ()

let printer_bw lst =
  if lst = [] then
    ANSITerminal.(print_string [] "\tYou currently have no borrowed books.\n")
  else printer lst

let printer_fav lst =
  if lst = [] then
    ANSITerminal.(print_string)
      [] "\tYou currently don't have books in the favorite section.\n"
  else printer lst

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
      ANSITerminal.(
        print_string []
          "\n\
           \tBorrowed books - see the name of the books you have borrowed from \
           Cornell Libraries\n\
           \tFavorite books - see the name of the books you have marked as \
           favorite before.\n");
      parse_options t
  | Borrowed_books ->
      t |> find_student students_lst |> borrowed_books |> printer_bw;
      parse_options t
  | Favorite_books ->
      t |> find_student students_lst |> favorite_books |> printer_fav;
      parse_options t
  | _ ->
      ANSITerminal.(print_string [] "\tPlease type a valid command.\n");
      parse_options t

let browse t =
  ANSITerminal.(
    print_string [ magenta ]
      "\n\
       \tYou can now start browsing through the Cornell University Database.\n";
    ANSITerminal.(
      print_string []
        "\n\
         \t\tType 'Options' to see the options \n\
         \tTo learn about the command options, please type 'HELP'\n"));
  parse_options t

let add_student un pw id =
  let v = create_student un pw id in
  v |> add_student_account student_db

let rec verify_password un pw id =
  ANSITerminal.(print_string [] "\tRe-enter your Password:");
  if read_line () = pw then (
    ANSITerminal.(
      print_string [ green ] "\n\tYou have successfully created an account!\n");
    ignore (student_db = add_student un pw id);
    browse un)
  else raise UnknownInput

let rec add_password (y : string) (t : int) =
  ANSITerminal.(print_string [] "\tChoose a Password:");
  let z = read_line () in
  match verify_password y z t with
  | exception UnknownInput ->
      ANSITerminal.(
        print_string [ red ]
          "\tThe password does not match. Please enter a new password.\n");
      add_password y t
  | _ -> ()

let rec find_password t =
  match read_line () = find_pw students_lst t with
  | true ->
      ANSITerminal.(print_string [ green ] "\t\tWelcome");
      ignore (browse t)
  | _ ->
      ANSITerminal.(print_string [ red ] "\tIncorrect pasword. Try again!");
      ANSITerminal.(print_string [] "\n\tPassword:");
      find_password t

let rec adduser (t : int) =
  let a = read_line () in
  let y = username_lst in
  if List.find_opt (fun x -> a = x) y = None then add_password a t
  else
    ANSITerminal.(
      print_string [] "\n\t\t";
      ANSITerminal.(
        print_string [ Background Red ] "A user with this username exists.";
        print_string [] "\n\n\tPlease choose a different username:");
      adduser t)

let rec find_user () =
  let a = read_line () in
  let y = username_lst in
  (if List.find_opt (fun x -> a = x) y = None then (
   ANSITerminal.(
     print_string []
       "This username does not exist.Please enter a correct username.\n\
        Username:");
   find_user ())
  else ANSITerminal.(print_string [] "\tPassword:"));
  find_password a

let rec take_username () =
  ANSITerminal.(print_string [] "\tType your id:");
  let x = read_line () in
  match int_of_string x with
  | exception Failure t ->
      ANSITerminal.(print_string [ red ] "\n\tPlease type a valid Student ID");
      take_username ()
  | exception End_of_file ->
      print_endline "Goodbye!\n";
      exit 0
  | _ ->
      ANSITerminal.(print_string [] "\n\tChoose a Username:");
      adduser (int_of_string x)

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

let rec read_old () =
  ANSITerminal.(
    print_string [ green ] "\n\t\tAre you a Student or a Librarian?\n\n");
  print_string "\t> ";
  match user_type (read_line ()) with
  | Quit ->
      print_endline "Goodbye!\n";
      exit 0
  | Student ->
      ANSITerminal.(print_string [] "\n\tUsername:");
      find_user ()
  | Librarian ->
      ANSITerminal.(print_string [] "\n\tUsername:");
      find_user ()
  | _ ->
      ANSITerminal.(print_string [ red ] "\t\tPlease input a valid command\n");
      read_old ()

let identify_user () =
  match logging (read_line ()) with
  | Quit ->
      print_endline "Goodbye!\n";
      exit 0
  | Login ->
      ANSITerminal.(print_string [ green ] "\t\t\tWelcome back!\n");
      read_old ()
  | Sign_up ->
      ANSITerminal.(print_string [ green ] "\t\t\tWelcome new user!\n");
      read_new ()
  | _ -> raise UnknownInput

let rec read_login () =
  ANSITerminal.(
    print_string [] "\n\tIf you have an account, type: ";
    print_string [ cyan ] "'Log in'\n\n");
  ANSITerminal.(
    print_string [] "\tIf you are a new user, type: ";
    print_string [ cyan ] "'Sign up'\n\n");
  print_endline "\tTo exit, type 'QUIT' or press 'Ctrl' + 'D'.\n";
  print_string "\t> ";
  let x = identify_user () in
  match x with
  | exception UnknownInput ->
      ANSITerminal.(print_string [ red ] "please input a valid command\n");
      read_login ()
  | _ -> x

(* [main ()] prompts the user to log in to their account or create an account if
   its their first time. And then calls read_input to check if their input is
   valid*)
let main () =
  ANSITerminal.(
    print_string [ green ]
      "\n\
      \       \t\t   Welcome to Cornell Library! \n\
       \t\tType 'HELP' for help with commands\n");

  read_login ()

(* Execute the dbms. *)
let () = main ()