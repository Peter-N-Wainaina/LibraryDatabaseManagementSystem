open Printf
open Dbms
open Database
open Student

open Yojson.Basic.Util
exception UnknownInput


let from_json json = 
  json |> member "students" |> to_list |> List.map to_student_list

let data_dir_prefix = "data" ^ Filename.dir_sep
let student_accounts = Yojson.Basic.from_file 
(data_dir_prefix ^ "student_accounts.json")

let students_lst = student_accounts |> from_json
let username_lst = students_lst |> List.map get_username
let id_lst = students_lst |> List.map get_id


let find_student id h = 
  let x = h |> List.find_opt (fun y ->  (y |> get_id) = id) in 
  match x  with 
  | None -> raise UnknownStudent
  | Some k -> k
  let browse () = 
    ANSITerminal. (print_string [magenta] ("\n\tYou can now start browsing \
    through the Cornell University Database.\n");
    ANSITerminal. (print_string [magenta] ("\n\t\tTo learn about the command options, \
    please type 'HELP'\n")));()
  let rec verify_password a = 
    (ANSITerminal.(print_string [] ("\tRe-enter your Password:"))); 
    if (read_line () = a )
      then (ANSITerminal.(print_string [green] 
      ("\n\tYou have successfully created an account!\n"));browse ()) 
    else 
        (ANSITerminal.(print_string [] ("The password does not match. \
    Please enter a new password."));())

  let add_password ()= (ANSITerminal.(print_string []
  ("\tChoose a Password:")); verify_password (read_line ())) 
  let rec adduser () = 
    let a = read_line () in
    match a with 
    | exception End_of_file -> print_endline "Goodbye!\n"; exit 0
    | _ -> 
      begin
    let y = (username_lst )  in 
    if (List.find_opt (fun x -> a= x) (y )= None )then 
    (ANSITerminal.(print_string [] ("\tChoose a Password:")); 
    verify_password (read_line ())) else 
      (ANSITerminal.(print_string [] ("\n\t\t");
      (ANSITerminal.(print_string [Background (Red)] 
      ("A user with this username exists.");
      print_string [] ("\n\n\tPlease choose a different username:")); adduser ())))
      end

let rec find_user () = 
  let a = read_line () in 
  let y = (username_lst ) in 
  if (List.find_opt (fun x -> a= x) y = None )then 
    (ANSITerminal.(print_string [] ("This username does not exist.\
  Please enter a correct username.\nUsername:"));find_user ()) else 
      (ANSITerminal.(print_string [] ("\tPassword:")); ())

let rec read_new () = 
  ANSITerminal.(print_string [green] (
    "\n\t\tAre you a Student or a Librarian?\n\n")); 
  print_string "\t> ";
  match read_line () with
  | exception End_of_file -> print_endline "Goodbye!\n"; exit 0
  | "Student" -> ANSITerminal.(print_string [] 
    ("\n\tChoose a Username:")); adduser ()
  | "student" -> ANSITerminal.(print_string [] 
    ("\n\tChoose a Username:")); adduser()
  | "Librarian" -> ANSITerminal.(print_string [] 
    ("\n\tChoose a Username:")); adduser()
  | "librarian" -> ANSITerminal.(print_string [] 
    ("\n\tChoose a Username:")); adduser()
  | _ -> ANSITerminal.(print_string [red] (
    "\t\tPlease input a valid command\n")); read_new ()

let rec read_old () = 
      ANSITerminal.(print_string [green] (
        "\n\t\tAre you a Student or a Librarian?\n\n")); 
      print_string "\t> ";
      match read_line () with
      | exception End_of_file -> print_endline "Goodbye!\n"; exit 0
      | "Student" -> ANSITerminal.(print_string [] 
        ("\n\tUsername:")); find_user ()
      | "student" -> ANSITerminal.(print_string [] 
        ("\n\tUsername:")); find_user ()
      | "Librarian" -> ANSITerminal.(print_string [] 
        ("\n\tUsername:")); find_user ()
      | "librarian" -> ANSITerminal.(print_string [] 
        ("\n\tUsername:")); find_user ()
      | _ -> ANSITerminal.(print_string [red] (
        "\t\tPlease input a valid command\n")); read_old ()

let identify_user () = 
  match read_line () with 
  | exception End_of_file -> print_endline "Goodbye!\n"; exit 0
  | "QUIT" -> print_endline "Goodbye!\n"; exit 0
  | "Log in" -> ANSITerminal.(print_string [green]
    ("\t\t\tWelcome back!\n"));read_old ()
  | "Log In" -> ANSITerminal.(print_string [green]
    ("\t\t\tWelcome back!\n"));read_old ()
  | "Sign up" -> ANSITerminal.(print_string [green]
    ("\t\t\tWelcome new user!\n"));read_new ()
  | "Sign Up" -> ANSITerminal.(print_string [green]
    ("\t\t\tWelcome new user!\n"));read_new ()
  | _ -> raise (UnknownInput)

let rec read_login () =
  ANSITerminal.(print_string []("\n\tIf you have an account, type: ");  
  print_string [cyan]("'Log in'\n\n")); 
  ANSITerminal.(print_string []("\tIf you are a new user, type: ");
  print_string [cyan]("'Sign up'\n\n")); 
  print_endline "\tTo exit, type 'QUIT' or press 'Ctrl' + 'D'.\n";
  print_string  "\t> "; let x = identify_user() in 
  match x with 
  | exception UnknownInput -> ANSITerminal.(print_string [red] (
    "please input a valid command\n")); read_login ()
  | _ -> x


(* [main ()] prompts the user to log in to their account or create an account 
   if its their first time. And then calls read_input to check if their 
   input is valid*)
    let main () = 
      ANSITerminal.(print_string [green] "
       \t\t   Welcome to Cornell Library! \
       \n\t\tType 'HELP' for help with commands\n");
      
      read_login ()
    
(* Execute the dbms. *)
let () = main ()