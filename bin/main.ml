open Printf

let rec adduser () = 
  let a = read_line () in
  let y = [] in 
  if (List.find_opt (fun x -> a= x) y = None )then 
  (ANSITerminal.(print_string [] ("Choose a Password:")); ()) else 
    (ANSITerminal.(print_string [] ("A user with this username exists.\n
    Please choose a different username:")); adduser ())

let rec find_user () = 
  let a = read_line () in 
  let y = [] in 
  if (List.find_opt (fun x -> a= x) y = None )then 
    (ANSITerminal.(print_string [] ("This username does not exist. )
    Please enter a correct username.\nUsername:"));find_user ()) else 
      (ANSITerminal.(print_string [] ("Enter password:")); ())

let rec read_new () = 
  ANSITerminal.(print_string [green] (
    "Are you a Student or a Librarian?\n")); 
  print_string "> ";
  match read_line () with
  | "Student" -> ANSITerminal.(print_string [] ("Choose a Username:")); adduser ()
  | "student" -> ANSITerminal.(print_string [] ("Choose a Username:")); adduser()
  | "Librarian" -> ANSITerminal.(print_string [] ("Choose a Username:")); adduser()
  | "librarian" -> ANSITerminal.(print_string [] ("Choose a Username:")); adduser()
  | _ -> ANSITerminal.(print_string [red] (
    "please input a valid command\n")); read_new ()

let rec read_old () = 
      ANSITerminal.(print_string [green] (
        "Are you a Student or a Librarian?\n")); 
      print_string "> ";
      match read_line () with
      | "Student" -> ANSITerminal.(print_string [] ("Username:")); find_user ()
      | "student" -> ANSITerminal.(print_string [] ("Username:")); find_user ()
      | "Librarian" -> ANSITerminal.(print_string [] ("Username:")); find_user ()
      | "librarian" -> ANSITerminal.(print_string [] ("Username:")); find_user ()
      | _ -> ANSITerminal.(print_string [red] (
        "please input a valid command\n")); read_old ()

let rec read_login () =
  print_endline "If you have an account, type 'Log in'."; 
  print_endline "If you are a new user, type 'Sign up'.";
  print_endline "To exit, type 'QUIT'.";
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | "" -> read_login ()
  | "QUIT" -> print_endline "Goodbye!\n"; exit 0
  | "Log in" -> ANSITerminal.(print_string [green]("Welcome back!\n"));read_old ()
  | "Log In" -> ANSITerminal.(print_string [green]("Welcome back!\n"));read_old ()
  | "Sign up" -> ANSITerminal.(print_string [green]("Welcome new user!\n"));read_new ()
  | "Sign Up" -> ANSITerminal.(print_string [green]("Welcome new user!\n"));read_new ()
  | _ -> ANSITerminal.(print_string [red] (
    "please input a valid command\n")); read_login ()

(* [main ()] prompts the user to log in to their account or create an account 
   if its their first time. And then calls read_input to check if their 
   input is valid*)
    let main () = 
      ANSITerminal.(print_string [green] "
       Welcome to Cornell Library! \nType 'HELP' for help with commands\n");
      
      read_login ()
    

(* Execute the dbms. *)
let () = main ()