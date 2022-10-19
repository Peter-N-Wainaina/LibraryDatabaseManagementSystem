type database = {
  name:string;
  libraries: Library.library list;
  librarians: Librarian.lib list;
  students: Student.student list
}

let create_database = raise  (Failure "Unimplemented:create_database")
let add_library  = raise  (Failure "Unimplemented:add_library")
let add_student_account = raise  (Failure "Unimplemented:add_student_account")
let add_librarian_account  = raise  (Failure "Unimplemented:add_librarian_account") 

let view_libraries = raise  (Failure "Unimplemented:view_libraries") 

let view_student_accounts = raise  (Failure "Unimplemented:view_student_accounts") 

let view_librarian_accounts  = raise  (Failure "Unimplemented:view_librarian_accounts") 

