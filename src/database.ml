type database = {
  name:string;
  libraries: Library.library list;
  librarians: Librarian.lib list;
  students: Student.student list
}

let create_database n = {
  name=n;
  libraries=[];
  librarians=[];
  students=[]
}
let add_library d l = 
  let new_libraries =l:: d.libraries in 
  {d with libraries = List.sort_uniq Stdlib.compare new_libraries}

let add_student_account d l = raise  (Failure "Unimplemented:add_student_account")
let add_librarian_account  d l= raise  (Failure "Unimplemented:add_librarian_account") 

let view_libraries d = d.libraries

let view_student_accounts d = d.students 

let view_librarian_accounts d  = d.librarians

