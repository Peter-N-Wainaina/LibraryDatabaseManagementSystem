(** Representation of a database:

    This module represents Library Management System. It contains all available
    libraries, student and librarian accounts and methods to access, and edit
    this information. *)

exception UnknownID of Student.student_id

type database
(**The abstract type representing the database*)

val create_database : string -> database
(**[create_database n] is a database with name [n]*)

val to_database : Yojson.Basic.t -> database
(**[to_database j] is a database made from json [j]*)

val add_library : database -> Library.library -> database
(**[add_student_account d l] is a database [d] with library [l] added to it. *)

val add_student_account : database -> Student.student -> database
(**[add_student_account d s] is a database [d] with student account [s] added to
   it. *)

val add_librarian_account : database -> Librarian.lib -> database
(**[add_librarian_account d l] is a database [d] with librarian account [l]
   added to it. *)

val view_libraries : database -> Library.library list
(**[view_libraries d] is a set-like list of all libraries in [d]*)

val view_student_accounts : database -> Student.student list
(**[view_student_accounts d] is a set-like list of all students in [d]*)

val view_librarian_accounts : database -> Librarian.lib list
(**[view_librarian_accounts d] is a set-like list of all librarians in [d]*)

val student_user_names : database -> string list
(**[student_user_names d] is a list of the usernames of the students in database
   [d]*)

val students_login : database -> (string * string) list
(**[students_login] is a list of the [usernames-password pairs] of the students
   in database [d]*)

val get_student : string -> database -> Student.student
(**[get_student un  d ] is the student in [d] with username [un] raises
   [InvalidUserName un] ifstudent is not in [d] Requires: There is at most one
   student with username [un] *)
