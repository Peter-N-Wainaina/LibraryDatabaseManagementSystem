exception UserNameNotFound
exception IncorrectPassword

val get_student : string -> string -> Database.database -> Student.student
(**[get_student un pw db] is a student with username [un] and password [pw] in
   database [db]. Requires:[db] is a valid database Raises [UserNameNotFound] un
   if username [un] is not in [d] Raises [IncorrectPassword pw] if [pw] does not
   match [un]*)

val get_librarian : string -> string -> Database.database -> Librarian.lib
(**[get_librarian un pw db] is a librarian with username [un] and password [pw]
   in database [db]. Requires:[db] is a valid database Raises [UserNameNotFound]
   un if username [un] is not in [d] Raises [IncorrectPassword pw] if [pw] does
   not match [un]*)
