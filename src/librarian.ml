open Library


type lib = 
{username : string;
password : string;
staff_id : int;
}

type studentID = int

let add_book l (bk : Library.book) =
view_books (Library.add_book l bk)

let get_first = function
  |(x, y) -> x

let get_borrowed (sid : Student.student)=
  (* find the student account with the given student ID and then access 
     the list of borrowed books for that student*)
  List.map get_first (Student.get_borrowed sid)

let view_books l= Library.view_books l