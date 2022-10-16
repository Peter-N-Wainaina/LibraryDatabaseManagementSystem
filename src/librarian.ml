
type lib = 
{username : string;
password : string;
staff_id : int;
}

type book = 
{name : string;
genre : string;
author : string;
pages : int;
description : string;
}

type studentID = int
type all_books = book list

let add_book all_books bk =
  let nblst = bk :: all_books in List.sort_uniq Stdlib.compare nblst

let get_borrowed sid =
  (* find the student account with the given student ID and then access 
     the list of borrowed books for that student*)
  failwith "error"  

let view_books all_books= all_books