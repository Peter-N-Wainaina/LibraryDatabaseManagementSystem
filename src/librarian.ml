
type lib = 
{username : string;
password : string;
staff_id : int;
}

(*TODO:Replace this with the value from library compilation unit *)
type book = 
{
  name : string;
  genre : string;
  author : string;
  pages : int;
  description : string;
}

type studentID = int

(*TODO: Replace this with the same value from library compilation unit*)
let  all_books:book list = []

(*[create_book n g a p d] is a book with name n, genre g ,author a , pages p, description d*)
let create_book n g a p d={
  name=n;
  genre=g;
  author=a;
  pages=p;
  description=d
}
let add_book all_books bk =
  let nblst = bk :: all_books in List.sort_uniq Stdlib.compare nblst

let get_borrowed sid =
  (* find the student account with the given student ID and then access 
     the list of borrowed books for that student*)
  failwith "error"  

let view_books () = all_books