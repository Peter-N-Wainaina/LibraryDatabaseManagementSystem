type book = 
{
  name : string;
  genre : string;
  author : string;
  pages : int;
  description : string;
}

(*name is the name of the library
  all_books is the list of all the books in the library*)
  type library = 
  {
    name:string;
    all_books: book list
  }
  
let create_book n g a p d={
  name=n;
  genre=g;
  author=a;
  pages=p;
  description=d
}

let create_library n ={
  name=n;
  all_books=[]
}

(*TODO: Make all_books a set-like list*)
let add_book l b =
  let books =b::l.all_books in 
  {l with all_books=List.sort_uniq Stdlib.compare books }

let view_books l = l.all_books