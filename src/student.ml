type book = 
{name : string;
genre : string;
author : string;
pages : int;
description : string;
}

type student = {
  username : string;
  password : string;
  student_id : int; 
  borrowed_books : book list;
  favorite_books : book list;
}

let get_borrowed std = std.borrowed_books

let borrow_book bk bklst= 
  let nwlst = bk ::bklst in List.sort_uniq Stdlib.compare nwlst

let mean week_lst = 
  let sum = List.fold_left (fun x y-> x + y) 0 week_lst 
  in sum / (List.length week_lst)

let progress pst nw = ((nw / pst) - 1) * 100

