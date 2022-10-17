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
  borrowed_books : (book*int) list;
  favorite_books : book list;
}

let get_borrowed std = std.borrowed_books

let borrow_book std bk = 
  let books= std.borrowed_books in {std with borrowed_books=bk::books}

let mean week_lst = 
  let sum = List.fold_left (fun x y-> x +. y) 0. week_lst 
  in sum /. float_of_int(List.length week_lst)

let progress pst nw = ((nw /. pst) -.1.) *. 100.

