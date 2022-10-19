open Yojson.Basic.Util
open Library

type student = {
  username : string;
  password : string;
  student_id : int; 
  borrowed_books : (book*int) list;
  favorite_books : book list;
}


type book = Library.book

let create_student un pw id = {
  username = un ;
  password = pw ;
  student_id = id ;
  borrowed_books = [];
  favorite_books = []
}
let to_book_list h  = let
  name = h |> member "name" |> to_string in let
  genre = h |> member "genre" |> to_string in let
  author = (h |> member "author" |> to_string) in let 
  pages = h |> member "pages" |> to_int in let
  description = h |> member " description" |> to_string in 
  create_book name genre author pages description


let to_borrowed h = 
  (h|> member "book detail" |> to_book_list, h |> member "Days remaining" |> to_int)

let to_student_list h = 
  {
    username = h |> member "username" |> to_string;
    password = h |> member "password" |> to_string;
    student_id = h |> member "student_id" |> to_int;
    borrowed_books = h |> member "borrowed_books" |> to_list |> List.map to_borrowed;
    favorite_books = h |> member "favorite_books" |> to_list|> List.map to_book_list;
  }
let from_json json = 
   json |> member "students" |> to_list |> List.map to_student_list

let username_list h = h |> List.map (fun h -> h.username)

let get_borrowed std = std.borrowed_books

let borrow_book std bk = 
  let books= std.borrowed_books in {std with borrowed_books=bk::books}

let mean week_lst = 
  let sum = List.fold_left (fun x y-> x +. y) 0. week_lst 
  in sum /. float_of_int(List.length week_lst)

let progress pst nw = ((nw /. pst) -.1.) *. 100.

