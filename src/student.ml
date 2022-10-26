open Yojson.Basic.Util
open Library

exception UnknownStudent

type student_id = int

type student = {
  username : string;
  password : string;
  student_id : student_id;
  borrowed_books : (book * int) list;
  favorite_books : book list;
}

let create_student un pw id =
  {
    username = un;
    password = pw;
    student_id = id;
    borrowed_books = [];
    favorite_books = [];
  }

let to_book_list h =
  let name = h |> member "name" |> to_string in
  let genre = h |> member "genre" |> to_string in
  let author = h |> member "author" |> to_string in
  let pages = h |> member "pages" |> to_int in
  let description = h |> member "description" |> to_string in
  create_book name genre author pages description

let to_borrowed h =
  ( h |> member "book detail" |> to_book_list,
    h |> member "Days remaining" |> to_int )

let to_student h =
  {
    username = h |> member "username" |> to_string;
    password = h |> member "password" |> to_string;
    student_id = h |> member "student_id" |> to_int;
    borrowed_books =
      h |> member "borrowed_books" |> to_list |> List.map to_borrowed;
    favorite_books =
      h |> member "favorite_books" |> to_list |> List.map to_book_list;
  }

let favorite_books std =
  std.favorite_books |> List.map (fun x -> x |> book_name)

let get_borrowed std = std.borrowed_books

let borrowed_books std =
  std.borrowed_books |> List.map (fun x -> fst x |> book_name)

let get_username std = std.username
let get_id std = std.student_id

let borrow_book std bk =
  let books = std.borrowed_books in
  { std with borrowed_books = bk :: books }

let add_favorite std bk =
  let books = std.favorite_books in
  { std with favorite_books = bk :: books }

let find_pw lst username =
  let x = List.find (fun x -> x.username = username) lst in
  x.password

let find_student lst username =
  let x = List.find (fun x -> x.username = username) lst in
  x

let mean week_lst =
  if week_lst = [] then 0.
  else
    let sum = List.fold_left (fun x y -> x +. y) 0. week_lst in
    sum /. float_of_int (List.length week_lst)

let progress pst nw = ((nw /. pst) -. 1.) *. 100.
