
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

let add_book blst bk =
  let nblst = bk :: blst in List.sort_uniq Stdlib.compare nblst