let hours_worked = [ 15; 15; 15 ]
let total_hours lst = List.fold_left (fun acc x -> acc + x) 0 lst
