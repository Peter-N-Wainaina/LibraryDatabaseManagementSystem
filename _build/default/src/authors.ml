let hours_worked = [2;2;2]
let total_hours lst =
  List.fold_left (fun acc x-> acc+x) 0 lst