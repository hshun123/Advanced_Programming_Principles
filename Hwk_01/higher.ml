
let all_odds lst = List.filter (fun a -> if a mod 2 == 0 then false else true) lst;;

let decrement_all lst = List.map(fun a -> a -1)lst;;

let min_fold lst =
  match lst with
  |[] -> raise (Failure "Empty list not accepted")
  |x::rest -> List.fold_left (fun a b -> if (>)a b then b else a)x lst;;

let sum_prod lst = (List.fold_left (+)0 lst, List.fold_left (fun a b -> a * b) 1 lst);;

let partition_left f lst = match  List.fold_left (fun (a,b) c -> if f c then (c::a,b) else (a, c::b))([],[]) lst with 
|(a,b)-> (List.rev a, List.rev b);;

let partition_right f lst = List.fold_right (fun c (a,b)  -> if f c then (c::a,b) else (a, c::b)) lst ([],[]) ;;
