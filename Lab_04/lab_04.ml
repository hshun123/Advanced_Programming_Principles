let length xs = List.fold_left(fun a _ -> a + 1) 0 xs;;

let andf xs = List.fold_left (&&) true xs;;

let orf xs = List.fold_left (||) false xs;;

let is_elem elm lst = List.fold_left (fun a b -> b = elm||a) false lst;;

let rev lst = List.fold_left (fun a b -> b::a ) [] lst;;

let ascii_sum lst = List.fold_left (fun a b -> Char.code b + a) 0 lst;;

let lebowski lst = List.fold_right (fun a b -> if a =='.' then ','::' '::'d'::'u'::'d'::'e'::'.':: b else a::b) lst [];;
