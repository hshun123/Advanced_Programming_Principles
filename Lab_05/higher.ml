(* Eric Hwang, Yongmin Jung, Hoin Jang *)

(* TO DO *)

let all_odds (lst: int list) : int list = List.filter (fun a -> if a mod 2 == 0 then false else true) lst

(* TO DO *)

let decrement_all (lst: int list) : int list = List.map(fun a -> a -1)lst

(* Type annotation added *)

let min_fold (lst: 'a list) : 'a =
    match lst with
    |[] -> raise (Failure "Empty list not accepted")
    |x::rest -> List.fold_left (fun a b -> if (>)a b then b else a)x lst

(* Type annotation added *)

let sum_prod (lst: int list) : int * int = (List.fold_left (+) 0 lst, List.fold_left (fun a b -> a * b) 1 lst)

(* Type annotation added. Indentation changed *)

let partition_left (f: ('a -> bool)) (lst: 'a list) : 'a list * 'a list = 
    match  List.fold_left (fun (a,b) c -> if f c then (c :: a, b) else (a, c :: b)) ([],[]) lst with 
    |(a,b)-> (List.rev a, List.rev b)

(* Type annotation added *)

let partition_right (f: ('a -> bool)) (lst: 'a list) : 'a list * 'a list = List.fold_right (fun c (a, b)  -> if f c then (c :: a,b) else (a, c :: b)) lst ([],[])
