(* Eric Hwang, Yong min Jung, Hoin Jang *)


let odd x = if x mod 2 ==0 then false else true

(* Changed the equality operator "==" to"=" and added type annotations to improve readability *)

let rec  euclid (a: int) (b: int) : int =
    if a = 0 || b = 0 then raise (Failure "Zero not accepted")
    else  if a = b then a
    else if a < b then euclid a (b-a)
    else euclid (a-b) b

(* Changed the equality operator "==" to"=" and added type annotations to improve readability *)

let frac_simplify ((x, y): int * int) : int * int =
    if euclid x y = 1 then (x,y)
    else (x/ (euclid x y), y/(euclid x y))

(* Indentation changed and type annotation added to improve readability *)

let rec min_list (lst : 'a list) : 'a =
    match lst with
    |[]-> raise (Failure "Empty list not accepted")
    |x::[] -> x
    |x::y::rest -> if x > y then min_list (y::rest) else min_list (x::rest)


(* Replace if statement with pattern matching. 
Erased Lis.fold_left function and handled in pattern matching. 
This improve the readability and ouput the desirable result without using higher order function *)

let rec drop (num: int) (lst: 'a list) : 'a list = 
    match lst with
    | [] -> []
    | x:: rest when num > 0 -> drop (num - 1) rest
    | x :: rest -> x :: rest
