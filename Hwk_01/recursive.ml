let odd x = if x mod 2 ==0 then false else true;;

let rec  euclid a b =
          if a == 0 || b == 0 then raise (Failure "Zero not accepted")
          else  if a = b then a
          else if a < b then euclid a (b-a)
          else euclid (a-b) b;;

let frac_simplify (x,y)=
	if euclid x y == 1 then (x,y)
	else(x/ (euclid x y), y/(euclid x y));;

let rec min_list lst =
    match lst with
|[]-> raise (Failure "Empty list not accepted")
|x::[] -> x
|x::y::rest -> if x > y then min_list (y::rest) else min_list (x::rest);;

let rec drop num lst = 
 if num == 0 then lst
 else if
   num > List.fold_left(fun a _ -> a + 1)0 lst then []
 else
  (drop (num-1)( match lst with 
    |[] -> []
    |a::b -> b));;
