(*Group by Eric Hwang, Yongmin Jung, Hoin Jang*)
(*Nothing changed since all look nice*)
#1
let circle_circum_v1 radius = radius *. 2.0 *. 3.1415

(*Change indentation and split into more lines for readability*)
#2
let circle_circum_v2 radius =
  let pi = 3.1415 
  in 
  radius *. pi *. 2.0

(*According to convention I changed the empty product 
case to result in 1 instead of 0. That way, single element list case is also solved. *)
#3
let rec product list =
  match list with
  |[] -> 1
  |i::rest -> i * product rest

(*Deleted the previous rule for single element and empty list,
 added a new rule for handling fewer elements and empty list *)
#4
let rec sum_sqrdiffs xs =
  match xs with
  |_::[]|[] -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
  |x::(y::[]) -> (x-y)*(x-y)
  |x::y::rest -> (x-y)*(x-y) + sum_sqrdiffs(y::rest)

(*Changed to use ** operator to improve readability. It is more readable.*)
#5
let distance (x_1,y_1) (x_2, y_2) =  sqrt((x_1-.x_2)**2.0 +. (y_1-.y_2)**2.0);;

(*Nothing changed. Used (x1, x2) (x2, y2) (x3, y3) instead of p1, p2,p3 because it is 
more clear to show it is calculating the distance between points. *)
#6
let triangle_perimeter (x1, y1) (x2, y2) (x3, y3) = distance (x1, y1) (x2, y2) +. distance (x2, y2)(x3,y3) +. distance (x1, y1) (x3,y3);;

