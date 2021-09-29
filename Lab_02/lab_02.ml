#1
let circle_circum_v1 radius = radius *. 2.0 *. 3.1415;;

#2
let pi = 3.1415
 let circle_circum_v2 = fun radius -> 2.0 *. pi *. radius;;

let circle_circum_v2 radius =
           let pi = 3.1415 in radius *. pi *. 2.0;;

#3
let rec product list =
match list with
|[] -> 0
|x::[]->x
|i::rest -> i * product rest;;


#4
let rec sum_sqrdiffs xs =
match xs with
|[]->0
|x::[]-> -1
|x::(y::[]) -> (x-y)*(x-y)
|x::y::rest -> (x-y)*(x-y) + sum_sqrdiffs(y::rest)
;;

#5
let distance (x_1,y_1) (x_2, y_2) =  sqrt((x_1-.x_2)*.(x_1-.x_2) +. (y_1-.y_2)*.(y_1 -. y_2));;

#6
let triangle_perimeter (x1, y1) (x2, y2) (x3, y3) = distance (x1, y1) (x2,y2) +. distance (x2, y2)(x3,y3) +. distance (x1, y1) (x3,y3);;

