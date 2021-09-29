(* Eric Hwang *)

(* declare a type for convinence *)
type position = int * int

let rec is_not_elem set v = not (List.mem v set)

(* final positoin *)
let final1 p1 = p1 = (3, 5)
let final2 p2 = p2 = (5, 1)

(* maze_moves function for checking valid moves *)
let maze_moves (pos: (int * int)) : (int * int) list =
  match pos with
  | (1, 1) -> [(2,1)]
  | (1, 2) -> [(1,3); (2,2)]
  | (1, 3) -> [ (1,2); (2,3); (1,4) ]
  | (1, 4) -> [(1,3); (1,5)]
  | (1, 5) -> [(1,4); (2,5)]
  
  | (2, 1) -> [(3,1)]
  | (2, 2) -> [(1,2); (3,2)]
  | (2, 3) -> [ (1,3)]
  | (2, 4) -> [(2,5); (3,4)]
  | (2, 5) -> [(1,5); (2,4)]
  
  | (3, 1) -> [(2,1); (3,2)]
  | (3, 2) -> [(2,2); (3,3); (4,2)]
  | (3, 3) -> [(3,2); (3,4); (4,3)]
  | (3, 4) -> [(2,4); (3,3) ;(4,4)]
  | (3, 5) -> [(4,5)]
  
  | (4, 1) -> [(4,2)]
  | (4, 2) -> [(4,1); (3,2)]
  | (4, 3) -> [(3,3); (5,3)]
  | (4, 4) -> [(3,4); (4,5)]
  | (4, 5) -> [(3,5); (4,4); (5,5)]

  | (5, 1) -> [(5,2)]
  | (5, 2) -> [(5,1); (5,3)]
  | (5, 3) -> [(5,2); (5,4); (4,3)]
  | (5, 4) -> [(5,3)]
  | (5, 5) -> [(5,4)]
  
  | _ -> raise (Failure "No Way")


(* exception *)
exception KeepLooking

(* maze function *)

let maze () =
  let rec go_from pos path =
    if final1 pos || final2 pos
    then  path
    else
      ( match List.filter (is_not_elem path) (maze_moves pos) with
        | [] -> raise KeepLooking
        | x::xs -> (try go_from x (path @ [x]) with
                  | KeepLooking -> (match xs with
                                    | [] -> raise KeepLooking
                                    | s::rest -> go_from s (path @ [s]))
                   )
      )
  in try Some ( go_from (2,3) [(2,3)]) with
     | KeepLooking -> None


