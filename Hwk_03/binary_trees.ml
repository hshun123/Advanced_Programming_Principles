(*Eric Hwang*)
type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

(*insert_by*)
let rec insert_by (f: 'a -> 'a ->int) (v:'a)( mytree:'a btree): 'a btree =
  match mytree with
  |Nil -> Leaf v
  |Leaf a -> if (f a v) > 0 then Fork(Leaf v,a, Nil) else if (f a v) < 0 then Fork(Nil, a , Leaf v) else mytree
  |Fork(lt,a,rt)-> if (f a v) > 0 then Fork((insert_by f v lt),a,rt) else if (f a v) < 0 then Fork(lt, a, (insert_by f v rt)) else mytree

(*from_list*)
let from_list (f:  'a -> 'a -> int) (lst: 'a list): 'a btree =
  List.fold_left (fun a b -> insert_by f b a )Nil lst

(*reduce*)
let rec reduce (t: 'a btree) (b: 'b) (f: 'b -> 'a -> 'b -> 'b) : 'b =
  match t with
  | Nil -> b
  | Leaf a -> f b a b
  | Fork (tl, a, tr) -> f (reduce tl b f) a (reduce tr b f)

(*to_list*)
let to_list (mytree: 'a btree) : 'a list =
  reduce mytree [] (fun lt v rt -> lt @ ( v :: rt))

(*rem_dups*)
(* 1. I chose List.fold_left function*)
(* 2. I chose List.fold_left because this one use accumulator which I can use to check if the element is already in the accumulator, 
that way I can reduce duplicates *)
(* 3. I did not use List.fold_right because I need to remain the first instance that appears in the list. 
If I use List.fold_right, then it is hard to know if the element has appeared before. *)

let rem_dups (lst: 'a list) : 'a list = List.rev (List.fold_left (fun a b -> if List.mem b a then a else b::a) [] lst)

(*form-check*)
(*helper function that checks if the form of the second invariant is satisfied*)
let rec form_check (mytree: 'a btree) : bool = 
  match mytree with
  |Nil -> true
  |Leaf a -> true
  |Fork(lt, a, rt) -> if (lt = Nil && rt = Nil) then false
                      else (form_check lt)&& (form_check rt)
(*check*)
(*use rem_dups to check if there is duplicate elements in the tree to check the first invariant*)
(*use List.sort and to_list to check the third invariant*)
let check (mytree: 'a btree) : bool =
  form_check mytree && 
  match mytree with
  |Nil -> true
  |Leaf a -> true
  |Fork (lt, a, rt) -> (rem_dups (List.sort compare (to_list mytree)) = 
                     (rem_dups (to_list lt)) @ [a] @ (rem_dups (to_list rt)))
               
(*tests*)
let () = 
  print_string "Testing part 4 ... " ;
  try
    assert (insert_by compare 4 Nil = Leaf 4);
    assert (insert_by compare 2 (insert_by compare 4 Nil) =
              Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
              Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) = 
              insert_by compare 4 Nil);

    (* Add more asserts here as you need them *)
    assert (insert_by compare 5 (Leaf 4) = Fork (Nil, 4, Leaf 5));
    assert (insert_by compare 4 (Leaf 4) = Leaf 4);

    assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                   ) 
           );
    assert (from_list compare [] = Nil );
    assert (from_list compare [4;2;5;3;6;7;8;8;8;8;8;8;8;8;8;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                   )
           );
    assert (from_list compare [1;2;3;4;5] =
              Fork (Nil, 1,
                    Fork (Nil, 2, Fork (Nil, 3, Fork (Nil, 4, Leaf 5)))
                   )
           );
    assert (from_list compare [5;4;3;2;1] =
              Fork ( Fork (Fork (Fork (Leaf 1, 2, Nil), 3, Nil),  4, Nil), 5,
                    Nil)
           );
    
    assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));
    assert (List.sort compare [1;2;3;4;6;7;8] =
              to_list (from_list compare [1;2;3;4;6;7;8]));
    assert (List.sort compare [] =
              to_list (from_list compare []));
    assert (List.sort compare [5;6;7;3;4;1] =
              to_list (from_list compare [1;6;7;3;4;5]));
    
    assert (reduce (from_list compare [1;2;3;4;6;7;8]) 0 (fun lt v rt -> lt + v + rt) = 31);
    assert (reduce (from_list compare [1;2;3;4;5]) 1 (fun lt v rt -> lt * v * rt) = 120);

    assert (rem_dups [1;3;3;5;1;7;2;5;7;2] = [1; 3; 5; 7; 2]);
    assert (check Nil);
    assert (check (Leaf 'c'));
    assert (check (from_list compare [4;2;5;3;6;7;8]));
    assert (not (check (Fork (Nil, 4, Nil))));
    assert (not (check (Fork (Leaf 3, 3, Nil))));
    assert (not (check (Fork (Fork (Nil, 2, Leaf 3), 1, Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))) )));

    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
