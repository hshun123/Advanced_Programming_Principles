(*Eric Hwang*)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )
(* A sample tree containing ints *)
let int_tree_2 : int tree = Node (19, Empty, Empty)

(* A sample tree containing ints *)
let int_tree_3 : int tree = Node (7, Empty, Node(13, Empty, Empty))

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )
(* A sample tree containing strings *)
let str_tree_2 : string tree = Node ("single Node", Empty, Empty)

(* A sample tree containing strings *)
let str_tree_3 : string tree = Node ("", Empty, Node ("", Empty, Empty))

(*trees for part2*)
(* A sample tree containing int list tree *)
let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

(* A sample tree containing ints *)
let ints_tree_2: int list tree = Node ([1;4], Empty, Empty)

(* A sample tree containing ints *)
let ints_tree_3: int list tree = Node ([5;5;5], Empty, Node ([], Empty, Empty))

(* A sample tree containing string list tree *)
let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )

(* A sample tree containing ints *)
let strs_tree_2: string list tree = Node ([""], Empty, Node ([""], Empty, Empty))

(*reduce function*)
let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
  match t with
  | Empty -> b
  | Node (v, tl, tr) -> f v (reduce tl b f) (reduce tr b f)

(*size*)
let size (mytree: 'a tree) : int = 
  reduce mytree 0 (fun v lt rt -> 1 + lt + rt)

(*sum*)
let sum (mytree: int tree): int =
  reduce mytree 0 (fun v lt rt -> v + lt + rt)

(*product*)
let product (mytree: int tree) : int =
  reduce mytree 1 (fun v lt rt -> v * lt * rt)

(*charcount*)
let charcount (mytree: string tree): int =
  reduce mytree 0 (fun v lt rt -> String.length v + lt + rt)

(*concat*)
let concat (mytree:string tree): string =
  reduce mytree "" (fun v lt rt -> lt ^ v ^ rt)

(*list_tree_size*)
let list_tree_size (mytree: 'a list tree): int =
  reduce mytree 0 (fun v lt rt -> List.length v + lt + rt)

(*list_tree_sum*)
let list_tree_sum (mytree:int list tree): int =
  reduce mytree 0 (fun v lt rt -> List.fold_left (fun a b -> a + b) 0 v + lt + rt)

(*list_tree_product*)
let list_tree_product (mytree: int list tree): int =
  reduce mytree 1 (fun v lt rt -> List.fold_left (fun a b -> a * b) 1 v* lt* rt)

(*list_tree_charcount*)
let list_tree_charcount (mytree:string list tree): int =
  reduce mytree 0 (fun v lt rt -> List.fold_left (fun a b -> a + String.length b)0 v + lt + rt)

(*list_tree_concat*)
let list_tree_concat (mytree: string list tree): string =
  reduce mytree "" (fun v lt rt -> lt ^ (List.fold_left (fun a b -> a ^ b)"" v) ^ rt)


(*Tests*)
let () = 
  print_string "Testing part 3 ... " ;
  try
    
    assert (size str_tree = 4);
    assert (size Empty = 0);
    assert (size int_tree = 4);
    assert (size int_tree_2 = 1);
    assert (size str_tree_2 = 1);
    assert (size int_tree_3 = 2);
    assert (size str_tree_3 = 2);
    
    assert (sum int_tree = 10);
    assert (sum Empty = 0);
    assert (sum int_tree_2 = 19);
    assert (sum int_tree_3 = 20);

    assert (product int_tree = 24);
    assert (product Empty = 1);
    assert (product int_tree_2 = 19);
    assert (product int_tree_3 = 91);
    
    assert (charcount str_tree = 20);
    assert (charcount Empty = 0);
    assert (charcount str_tree_2 = 11);
    assert (charcount str_tree_3 = 0);    

    assert (concat str_tree = "I really love OCaml!");
    assert (concat Empty = "");
    assert (concat str_tree_2 = "single Node");
    assert (concat str_tree_3 = "");

    assert (list_tree_size strs_tree = 11);
    assert (list_tree_size Empty = 0);
    assert (list_tree_size ints_tree = 10);
    assert (list_tree_size ints_tree_2 = 2);
    assert (list_tree_size ints_tree_3 = 3);
    assert (list_tree_size strs_tree_2 = 2);
    
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_sum Empty = 0);
    assert (list_tree_sum ints_tree_2 = 5);
    assert (list_tree_sum ints_tree_3 = 15);

    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_product Empty = 1);
    assert (list_tree_product ints_tree_2 = 4);
    assert (list_tree_product ints_tree_3 = 125);

    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_charcount Empty = 0);
    assert (list_tree_charcount strs_tree_2 = 0);

    assert (list_tree_concat strs_tree = 
              "I really do love Ocaml!  It must be your favorite too!");
    assert (list_tree_concat Empty = "");
    assert (list_tree_concat strs_tree_2 = "");

    
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
