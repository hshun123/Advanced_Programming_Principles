(*Eric Hwang*)

(* A tree type declaration. *)
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
(* A sample tree containing string list tree *)
let strs_tree_2: string list tree = Node ([""], Empty, Node ([""], Empty, Empty))

(*Part1*)
(*size*)
let rec size (mytree: 'a tree) : int =
  match mytree with
  |Empty -> 0
  |Node (v,lt,rt) -> 1 + size lt + size rt

(*sum*)
let rec sum (mytree: int tree) : int =
  match mytree with
  |Empty -> 0
  |Node (v, lt,rt) -> v + sum lt + sum rt

(*product*)
let rec product (mytree: int tree) : int =
  match mytree with
  |Empty -> 1
  |Node(v, lt,rt) -> v * product lt * product rt

(*charcount*)
let rec charcount (mytree: string tree): int = 
  match mytree with
  |Empty -> 0
  |Node (v, lt, rt) -> String.length v + charcount lt + charcount rt

(*concat*)
let rec concat (mytree: string tree): string =
  match mytree with
  |Empty -> ""
  |Node (v, lt, rt)->  concat lt ^ v ^ concat rt

(*Part2*)
(*list_tree_size*)
(*use List.length to count the size of the list*)
let rec list_tree_size (mytree: 'a list tree) : int =
  match mytree with
  |Empty -> 0
  |Node(v, lt, rt) -> List.length v + list_tree_size lt + list_tree_size rt

(*list_tree_sum*)
(*declare the sum_list recursive function to help find the sum*)
let rec list_tree_sum (mytree: int list tree): int = 
  let rec sum_list (lst: int list) : int = 
    match lst with
    |[] -> 0
    |a::rest -> a + sum_list rest
  in 

  match mytree with
  |Empty ->0
  |Node(v, lt, rt) -> sum_list v + list_tree_sum lt + list_tree_sum rt

(*list_tree_product*)
(*declare the recursive function mul to hlep find product*)
let rec list_tree_product (mytree: int list tree) : int =
  let rec mul (lst: int list): int =
    match lst with
    |[]-> 1
    |a::rest -> a*mul rest
  in
  match mytree with
  |Empty -> 1 
  |Node(v, lt, rt) -> mul v * list_tree_product lt * list_tree_product rt

(*list_tree_charcount*)
(*declare the recursive function count to help count the all characters*)
let rec list_tree_charcount (mytree: string list tree) : int =
  let rec count (lst:string list) : int =
    match lst with
    |[] -> 0
    |a::rest -> String.length a + count rest
  in
  match mytree with
  |Empty -> 0
  |Node(s, lt, rt) -> count s +  list_tree_charcount lt + list_tree_charcount rt

(*list_tree_concat*)
(*declare the recursive function concat to help concat strings*)
let rec list_tree_concat (mytree: string list tree): string =
  let rec concat (lst: string list):string =
    match lst with
    |[]-> ""
    |s::rest -> s ^ concat rest
  in
  match mytree with
  |Empty -> ""
  |Node (s, lt, rt) -> list_tree_concat lt ^ concat s ^ list_tree_concat rt



(*Part1 tests*)
let () = 
  print_string "Testing part 1 ... " ;
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

    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg

(*Part 2 tests*)
let () = 
  print_string "Testing part 2 ... " ;
  try
    
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
