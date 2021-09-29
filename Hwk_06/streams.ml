(* Lazy streams and generators.

   -Eric Van Wyk
 *)

(* Eric Hwang *)
(* hwang241 *)

(* Lazy type declaration and supporting function. 
 *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values 
 *)
type 'a stream = Cons of 'a * 'a stream lazee

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))


(* A copy of the generator declarations.
 *)
type 'a generator = unit -> 'a

let random_int (low: int) (high: int) : int generator
  = fun () -> (Random.int ((high+1) - low)) + low

let random_char : char generator
  = fun () -> Char.chr (Random.int (127 - 32) + 32)


let rec generated_values (agen: 'a generator) : 'a stream =
  Cons ( agen (), delay (fun () -> generated_values agen ))

let some_random_ints : int list = 
  take 10 (generated_values (random_int 0 100))


(* For part 3 write your implementations for the streams 
   - increasing_lenth_lists
   - all_coordinates
   - non_negative_coordinates
   - all_list_length_pairs
  below.
 *)

(* n_generated_values for increasing_length_lists func*)
let rec n_generated_values (agen: 'a generator) (n: int): 'a list generator =
    fun () -> if n = 0 then [] else agen () :: n_generated_values agen (n - 1) ()

(* increasing_length_lists *)
let rec increasing_length_lists (agen: 'a generator): 'a list stream =
  let rec increasing (bgen: 'a generator) (n: int) = 
    Cons (n_generated_values bgen n (), delay (fun () -> increasing bgen (n + 1)))
  in
  increasing agen 0

(* all_coordinates *)
let rec all_coordinates: (int * int) stream =
         let rec help a b = 
           match a, b with
           | 0,0 -> Cons ((0,0), delay (fun () -> help (a + 1) b))
           | x,y -> if x + y > 0 && x > y then Cons ((x, y), delay (fun () -> help a (b + 1) ))
                    else if x + y > 0 && x <= y then Cons ((x, y), delay (fun () -> help (a - 1) b))
                    else if x + y <= 0 && x < y then Cons ((x, y), delay (fun () -> help a (b - 1)))
                    else Cons ((x, y), delay (fun () -> help (a+1) b))
         in
         help 0 0

(* non_negative_coordinates *)
let rec non_negative_coordinates: (int * int) stream =
    filter (fun (a, b) -> a >= 0 && b >= 0) all_coordinates

(* all_list_length_pairs *)
let rec all_list_length_pairs (agen: 'a generator) =
    map (fun (a,b) -> (n_generated_values agen a (), n_generated_values agen b ())) non_negative_coordinates

(* Below, use the functions above to test the prod function and
   property given below:
 *)
let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys

let prop_prooerty (l1, l2) = prod (l1 @ l2) = prod l1 * prod l2


(*test*)
(* test the property of prod function *)

let test1 = map prop_prooerty (all_list_length_pairs (random_int 0 9))
let test2 = map prop_prooerty (all_list_length_pairs (random_int 0 0))
let test3 = map prop_prooerty (all_list_length_pairs (random_int 0 123))

(* prop_prooerty function should always return true*)

let e1 = List.fold_left (fun a b -> a && b) true (take 0 test1)
let e2 = List.fold_left (fun a b -> a && b) true (take 1 test1)
let e3 = List.fold_left (fun a b -> a && b) true (take 99 test1)
let e4 = List.fold_left (fun a b -> a && b) true (take 159 test1)

let e5 = List.fold_left (fun a b -> a && b) true (take 0 test2)
let e6 = List.fold_left (fun a b -> a && b) true (take 1 test2)
let e7 = List.fold_left (fun a b -> a && b) true (take 99 test3)
let e8 = List.fold_left (fun a b -> a && b) true (take 159 test3)

let () =
  assert (e1 = true);
  assert (e2 = true);
  assert (e3 = true);
  assert (e4 = true);
  assert (e5 = true);
  assert (e6 = true);
  assert (e7 = true);
  assert (e8 = true)

(* test for increasing_length_lists *)

let test4 = take 1 (increasing_length_lists random_char)
let test5 = take 4 (increasing_length_lists random_char)
let test6 = take 6 (increasing_length_lists random_char)
let test7 = take 10 (increasing_length_lists (random_int 0 9))

(* The sum of the length of each list *)
let e9 = List.fold_left (fun a b -> a + List.length b ) 0 test4
let e10 = List.fold_left (fun a b -> a + List.length b ) 0 test5
let e11 = List.fold_left (fun a b -> a + List.length b ) 0 test6
let e12 = List.fold_left (fun a b -> a + List.length b ) 0 test7

(* length of each list is increasing by 1 *)
let e13 = List.map (fun a -> List.length a) test5
let e14 = List.map (fun a -> List.length a) test4
let e15 = List.map (fun a -> List.length a) test6

let () =
  assert (e9 = 0);
  assert (e10 = 6);
  assert (e11 = 15);
  assert (e12 = 45 );
  assert (e13 = [0; 1; 2; 3]);
  assert (e15 = [0; 1; 2; 3; 4; 5]);
  assert (e14 = [0])

(* test for all_coordinates *)

let test8 = take 1 all_coordinates
let test9 = take 50 all_coordinates

let () =
  assert (test8 = [(0, 0)]);
  assert (test9 = [(0, 0); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); 
 (1, -1); (2, -1); (2, 0); (2, 1); (2, 2); (1, 2); (0, 2); (-1, 2); (-2, 2);
 (-2, 1); (-2, 0); (-2, -1); (-2, -2); (-1, -2); (0, -2); (1, -2); (2, -2);
 (3, -2); (3, -1); (3, 0); (3, 1); (3, 2); (3, 3); (2, 3); (1, 3); (0, 3);
 (-1, 3); (-2, 3); (-3, 3); (-3, 2); (-3, 1); (-3, 0); (-3, -1); (-3, -2);
 (-3, -3); (-2, -3); (-1, -3); (0, -3); (1, -3); (2, -3); (3, -3); (4, -3)])

(* test for non_negative_coordinates *)

let test10 = take 10 non_negative_coordinates
let test11 = take 50 non_negative_coordinates

(* every elements should not be negative *)
let e16 = List.fold_left (fun a (b, c) -> a  && b >= 0 && c >= 0) true test10
let e17 = List.fold_left (fun a (b, c) -> a  && b >= 0 && c >= 0) true test11

let () =
  assert (test10 = [(0, 0); (1, 0); (1, 1); (0, 1); (2, 0); (2, 1); (2, 2); (1, 2); (0, 2); (3, 0)]);
  assert (e16 = true);
  assert (e17 = true)

(* test for all_list_length_pairs *)

let test12 = take 10 (all_list_length_pairs (random_int 0 9))
let test13 = take 10 (all_list_length_pairs (random_int 0 0))

(* The length of pairs of lists are the same as the non-negative coordinates *)
let e18 = List.fold_left (fun acc (x, y) -> acc + (List.length x + List.length y)) 0 test12

let () =
  assert (test13 = [([], []); ([0], []); ([0], [0]); ([], [0]); ([0; 0], []); ([0; 0], [0]);
                       ([0; 0], [0; 0]); ([0], [0; 0]); ([], [0; 0]); ([0; 0; 0], [])]);

  assert (e18 = List.fold_left (fun acc (x, y) -> acc + x + y) 0 test10)


(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
print_endline ("Success! All tests passed.")

