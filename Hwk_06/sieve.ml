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

let rec from n = Cons ( n, delay ( fun () -> from (n+1)))


(* sift function *)
let sift (n: int) (stm: int stream): int stream =
    filter (fun a -> a mod n <> 0) stm

(* sieve function *)
(* Sieve of Eratosthenes starts from 2 *)

let rec sieve (stm: int stream): int stream =
  match stm with
  |Cons (1, _) -> raise (Failure "This is not the case of Sieve")
  |Cons (a, _) -> Cons (a, delay (fun () -> sieve (sift a stm)))

(* test for sieve *)
let first_ten_primes =take 10 (sieve (from 2))
let first_fifteen_primes =take 15 (sieve (from 2))

let () =
  assert (first_ten_primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]);
  assert (first_fifteen_primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47])


(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
print_endline ("Success! All tests passed.")

