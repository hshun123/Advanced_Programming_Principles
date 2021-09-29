(* Eric Hwang *)
(* hwang241 *)

open Lazee

(* The STREAM signature combines our past work on generators and streams
   into on signature.  It is defined below.
 *)
module type STREAM = sig
  type 'a lazee
  type 'a generator = unit -> 'a
  type 'a t = Cons of 'a * 'a t lazee

  (* Lazy functions *)
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a 

  (* Generator functions *)
  val random_int : int -> int -> int generator
  val random_char : char generator
  val generated_values : 'a generator -> 'a t

  (* Stream functions *)
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t

  (* Add additional stream function ``val`` declarations here.
     These may be useful in later parts of the assignment.
   *)
  val take: int -> 'a t -> 'a list
  val filter: ('a -> bool) -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

(* Define Streams functor that takes a module with sig LAZEE and
   produces module with signature matching STREAM.  Give the header
   line. *)

module Stream (L: LAZEE) : STREAM = struct
 
 (* The three lines below illustrate how types and values from a module
    provided as input to a functor can be copied into the module produced
    by the functor.  You will need to do something similar to this in
    other files in this assignment.
  *)
  type 'a lazee = 'a L.t
  type 'a t = Cons of 'a * 'a t lazee
  let delay = L.delay
  let demand = L.demand

  type 'a generator = unit -> 'a
  (* Below, fill in the type and value implementations so that this
     functor satisfies the STREAM signature as indicate in the functor
     "header" a few lines above.
   *)


  let random_int (low: int) (high: int): int generator
    = fun () -> (Random.int ((high + 1) - low)) + low

  let random_char : char generator
    = fun () -> Char.chr (Random.int (127 - 32) + 32)
  
  let rec generated_values (agen: 'a generator) : 'a t =
    Cons ( agen (), L.delay (fun () -> generated_values agen ))
 
  let head (s: 'a t) : 'a = match s with
    | Cons (v, _) -> v

  let tail (s :'a t) : 'a t = match s with
    | Cons (_, tl) -> L.demand tl

  let rec take (n:int) (s : 'a t) : ('a list) =
    match n, s with
    | 0, _ -> []
    | _, Cons (v, tl) -> v :: take (n-1) (L.demand tl)

  let rec filter (p: 'a -> bool) (s: 'a t) : 'a t =
    match s with
    | Cons (hd, tl) ->
       let rest = L.delay (fun () -> filter p (L.demand tl)) in
       if p hd
       then Cons (hd, rest)
       else L.demand rest
  
  let rec map (f: 'a -> 'b) (s: 'a t) : 'b t =
    match s with
    | Cons (hd, tl) ->
        Cons (f hd, L.delay (fun () -> map f (L.demand tl)))
   
  let rec zip (f: 'a -> 'b -> 'c) (s1: 'a t) (s2: 'b t) : 'c t =
    match s1, s2 with
    | Cons (h1, t1), Cons (h2, t2) ->
        Cons (f h1 h2, L.delay (fun () -> zip f (L.demand t1) (L.demand t2)))


end

(* Add module declarations for ``Stream_lazy`` and ``Stream_slow``.
   These will have the form:
module Stream_slow = ... use Stream functor here ...
module Stream_lazy = ... use Stream functor here ...

 *)

module Stream_slow = Stream (Lazee_slow)

module Stream_lazy = Stream (Lazee)
