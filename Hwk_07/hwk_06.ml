(* Eric Hwang *)
(* hwang241 *)

open  Streams
(* Below is a signature, HWK_06, that indicates the type and values
   that were implemented in Homework 06.
 *)

module type HWK_06 = sig

  (* HWK_06 has a nested module, for streams. *)
  module S : Streams.STREAM 

  (* Part 2 functions *)
  val n_random_chars : int -> char list S.generator
  val n_generated_values : 'a S.generator -> int -> 'a list S.generator
  val n_increasing_length_lists : 'a S.generator -> int -> 
                                  'a list list S.generator
  val n_random_length_lists : 'a S.generator -> int -> int -> int ->
                              'a list list S.generator

  (* Part 3 functions *)
  val increasing_length_lists : 'a S.generator -> 'a list S.t
  val all_coordinates : (int * int) S.t
  val non_negative_coordinates : (int * int) S.t
  val all_list_length_pairs : 'a S.generator -> ('a list * 'a list) S.t

end

(* Define the Hwk_06 functor and its uses below.
 *)

module Hwk_06 (St: Streams.STREAM) : HWK_06 = struct

  module S = St
  
  (* Part 2 functions *)

  (* n_random_chars *)
  let rec n_random_chars (n: int) : (char list) S.generator =
    fun () -> if n = 0 then [] else S.random_char () :: n_random_chars (n - 1) ()

  (* n_generated_values *)
  let rec n_generated_values (agen: 'a S.generator) (n: int): 'a list S.generator =
    fun () -> if n = 0 then [] else agen () :: n_generated_values agen (n - 1) ()

  (* n_increasing_length_lists *)
  let rec n_increasing_length_lists (agen: 'a S.generator) (n: int): 'a list list S.generator =
    let rec answer (bgen:'a S.generator) (a: int): 'a list list S.generator =
      fun () -> if a = 0 then [] else n_generated_values bgen (a - 1) ():: answer bgen (a - 1) ()
    in
      fun () -> List.rev (answer agen n ())

  (* n_random_length_lists *)
  let rec n_random_length_lists (agen: 'a S.generator) (n: int) (low: int) (high: int) : ('a list list) S.generator =
    let rec answer (bgen:'a S.generator) (a: int) (l:int) (h: int): 'a list list S.generator =
      fun () -> if a = 0 then [] else n_generated_values bgen (S.random_int l h ()) () :: answer bgen (a - 1) l h ()
    in
      fun () -> answer agen n low high ()

  (* Part 3 functions *)

  (* increasing_length_lists *)
  let rec increasing_length_lists (agen: 'a S.generator): 'a list S.t =
    let rec increasing (bgen: 'a S.generator) (n: int) =
      S.Cons (n_generated_values bgen n (), S.delay (fun () -> increasing bgen (n + 1)))
    in
    increasing agen 0

  (* all_coordinates *)
  let rec all_coordinates: (int * int) S.t =
         let rec help (a: int) (b: int) : (int * int) S.t =
           match a, b with
           | 0,0 -> S.Cons ((0,0), S.delay (fun () -> help (a + 1) b))
           | x,y -> if x + y > 0 && x > y then S.Cons ((x, y), S.delay (fun () -> help a (b + 1) ))
                    else if x + y > 0 && x <= y then S.Cons ((x, y), S.delay (fun () -> help (a - 1) b))
                    else if x + y <= 0 && x < y then S.Cons ((x, y), S.delay (fun () -> help a (b - 1)))
                    else S.Cons ((x, y), S.delay (fun () -> help (a+1) b))
         in
         help 0 0

  (* non_negative_coordinates *)
  let rec non_negative_coordinates: (int * int) S.t =
    S.filter (fun (a, b) -> a >= 0 && b >= 0) all_coordinates

  (* all_list_length_pairs *)
  let rec all_list_length_pairs (agen: 'a S.generator) : ('a list * 'a list) S.t =
    S.map (fun (a,b) -> (n_generated_values agen a (), n_generated_values agen b ())) non_negative_coordinates

end

  module H6_fast = Hwk_06 (Stream_lazy)

  module H6_slow = Hwk_06 (Stream_slow)
