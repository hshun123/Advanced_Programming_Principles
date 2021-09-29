(* Eric Hwang *)
(* hwang241 *)

(* Organizing the lazy evaluation code into a module. *)

(* Here is a signature for what needs to be provided by any
   LAZEE module.
 *)
module type LAZEE = sig
  type 'a t
  val delay: (unit -> 'a) -> 'a t
  val demand: 'a t -> 'a 
end

(* Fill in the contents of the Lazee module below based on the 
   implementation from class and in the public class repository.
   You will need to rename the ``lazee`` type to ``t``.
 *)
module Lazee : LAZEE = struct
  type 'a t = 'a hidden ref

  and 'a hidden = Value of 'a 
                 | Thunk of (unit -> 'a)

  let delay (unit_to_x: unit -> 'a) : 'a t = ref (Thunk unit_to_x)
    
  let force (l: 'a t) : unit = match !l with
    | Value _ -> ()
    | Thunk f -> l := Value (f ())

  let demand (l: 'a t) : 'a = 
    force l; 
    match !l with
    | Value v -> v
    | Thunk f -> raise (Failure "this should not happen")
end

(* Here is another module that implements the LAZEE signature above
   but it is not as efficient.  Explain why in THIS comment.

   -- write your explanation here --
   The Lazee_slow module is saves the value of executing Thunk instead of using references. 
   It causes unneccessary steps because it always has to use unit to determine the next step instead of just "ref" it. 
   So, using reference to access parameters is faster than 
   just saves the value of (unit -> 'a). In addition, when the delay funciton and demand function are 
   called for many times, the total speed will become much slower because of the inefficient accessing.     
 *)
module Lazee_slow : LAZEE = struct
  type 'a t = unit -> 'a
  let delay (unit_to_x: unit -> 'a) : 'a t = unit_to_x
  let demand (l: 'a t) : 'a = l ()
end
