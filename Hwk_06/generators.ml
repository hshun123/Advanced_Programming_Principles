(* Generator declarations for Hwk 06.  

   - Eric Van Wyk
 *)

(* Eric Hwang *)
(* hwang241 *)

(* Generators - a form of delayed evaluation that uses some
   side-effecting computation.
 *)
type 'a generator = unit -> 'a

let random_int (low: int) (high: int) : int generator
  = fun () -> (Random.int ((high+1) - low)) + low

let single_digit = random_int 0 9

let random_char : char generator
  = fun () -> Char.chr (Random.int (127 - 32) + 32)

let random_pair (agen: 'a generator) (bgen: 'b generator)
    : ('a * 'b) generator
  = fun () -> (agen (), bgen ())

let random_int_char_pair : (int * char) generator =
  random_pair (random_int 0 100) random_char

(* generating lists of data *)
let length_3 (agen: 'a generator) : ('a list) generator =
  fun () -> [ agen (); agen(); agen() ]


(* The even-odd example
 *)
let rec even n = match n with
  | 0 -> true
  | _ -> odd (n-1)
and odd n = match n with
  | 0 -> false
  | _ -> even (n-1)

let even_or_odd n = even n || odd n
let not_even_and_odd n = not (even n && odd n)
let even_odd_property n = even_or_odd n && not_even_and_odd n

(* These properties, the last of which combines the first two, should
   return true for any input.  We can use ``length_3`` to create a
   list of integers generator, use it to generate data, and then map
   the property function over the list of test data.  This is done
   below:
 *)

let test_cases_gen = length_3 (random_int 0 100)
let test_cases_ints = test_cases_gen ()
let test_results_all_even_odd = List.map even_odd_property test_cases_ints

(* We can write a conjunction function, called ``all``, to check that
   all tests pass and apply it to all the test results above:
 *)
let rec all = function
  | [] -> true
  | true::bs -> all bs
  | false::_ -> false

let test_results_even_odd : bool = all test_results_all_even_odd


(* Write your solutions to Part 2 functions here:
   (n_random_chars, 
    n_generated_values, 
    n_increasing_length_lists,
    n_random_length_lists
*)
(* n_random_chars *)
let rec n_random_chars (n: int) : (char list) generator =
    fun () -> if n = 0 then [] else random_char () :: n_random_chars (n - 1) ()

(* n_generated_values *)
let rec n_generated_values (agen: 'a generator) (n: int): 'a list generator =
    fun () -> if n = 0 then [] else agen () :: n_generated_values agen (n - 1) ()

(* n_increasing_length_lists *)
let rec n_increasing_length_lists (agen: 'a generator) (n: int): 'a list list generator =
  let rec answer (bgen:'a generator) (a: int): 'a list list generator =  
    fun () -> if a = 0 then [] else n_generated_values bgen (a - 1) ():: answer bgen (a - 1) ()
  in
    fun () -> List.rev (answer agen n ())

(* n_random_length_lists *)
let rec n_random_length_lists (agen: 'a generator) (n: int) (low: int) (high: int) : ('a list list) generator =
  let rec answer (bgen:'a generator) (a: int) (l:int) (h: int): 'a list list generator =
    fun () -> if a = 0 then [] else n_generated_values bgen (random_int l h ()) () :: answer bgen (a - 1) l h ()
  in
    fun () -> answer agen n low high ()



(* Here are the functions and property you can test, using
   your functions above:
 *)
let rec sum = function
  | [] -> 0
  | y::ys -> y + sum ys

let rec length = function
  | [] -> 0
  | y::ys -> 1 + length ys

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys

let inc_all_property l = sum (inc_all l) = length l + sum l

(* test *)

let test_p_2 =
  fun () -> n_generated_values (random_int 0 100) 10 ()
            <> n_generated_values (random_int 0 100) 10 ()
            && List.length (n_generated_values (random_int 0 100) 10 ()) = 10

let test_cases_lists_1_gen = n_increasing_length_lists (random_int 0 9) 5

let test_cases_5 = test_cases_lists_1_gen ()

let test_results_5 = all (List.map inc_all_property test_cases_5)

let init n f =
  let rec helper i f = if i < n then f i :: helper (i+1) f else []
  in helper 0 f

let test_p_3 len =
  ( n_increasing_length_lists (random_int 0 9) len () <>
      n_increasing_length_lists (random_int 0 9) len () ) 
  && ( ( List.map 
           List.length 
           (n_increasing_length_lists (random_int 0 9) len ()) )
       = init len (fun x -> x)
     )

let test_p_4 len =
  (* values in random lists of lists are different *)
  ( n_random_length_lists random_char len 0 10 () <>
      n_random_length_lists random_char len 0 10 () )
  && (* length of lists are different *)
    ( ( List.map 
          List.length 
          (n_random_length_lists random_char len 0 10 ()) )
      <>
        ( List.map 
            List.length 
            (n_random_length_lists random_char len 0 10 ()) )
    )

(* tests *)
(* test for n_random_chars *)
let test1 = n_random_chars  10 ()
            <> n_random_chars  10 ()
            && List.length (n_random_chars 10 ()) = 10

let () = 
   assert (test1 = true)

(* test for n_generated_values *)

let () =
   assert (test_p_2 () = true)

(* test for n_increasing_length_lists *)

let () =
   assert (test_p_3 10 = true);
   assert (test_p_3 100 = true);
   assert (test_p_3 45 = true);
   assert (test_results_5 = true)

(* test for n_random_length_lists *)

let () =
   assert (test_p_4 10 = true);
   assert (test_p_4 100 = true);
   assert (test_p_4 78 = true)

(* test for  test_results_even_odd *)

let () = 
   assert ( test_results_even_odd = true) 

(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)

let () =
print_endline ("Success! All tests passed.")


