(* Eric Hwang Lab_06 working with YongMin Jung*)

(* Removed the previous helper functions: extrc, extrc_8. And, added new functions: find_6_8, find_answer. 
In previous version of code, I extracted  6 letter words and 8 letter words independently. 
Then found the answer from 8 letter words list, then construct the answer list.
However, in this new code, the whole processes are done in just two functions find_6_8 and find_answer. *)

(* function for reading file*)

let read_file (file_name: string) : char list =
  let ic = open_in file_name 
  in 
  let rec read_chars ic =
    try 
      let next_char = input_char ic
      in next_char :: read_chars ic
    with 
      _ -> [] 
  in read_chars ic

(* implode function *)

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

(* is_element returns true is the element is in the list *)

let is_elem elm lst = List.fold_left (fun a b -> b = elm||a) false lst

(* split function*)

let split (sepelst: 'a list) (xs: 'a list): 'a list list =
  let accum : 'a list * 'a list list = ([], [])
  in
  let f (elems, groups) x =
       if  is_elem x sepelst
       then ( [], List.rev (elems) :: groups )
       else ( x::elems, groups)
  in
  match List.fold_left f accum xs with
  | (leftovers, groups) -> List.rev ( List.rev leftovers :: groups)

(* make 8 to 6 string returns 6 letter word after removing the middle 2 letters from 8 letter word *)

let make_8_to_6 (str: string) : string = String.sub str 0 3 ^ String.sub str 5 3

(* transform1 from part 1*)

let transform1 (st1: char list) : char list list = split   [' '; '\t'; '\n'; ';'; ':'; '.'; '?'; '!'] st1

(* transform2 from part 1*)

let transform2 (st2: char list list) : string list = List.map(implode)(st2)

(* This funciton returns 6 letter words and 8 letter words list. Code idea from YongMin Jung *)

let find_6_8 (lst: string list) : string list = List.filter (fun a -> String.length a = 6 || String.length a = 8) lst

(* This function returns the answer. Code idea from YongMin Jung *)

let find_answer (lst: string list) : string list = List.filter (fun a -> String.length a > 7 && is_elem ((String.sub a 0 3) ^ (String.sub a 5 3)) lst) lst

(* answers function. Changed the name of each function to improve readability *)

let answers (fn: string) : string list =
    let step1 = read_file fn in
    let step2 = transform1 step1 in
    let step3 = transform2 step2 in
    let step4 = find_6_8 step3 in
    let step5 = find_answer step4 in
    step5

(*Sameple directory *)

let d1 = "../../public-class-repo/Homeworks/Files/words-small.txt"
let d2 = "../../public-class-repo/Homeworks/Files/words-google-10000.txt"

(* pretty answers *)
let pretty_answers (strlst: string list) : (string * string)list = List.map(fun a -> (make_8_to_6 a, a))strlst;;

