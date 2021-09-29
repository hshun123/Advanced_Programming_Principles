(* Eric Hwang*)

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

(* length function returns the length of the list *)

let length xs = List.fold_left (fun a _ -> a + 1) 0 xs

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

(* extract length 6 function extract 6 letter words *)

let extrc (chalst: 'a list list) : 'a list list = List.filter (fun a -> length a = 6 ) chalst

(* extract length 8*)

let extrc_8 (chalst: 'a list list) : 'a list list = List.filter (fun a -> length a = 8 ) chalst

(* make 8 to 6 string returns 6 letter word after removing the middle 2 letters from 8 letter word *)

let make_8_to_6 (str: string) : string = String.sub str 0 3 ^ String.sub str 5 3

(* transform1 from part 1*)

let transform1 (st1: char list) : char list list = split [ ' '; ','; ';'; '.'; '!'; '\n' ] st1

(* transform2 from part 1*)

let transform2 (st2: char list list) : string list = List.map(implode)(st2)

(* answers function *)
let answers (fn: string) : string list =
  let step1: char list = read_file fn in
  let step2: char list list = transform1 step1 in
  let len6: char list list = extrc step2 in
  let len8: char list list = extrc_8 step2 in
  let str6: string list = transform2 len6 in
  let str8: string list = transform2 len8 in
  let find: string list = List.fold_right(fun a b -> if is_elem (make_8_to_6 a) str6 then a::b else b)str8 [] in
  find



(*Sameple directory *)

let d1 = "../../public-class-repo/Homeworks/Files/words-small.txt"
let d2 = "../../public-class-repo/Homeworks/Files/words-google-10000.txt"

(* pretty answers *)
let pretty_answers (strlst: string list) : (string * string)list = List.map(fun a -> (make_8_to_6 a, a))strlst;;

