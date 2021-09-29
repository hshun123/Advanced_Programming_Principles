(* Eric Hwang Hw2*)

(* Assistant function *)
let read_file (file_name: string) : char list =
  let ic = open_in file_name 
  in 
  let rec read_chars ic =
    (* `input_char` raises an exception when it attempts to read
       past the end of the file.  This is caught to terminate
       the `read_chars` function. *)
    try 
      let next_char = input_char ic
      in next_char :: read_chars ic
    with 
      _ -> [] 
  in read_chars ic

(* Assistant function *)
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

(* Assistant function *)
let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

(* Assistant function *)
let rec lookup_all s m =
  match m with
  | [] -> []
  | (name,value)::ms -> 
     let rest = lookup_all s ms
in if s = name then value :: rest else rest

(* is_elem function check if the element is in the list. 
This is a helper function for split function. *)

let is_elem elm lst = List.fold_left (fun a b -> b = elm||a) false lst

(* sum function add all the value is the list. 
This is the helper function for transform6 *)

let sum lst = List.fold_left (fun a b -> a + 1)0 lst

(* split function used in transform1. *)

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

(* transform functions *)

(* transform1 function. Seperators: [' '; '\t'; '\n'; ';'; ':'; '.'; '?'; '!']*)

let transform1 (st1: char list) : char list list = split [' '; '\t'; '\n'; ';'; ':'; '.'; '?'; '!'] st1

(* transfrom2 function *)

let transform2 (st2: char list list) : string list = List.map(implode)(st2)

(* transform3 function *)

let transform3 (st3: string list) : string list =
    List.fold_right (fun a b -> if a = "" then b else a::b) st3 []

(* transform4 function *)

let transform4 (st4:string list) : (string * int) list = List.fold_right (fun a b -> (a,1)::b)st4 []

(* transform5 function *)

let transform5 (st5: (string * int) list) : (string * int list) list =
    List.rev (List.fold_right (fun t r -> if is_elem t r then r else t::r) ((List.fold_right (fun (a,c) b -> (a,lookup_all a st5)::b ))st5 []) [])

(* transform6 function *)

let transform6 (st6: (string * int list) list) : (string * int) list = List.fold_right (fun (n,vl) b -> (n,sum vl) :: b)st6 []

(* word_count funciton *)

let word_count (fn: string) : (string * int) list =
  let step1: char list = read_file fn in
  let step2: char list list = transform1 step1 in
  let step3: string list = transform2 step2 in
  let step4: string list = transform3 step3 in
  let step5: (string * int) list = transform4 step4 in
  let step6: (string * int list) list = transform5 step5 in
  let step7: (string * int) list = transform6 step6 in
  step7;;

(* Tests *)
(*
let demo_count = word_count "../../public-class-repo/Homeworks/Files/demo.txt"

let test1 = lookup_all "new" demo_count = [2]
let test2 = lookup_all "the" demo_count = [5]
let test3 = lookup_all "to" demo_count = [2]
let test4 = lookup_all "punctuation" demo_count = [3]
*)
