(* Eric Hwang *)

type formula = And  of formula * formula
             | Or of formula * formula
             | Implies of formula * formula
             | Not of formula 
             | Prop of string
             | Bool of bool

type subst = (string * bool) list

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair

let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let rec explode = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let dedup lst =
  let f elem to_keep =
    if is_elem elem to_keep then to_keep else elem::to_keep
  in List.fold_right f lst []

(* lookup function used in eval *)
let rec lookup (n: string) (sbt: subst) : bool =
  match sbt with
  | [] -> raise (Failure ("Name \"" ^ n ^ "\" not in scope"))
  | (n',v)::_ when n' = n -> v
  | _::rest -> lookup n rest


(* eval function *)
let rec eval (fml: formula) (sbt: subst): bool =
  match fml with
   | Bool b -> b

   | And (f1, f2) -> (eval f1 sbt) && (eval f2 sbt)

    | Or (f1,f2) -> (eval f1 sbt) || (eval f2 sbt)

    | Implies (f1, f2) -> if (eval f1 sbt) = true && (eval f2 sbt) = false 
                          then false 
                          else true

    | Not f1 -> not (eval f1 sbt)

    | Prop s -> lookup s sbt
          
(*  freevars function *)         
let rec freevars (fml: formula): string list =
  let rec help (f: formula): string list =
    match f with
    | Bool b -> []

    | And (f1, f2)
    | Or (f1, f2)
    | Implies (f1, f2)-> freevars f1 @ freevars f2

    | Not f1 -> freevars f1

    | Prop s -> [s]
  
  in dedup (help fml)


(* Version 1 *)
exception KeepLooking


let is_tautology_v1 (fml: formula) (func: (subst -> subst option)): subst option =
  let rec try_subset check_sbt rest_sbt =
    match rest_sbt with
    | [] -> (match eval fml check_sbt with
             | true -> raise KeepLooking
             | false -> func check_sbt
            )
    | x::xs -> try try_subset ((x, true) :: check_sbt) xs with
		    | KeepLooking -> try_subset  ((x, false) :: check_sbt) xs

  in try (try_subset [] (freevars fml)) with
     | KeepLooking -> None


(* given functions *)
let is_tautology_v1_first f = is_tautology_v1 f (fun s -> Some s)

let is_tautology_v1_print_all f =
  is_tautology_v1
    f
    (fun s -> print_endline (show_subst s); 
	      raise KeepLooking)

(* Version 2 *)
exception FoundCounterExample of subst


let is_tautology_v2 (fml: formula) (func: (subst -> unit)) : subst option =
  let rec try_subset check_sbt rest_sbt =
    if check_sbt <> [] &&
       rest_sbt = [] &&
       eval fml check_sbt = false
    then
      func check_sbt
    else (match rest_sbt with
          | [] -> ()
          | x::xs -> try_subset ((x, true) :: check_sbt) xs; 
                      try_subset ((x, false) :: check_sbt) xs
         )
  in try (try_subset [] (freevars fml)) ; None with
     | FoundCounterExample result -> Some result
     | Failure msg -> print_endline msg; None


(* given functions *)
let is_tautology_v2_first f = is_tautology_v2 f (fun s -> raise (FoundCounterExample s))

let is_tautology_v2_print_all f =
  is_tautology_v2
    f
    (fun s -> print_endline (show_subst s))
