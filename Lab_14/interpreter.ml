(* Simple interpreter based on Denotational Semantcs.  

   Eric Van Wyk
 *)

(* Eric Hwang *)

type value
  = Int of int
  | Bool of bool

type expr = 
  | Val of value
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr
  | Not of expr
  | And of expr * expr
  | Mod of expr * expr

type environment = (string * value) list

let rec lookup name env =
  match env with 
  | [ ] -> raise (Failure ("Name \"" ^ name ^ "\" not found."))
  | (k,v)::rest -> if name = k then v else lookup name rest

let rec eval (e: expr) (env: environment) : value = 
  match e with
  | Val v -> v
  | Add (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Sub (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (Failure "incompatible types, Sub") 
    )
  | Mul (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (Failure "incompatible types, Mul")
     )
  | Div (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 / v2)
       | _ -> raise (Failure "incompatible types, Div")
     )
  | Lt (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 < v2)
       | _ -> raise (Failure "incompatible types, Lt")
     )
  | Eq (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 = v2)
       | Bool v1, Bool v2 -> Bool (v1 = v2)
       | _ -> raise (Failure "incompatible types, Eq")
     )
  | And (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool v1, Bool v2 -> Bool (v1 && v2)
       | _ -> raise (Failure "incompatible types, And")
     )
  | Not e' ->
     ( match eval e' env with
       | Bool v -> Bool (not v)
       | _ -> raise (Failure "incompatible types, Not")
     )
  | Var n -> lookup n env

  | Mod (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 mod v2)
       | _ -> raise (Failure "incompatible types, Mod")      
)  

let rec read_number () =
  print_endline "Enter an integer value:" ;
  try int_of_string (read_line ()) with
  | Failure _ -> read_number ()

let write_number n = print_endline (string_of_int n)


type state = environment

type stmt =
  | Assign of string * expr
  | Seq of stmt * stmt
  | ReadNum of string
  | WriteNum of expr
  | IfThen of expr * stmt
  | While of expr * stmt
  | IfThenElse of expr * stmt * stmt
  | Skip  
  | For of string * expr * expr * stmt  

let rec exec (s: stmt) (stt: state) : state =
  match s with
  | Assign (nm, e) -> (nm, eval e stt) :: stt
  | Seq (s1, s2) -> let new_state = exec s1 stt in exec s2 new_state
  | ReadNum nm -> let v = read_number () 
                  in (nm, Int v) :: stt
  | WriteNum e -> (match eval e stt with
                   | Int v1 -> write_number v1; stt
                   | _ -> failwith "Oh no, incompatible types"
                  )
  | IfThen (cond, body) -> (match eval cond stt with
                            | Bool true -> exec body stt
                            | Bool false -> exec Skip stt
                            | _ -> failwith "Oh no, incompatible types"
                           )
  | While (cond, body) -> (match eval cond stt with
                           | Bool false -> stt
                           | Bool true -> exec (While (cond, body)) (exec body stt)
                           | _ -> failwith "Oh no, incompatible types"
                          )
  | IfThenElse (cond, st1, st2) -> (match eval cond stt with
                                    | Bool true -> exec st1 stt
                                    | Bool false -> exec st2 stt
                                    | _ -> failwith "oh no, incompatible types" 
                                   )

  | Skip -> stt

  | For (i, low, up, body) -> (match eval low stt, eval up stt with
                               | Int l, Int u -> if l <= u 
                                                 then  
                                                 exec (For (i, Add (low, Val (Int 1)), Val (Int u), body)) (exec body ((exec (Assign (i, low)) stt)) ) 
                                                 else stt
                               | _, _ -> failwith "oh no, incompatible types"	
 
                               )
                               

(* program_for *)
let program_for = For ("i", Val (Int 1), Val (Int 5), WriteNum (Var "i"))

let program_seq =
(* x := 1;
   y := x + 2;
   z := y * 3;
   write z
 *) 
  Seq (Assign ("x", Val (Int 1)),
  Seq (Assign ("y", Add (Var "x", Val (Int 2))),
  Seq (Assign ("z", Mul (Var "y", Val (Int 3))),
       WriteNum (Var "z")
      ) ) )

let program_while =
(* read x;
   i = 0;
   sum = 0;
   while (i < x) {
     write i;
     sum = sum + i;
     i = i + 1
   }
   write sum
 *)
  Seq (ReadNum "x",
  Seq (Assign ("i", Val (Int 0)),
  Seq (Assign ("sum", Val (Int 0)),
  Seq (While (Lt (Var "i", Var "x"),
	      Seq (WriteNum (Var "i"),
	      Seq (Assign ("sum", Add (Var "sum", Var "i")),
		   Assign ("i", Add (Var "i", Val (Int 1)))
	          ) ) ),
        WriteNum (Var "sum")
      ) ) ) )

(* program_while_ifthenelse *)
let program_while_ifthenelse =
(* read x;
   i = 0;
   sum_evens = 0;
   sum_odds = 0;
   while (i < x) {
     write i;
     if i mod 2 = 0 then
        sum_evens = sum_evens + i;
     else
        sum_odds = sum_odds + i;
     i = i + 1
   }
   write sum_evens;
   write sum_odds
 *)

  Seq (ReadNum "x",
   Seq (Assign ("i", Val (Int 0)),
    Seq (Assign ("sum_evens", Val (Int 0)),
     Seq (Assign ("sum_odds", Val (Int 0)),
      Seq ( While (Lt (Var "i", Var "x"),
               Seq (WriteNum (Var "i"),
                Seq (IfThenElse (Eq ( (Mod (Var "i", Val (Int 2)), Val (Int 0))), 
                     Assign ("sum_evens", Add (Var "sum_evens", Var "i")),
                     Assign ("sum_odds", Add (Var "sum_odds", Var "i"))),
                Assign ("i", Add (Var "i", Val (Int 1)))
         
         ) ) ),
        Seq (WriteNum (Var "sum_evens"), WriteNum (Var "sum_odds"))
      ) ) ) ) ) 

let val_sum_evens = 56
let val_sum_odds = 49
let num_sum_evens = 9
let num_sum_odds = 8
(* program_while_ifthenelse_test *)
let program_while_ifthenelse_test =
  Seq (Assign ("x", Val (Int 12)),
   Seq (Assign ("i", Val (Int 0)),
    Seq (Assign ("sum_evens", Val (Int 0)),
     Seq (Assign ("sum_odds", Val (Int 0)),
      Seq ( While (Lt (Var "i", Var "x"),
               Seq (WriteNum (Var "i"),
                Seq (IfThenElse (Eq ( (Mod (Var "i", Val (Int 2)), Val (Int 0))),
                     Assign ("sum_evens", Add (Var "sum_evens", Var "i")),
                     Assign ("sum_odds", Add (Var "sum_odds", Var "i"))),
                Assign ("i", Add (Var "i", Val (Int 1)))

         ) ) ),
        Seq (WriteNum (Var "sum_evens"), WriteNum (Var "sum_odds"))
      ) ) ) ) )

(* program_ifthen *)
let program_ifthen =
  (* y = 0;
   if x mod 2 = 0 then y = y + 2;
   if x mod 3 = 0 then y = y + 3;
   if x mod 4 = 0 then y = y + 4;
  *)

  Seq ( Assign ( "y", Val (Int 0)),
   Seq ( IfThen ( Eq ( Mod ((Var "x"), Val (Int 2)), Val (Int 0)), 
         Assign ( "y", Add (Var "y", Val (Int 2)))),
      Seq ( IfThen ( Eq ( Mod ((Var "x"), Val (Int 3)), Val (Int 0)), 
          Assign ( "y", Add (Var "y", Val (Int 3)))),
         (IfThen ( Eq ( Mod ((Var "x"), Val (Int 4)), Val (Int 0)), 
           Assign ( "y", Add (Var "y", Val (Int 4))))
  ) ) ) ) 

(* program_sum_10 *)
let program_sum_10 =
(* sum = 0
   for i = 1 to 10
     sum = sum + i
   write sum
 *)
  Seq (Assign ("sum", Val (Int 0)),
  Seq (For ("i", Val (Int 1), Val (Int 10),
            Assign("sum", Add (Var "sum", Var "i"))),
       WriteNum (Var "sum")
      ) )

(* program_sum_10_n *)
let program_sum_10_n =
(* n = 10
   sum = 0
   for i = 1 to n
     sum = sum + i
     n = sum
   write sum
   write n  
 *)
  Seq (Assign ("n", Val (Int 10)),
  Seq (Assign ("sum", Val (Int 0)),
  Seq (For ("i", Val (Int 1), Var "n",
            Seq (Assign("sum", Add (Var "sum", Var "i")),
                 Assign("n", Var "sum")
                )
           ),
  Seq(WriteNum (Var "sum"),
      WriteNum (Var "n")
     ) ) ) )
