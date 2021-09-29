# Homework 4: Reasoning about Correctness.

by Eric Hwang

**Problem 1**

```
let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys
```
Using the definition above, show by induction that

prod (l1 @ l2) = prod l1 * prod l2

```
P(l1): prod (l1 @ l2) = prod l1 * prod l2
```
```
Base case: P([])
Show: prod ([] @ l2) = prod [] * prod l2

  prod ([] @ l2)
= prod (l2), by property of @
= 1 * prod (l2), by property of multiplication *
= prod [] * prod l2, by def. of prod
```
```
Inductive Case: P(y :: ys)
Show: prod ((y :: ys) @ l2) = prod (y :: ys) * prod l2
Given: prod (ys @ l2) = prod ys * prod l2

  prod ((y :: ys) @ l2)
= prod (y :: (ys @ l2)), by properties of :: and @
= y * (prod (ys @ l2)), by definition of prod
= y * (prod ys * prod l2), by inductive hypothesis
= (y * prod ys) * prod l2, by associativity of multiplication *
= prod (y :: ys) * prod l2, by definition of prod
```
```
The Principle of Induction:
                           P([]) holds (base case)
                           p(ys) -> P(y::ys) holds (inductive case)
```

**Problem 2**

```
let rec sum = function
  | [] -> 0
  | y::ys -> y + sum ys

let rec length = function
  | [] -> 0
  | y::ys -> 1 + length ys

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys
```
Using the definitions above, show by induction that

sum (inc_all l) = length l + sum l

```
P(l): sum (inc_all l) = length l + sum l
```
```
Base Case: P([])
Show: sum (inc_all []) = length [] + sum []

  sum (inc_all [])
= sum [], by definition of inc_all
= 0 + sum [], by properties of addition and zero
= length [] + sum [], by definition of length
```
```
Inductive Case: P(y :: ys)
Show: sum (inc_all (y :: ys)) = length (y :: ys) + sum (y :: ys)
Given: sum (inc_all ys) = length ys + sum ys

  sum (inc_all (y :: ys))
= sum ((y + 1) :: inc_all ys), by definition of inc_all
= (y + 1) + sum (inc_all ys), by definitin of sum
= (y + 1) + (length ys + sum ys), by inductive hypothesis
= (1 + length ys) + (y + sum ys) , by associativity and Commutativity of addition +
= length (y :: ys) + sum (y :: ys), by definition of sum and length
```
```
The Principle of Induction over list:
                           P([]) holds (base case)
                           p(ys) -> P(y::ys) holds (inductive case)
```
**Problem 3**

```
let rec map f l = match l with
  | [] -> []
  | y::ys -> f y :: map f ys

let inc x = x + 1
```

Using the definitions above, show by induction that

map inc l = inc_all l

```
P(l): map inc l = inc_all l
```
```
Base Case: P([])
Show: map inc [] = inc_all []

  map inc []
= [], by definition of map
= inc_all [], by definition of inc_all
```
```
Inductive Case: P(y :: ys)
Show: map inc (y :: ys) = inc_all (y :: ys)
Given: map inc ys = inc_all ys

  map inc (y :: ys)
= inc y :: map inc ys, by definition of map
= inc y :: (inc_all ys), by inductive hypothesis
= (y + 1) :: (inc_all ys), by definition of inc
= inc_all (y :: ys), by definition of inc_all
```
```
The Principle of Induction over list: 
                                     P([]) holds (base case)
                                     p(ys) -> P(y::ys) holds (inductive case) 
```

**Problem 4**

```
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

let rec to_list (t: 'a tree) : 'a list = match t with
  | Empty -> []
  | Node (v, tl, tr) -> to_list tl @ [v] @ to_list tr

let rec product (mytree: int tree) : int =
  match mytree with
  |Empty -> 1
  |Node(v, lt,rt) -> v * product lt * product rt
```
```
P(t): prod (to_list t) = product t
```
```
Base Case: P(Empty)
Show: prod (to_list Empty) = product Empty

  prod (to_list Empty)
= prod ([]), by definition of to_list
= 1, by definition of prod
= product Empty, by definition of product
```
```
Inductive Case: P(Node(v, lt, rt))
Show: prod (to_list (Node (v, lt, rt))) = product (Node (v, lt, rt))
Given: prod (to_list lt) = product lt and prod (to_list rt) = product rt

  prod (to_list (Node (v, lt, rt)))
= prod (to_list lt @ [v] @ to_list rt), by definition of to_list
= prod (to_list lt @ ([v] @ to_list rt)), by associativity of @
= prod (to_list lt) * prod ([v] @ to_list rt), by property of prod proved in Problem 1
= prod (to_list lt) * (prod (v :: to_list rt), by properties of @ and :: and list
= product lt * (prod (v :: to_list rt), by inductive hypothesis
= product lt * (v * prod (to_list rt), by definition of prod
= product lt * v * prod (to_list rt), by associativity of multiplication *
= product lt * v * product rt, by inductive hypothesis
= v * product lt * product rt, by Commutative property of multiplication *
= product (Node (v, lt, rt)), by definition of product
```
```
The principle of induction over tree:
                                     P(Empty) holds (base case)
                                     P(lt) and P(rt) → P(Node(v, lt, rt)) holds (inductive case)
```

**Problem 5**

```
(*size*)
let rec size (mytree: 'a tree) : int =
  match mytree with
  |Empty -> 0
  |Node (v,lt,rt) -> 1 + size lt + size rt

(*size_r*)
let size_r (mytree: 'a tree) : int = 
  reduce mytree 0 (fun v lt rt -> 1 + lt + rt)

(*reduce function*)
let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
  match t with
  | Empty -> b
  | Node (v, tl, tr) -> f v (reduce tl b f) (reduce tr b f)
```
```
P(t): size t = size_r t
```
```
Base Case: P(Empty)
Show: size Empty = size_r Empty

  size Empty 
= 0, by definition of size
= reduce Empty 0 (fun v lt rt -> 1 + lt + rt), by definition of reduce
= size_r Empty, by definition of size_r
```
```
Inductive Case: P (Node (v, lt, rt))
Show: size (Node (v, lt, rt)) = size_r (Node (v, lt, rt))
Given: size lt = size_r and size rt = size_r rt

  size (Node (v, lt, rt))
= 1 + size lt + size rt, by definition of size
= 1 + sizse_r lt + size_r rt, by inductive hypothesis
= 1 + (reduce lt 0 (fun v lt rt -> 1 + lt + rt)) + (reduce rt 0 (fun v lt rt -> 1 + lt + rt)), by definition of size_r
= (fun v lt rt -> 1 + lt + rt) v (reduce lt 0 (fun v lt rt -> 1 + lt + rt)) (reduce rt 0 (fun v lt rt -> 1 + lt + rt)), by definition of function:(fun v lt rt -> 1 + lt + rt) which uses in reduce
= reduce (Node (v, lt, rt)) 0 (fun v lt rt -> 1 + lt + rt), by definition of reduce
= size_r (Node (v, lt, rt)), by definition of size_r
```
```
The principle of induction over tree:
                                    P(Empty) holds (base case)
                                    P(lt) and P(rt) → P(Node(v, lt, rt)) holds (inductive case)
```
