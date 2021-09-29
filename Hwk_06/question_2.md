(* Eric Hwang *)
(* hwang241 *)

## CALL BY VALUE

# orl
```
 orl (false :: false ::true :: false :: [])
= foldl or false (false :: false ::true :: false :: [])
= foldl or (or false false) (false :: true :: false :: [])
= foldl or false (false :: true :: false :: [])
= foldl or (or false false) (true :: false :: [])
= foldl or false (true :: false :: [])
= foldl or (or false true) (false :: [])
= foldl or true (false :: [])
= foldl or (or true false) ([])
= foldl or true []
= true
```
# orr
```
 orr (false :: false :: true :: false :: [])
= foldr or (false :: false :: true :: false :: []) false
= or false (foldr or (false :: true :: false :: []) false)
= or false (or false (foldr or (true :: false :: []) false))
= or false (or false (or true (foldr or (false :: []) false)))
= or false (or false (or true (or false (foldr or ([]) false)))) 
= or false (or false (or true (or false (false))))
= or false (or false (or true (false)))
= or false (or false (true))
= or false (true)
= true
```

## CALL BY NAME
# orl
```
 orl (false :: false ::true :: false :: [])
= foldl or false (false :: false ::true :: false :: [])
= foldl or (or false false) (false :: true :: false :: [])
= foldl or (or (or false false) false) (true :: false :: [])
= foldl or (or (or (or false false) false) true) (false :: [])
= foldl or (or (or (or (or false false) false) true) false) []
= (or (or (or (or false false) false) true) false)
= (or (or (or false false) true) false)
= (or (or false true) false)
= or true false
= true
```
# orr
```
 orr (false :: false ::true :: false :: [])
= foldr or (false :: false :: true :: false :: []) false
= or false (foldr or (false :: true :: false :: []) false)
= foldr or (false ::true :: false :: []) false
= or false (foldr or (true :: false :: []) false)
= foldr or (true :: false :: []) false
= or true (foldr or (false :: []) false)
= true 
```
## CALL BY NEED

# orl
```
 orl (false :: false ::true :: false :: [])
= foldl or false (false :: false ::true :: false :: [])
= foldl or (or false false) (false :: true :: false :: [])
= foldl or (or v false) (true :: false :: [])
  where v = or false false
= foldl or (or (or v false) true) (false :: [])
  where v = or false false
= foldl or (or (or (or v false) true) false) []
  where v = or false false
= or (or (or v false) true) false)
  where v = or false false
= or (or (or false false) true) false
= (or (or v true) false)
  where v = or false false
= or (or false true) false
= or v false
  where v = or false true
= or true false
= true
```
# orr
```
 orr (false :: false ::true :: false :: [])
= foldr or (false :: false :: true :: false :: []) false
= or false (foldr or (false :: true :: false :: []) false)
= foldr or (false ::true :: false :: []) false
= or false (foldr or (true :: false :: []) false)
= foldr or (true :: false :: []) false
= or true (foldr or (false :: []) false)
= true
 ```
