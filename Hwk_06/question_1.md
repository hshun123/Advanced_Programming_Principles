(* Eric Hwang *)
(* hwang241 *)

## CALL BY VALUE

```
  prod (take 3 (odds_from 5 0))
= prod (take 3 ((0 + 0 + 1) :: odds_from (5 - 1)(0 + 1))
= prod (take 3 (1 :: odds_from 4 1))
= prod (take 3 (1 :: ((1 + 1 + 1) :: odds_from (4 - 1)(1 + 1))))
= prod (take 3 (1 :: (3 :: odds_from 3 2)))
= prod (take 3 (1 :: (3 :: ((2 + 2 + 1) :: odds_from (3 - 1)(2 + 1)))))
= prod (take 3 (1 :: (3 :: (5 :: (odds_from 2 3)))))
= prod (take 3 (1 :: (3 :: (5 :: ((3 + 3 + 1) :: (odds_from (2 - 1) (3 + 1)))))))
= prod (take 3 (1 :: (3 :: (5 :: (7 :: (odds_from 1 4))))))
= prod (take 3 (1 :: (3 :: (5 :: (7 :: ((4 + 4 + 1) :: (odds_from (1 - 1) (4 + 1))))))))
= prod (take 3 (1 :: (3 :: (5 :: (7 :: (9 :: (odds_from 0 5)))))))
= prod (take 3 (1 :: (3 :: (5 :: (7 :: (9 :: []))))))
= prod (1 :: take (3 - 1) ((3 :: (5 :: (7 :: (9 :: []))))))
= prod (1 :: take 2 ((3 :: (5 :: (7 :: (9 :: [])))))))
= prod (1 :: (3 :: take (2 - 1) (5 :: (7 :: (9 :: [])))))
= prod (1 :: (3 :: take 1 (5 :: (7 :: (9 :: [])))))
= prod (1 :: (3 :: (5 :: take (1 - 1) (7 :: (9 :: [])))))
= prod (1 :: (3 :: (5 :: take 0 (7 :: (9 :: []))))
= prod (1 :: (3:: (5 :: [])))
= 1 * prod (3 :: (5 :: [])))
= 1 * (3 * prod (5 :: [])))
= 1 * (3 * (5 * prod []))
= 1 * (3 * (5 * (1)))
= 15
```

## CALL BY NAME
```
  prod (take 3 (odds_from 5 0))
= prod (take 3 ((0 + 0 + 1) :: odds_from (5 - 1) (0 + 1))))
= prod ((0 + 0 + 1) :: (take (3 - 1) (odds_from (5 - 1) (0 + 1)))
= 1 * (prod (take (3 - 1) (odds_from (5 - 1) (0 + 1))))
= 1 * (prod (take (3 - 1) (odds_from 4 (0 + 1)))
= 1 * (prod (take (3 - 1) (((0 + 1) + (0 + 1) + 1) :: odds_from (4 - 1) ((0 + 1) + 1)))
= 1 * (prod (take 2 (((0 + 1) + (0 + 1) + 1) :: odds_from (4 - 1) ((0 + 1) + 1)))
= 1 * (prod (((0 + 1) + (0 + 1) + 1) :: take (2 - 1) (odds_from (4 - 1) ((0 + 1) + 1)))
= 1 * ((0 + 1) + (0 + 1) + 1) * (prod (take (2 - 1) (odds_from (4 - 1) ((0 + 1) + 1))))
= 1 * 3 * (prod (take (2 - 1) (odds_from (4 - 1) ((0 + 1) + 1))))
= 1 * 3 * (prod (take (2 - 1) (odds_from 3 ((0 + 1) + 1))))
= 1 * 3 * (prod (take (2 - 1) ((((0 + 1) + 1) + ((0 + 1) + 1) + 1)
             :: odds_from (3 - 1) (((0 + 1) + 1) + 1))))
= 1 * 3 * (prod (take 1 ((((0 + 1) + 1) + ((0 + 1) + 1) + 1)
             :: odds_from (3 - 1) (((0 + 1) + 1) + 1))))
= 1 * 3 * (prod ((((0 + 1) + 1) + ((0 + 1) + 1) + 1)
             :: take (1 - 1) (odds_from (3 - 1) (((0 + 1) + 1) + 1))))
= 1 * 3 * (((0 + 1) + 1) + ((0 + 1) + 1) + 1)
             * (prod (take (1 - 1) (odds_from (3 - 1) (((0 + 1) + 1) + 1))))
= 1 * 3 * 5 * (prod (take (1 - 1) (odds_from (3 - 1) (((0 + 1) + 1) + 1))))
= 1 * 3 * 5 * (prod (take (1 - 1) (odds_from 2 ((((0 + 1) + 1) + 1))))
= 1 * 3 * 5 * (prod (take (1 - 1) (((((0 + 1) + 1) + 1) + (((0 + 1) + 1) + 1) + 1)
             :: odds_from (2 - 1) (((0 + 1) + 1) + 1) + 1)))
= 1 * 3 * 5 * (prod (take 0 (((((0 + 1) + 1) + 1) + (((0 + 1) + 1) + 1) + 1)
             :: odds_from (2 - 1) (((0 + 1) + 1) + 1) + 1)))))
= 1 * 3 * 5 * (prod [])
= 1 * 3 * 5 * 1
= 15
```
## CALL BY NEED
```
  prod (take 3 (odds_from 5 0))
= prod (take 3 ((0 + 0 + 1) :: odds_from (5 - 1) (0 + 1))))
= prod ((0 + 0 + 1) :: (take (3 - 1) (odds_from (5 - 1) (0 + 1)))
= 1 * (prod (take (3 - 1) (odds_from (5 - 1) (0 + 1))))
= 1 * (prod (take (3 - 1) (odds_from 4 (0 + 1)))
= 1 * (prod (take (3 - 1) ((v + v + 1) :: odds_from (4 - 1) (v + 1)))
  where v = 0 + 1
= 1 * (prod (take 2 ((v + v + 1)) :: odds_from (4 - 1) (v + 1)))
  where v = 0 + 1
= 1 * (prod ((v + v + 1) :: take (2 - 1) (odds_from (4 - 1) (v + 1))))
  where v = 0 + 1
= 1 * (v + v + 1) * (prod (take (2 - 1) (odds_from (4 - 1) (v + 1)))
  where v = 0 + 1
= 1 * 3 * (prod (take (2 - 1) (odds_from (4 - 1) (1 + 1)))
= 1 * 3 * (prod (take (2 - 1) (odds_from 3 (1 + 1))))
= 1 * 3 * (prod (take (2 - 1) ((v + v + 1) :: odds_from (3 - 1) (v + 1))))
  where v = 1 + 1
= 1 * 3 * (prod (take 1 ((v + v + 1) :: odds_from (3 - 1) (v + 1))))
  where v = 1 + 1 
= 1 * 3 * (prod ((v + v + 1) :: take (1 - 1) (odds_from (3 - 1) (v + 1))))
  where v = 1 + 1 
= 1 * 3 * (v + v + 1) * (prod (take (1 - 1) (odds _from (3 - 1) (v + 1))))
  where v = 1 + 1
= 1 * 3 * 5 * (prod (take (1 - 1) (odds_from (3 - 1) (2 + 1))))
= 1 * 3 * 5 * (prod (take (1 - 1) (odds_from 2 (2 + 1))))
= 1 * 3 * 5 * (prod (take (1 - 1) ((v + v + 1) :: odds_from (2 - 1) (v + 1))))
  where v = 2 + 1
= 1 * 3 * 5 * (prod (take 0 ((v + v + 1) :: odds_from (2 - 1) (v + 1))))
  where v = 2 + 1
= 1 * 3 * 5 * (prod ([]))
= 1 * 3 * 5 * 1
= 15
```
