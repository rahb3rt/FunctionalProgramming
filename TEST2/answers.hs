-- Question 1
firstQ :: [Int] -> [Int]
firstQ l = filter f l
 where f x = x^2 - x > 100
 
-- Question 2
exclam :: String -> String
exclam "" = ""
exclam (c:cs) 
 | elem c "aeiou" = c : ("!" ++ exclam cs)
 | otherwise      = c : exclam cs

-- Question 3
omitter :: String -> String
omitter "" = ""
omitter (c:cs) = c : omitter (tail cs)

--Or like this:
--omitter "" = ""
--omitter (a:"") = a:""
--omitter (a:b:cs) = a : omitter cs

{- Question 4
Part a:
This can do a few steps like so:

f [1,2,3,4,5] = 1 + 2 - (f [3,4,5])
              = 1 + 2 - (3 + 4 - f [5])

And now you get "nonexhaustive patterns" since [5] doesn't fit either of the patterns for f.

Part b:
in f [1,2,3,4] you get lucky and never have one element left over by itself. It looks like:
f [1,2,3,4,5] = 1 + 2 - (f [3,4,5])
              = 1 + 2 - (3 + 4 - f [])
              = 1 + 2 - (3 + 4 - 4)
              = 0
-}

{-
Question 5

Part a:
(Int -> Int) -> Int

Part b:
[Int] -> (Int -> Int) -> [Int]

Part c:
String -> Int -> Int -> String

Part d:
[Int -> Int] -> Int -> Int -> Int
-}

--Question 6
data Sgn = Pos | Neg | Zero

sign :: Int -> Sgn
sign x 
 | x > 0  = Pos
 | x == 0 = Zero
 | x < 0  = Neg

opposite :: Sgn -> Sgn
opposite Pos  = Neg
opposite Neg  = Pos
opposite Zero = Zero

mult :: Sgn -> Sgn -> Sgn
mult Pos Pos = Pos
mult Pos Neg = Neg
mult Neg Pos = Neg
mult Neg Neg = Pos
mult Zero _  = Zero
mult _ Zero  = Zero

--Question 7
data Species = Human | Alien | Monster | Robot deriving Show

data Character = Guy String String Species deriving Show
-- "Guy" is the name of the constructor, which I made up. 
-- You can choose that to be anything you like.

orko = Guy "Orko" "He Man" Human

showName :: Character -> String
showName (Guy _ s _) = s

bothNames :: Character -> Character -> String
bothNames (Guy s _ _) (Guy t _ _) = s ++ " and " ++ t

{-
Question 8
part a:
map (^2) $ filter even [1..10]
 = map (^2) (filter even [1..10])
 = map (^2) [2,4,6,8,10]
 = [2^2, 4^2, 6^2, 8^2, 10^2]
 = [4, 16, 36, 64, 100]

part b:
foldr drop [1..] [3,5,2]
 = drop 3 (drop 5 (drop 2 [1..]))
 = drop 3 (drop 5 [3..])
 = drop 3 [8..]
 = [11..]

part c:
f 2 1 = f 1 1          + f 0 2
      = f 0 1 + f 0 1  + 6
      = 6     + 6      + 6
      = 36
-}
 