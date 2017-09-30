f :: (Ord a, Num a) => a -> [Char]
f x
    | x > 73 = "Too big!"
    | x < 73 = "Too small!"
    | x == 73 = "Right!" 

bro :: [Char] -> [Char]
bro x 
    | head x < 'g' = "Dude!"
    | head x >= 'h' || head x <= 'r' = "Sweet!"
    | otherwise = "Bummer!"

ekzer :: [Char] -> [Char] 
ekzer (a:as) = a : "x" ++ as

spooner :: ([Char], [Char]) -> ([Char], [Char])
spooner ([], []) = ("","")
spooner (x:xs,y:ys) = (x:ys,y:xs)
spooner ([],y:ys) = ([y],ys)
spooner (x:xs,[]) = (xs, [x])

quadrant :: (Ord a1, Ord a2, Num a1, Num a2, Num p) => (a1, a2) -> p
quadrant (x, y) 
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = 0

stringYear :: (Eq a, Num a) => (a, [Char]) -> [Char]
stringYear (x, y) 
    | x == 1  = y ++ " is a freshman"
    | x == 2  = y ++ " is a sophomore"
    | x == 3  = y ++ " is a junior"
    | x == 4  = y ++ " is a senior"
    | otherwise = y ++ " is a graduate"

match :: Eq a => [a] -> Bool
match y
    | length y > 1  = (y !! 0 == y !! 1)
    | length y == 1 = (y !! 0) == (y !! 0)
    | length y == 0 = (length y > 0) 

aLover :: [Char] -> [Char]
aLover (x:xs)
    | elem x "aeiou" = 'a':replace  
    | elem x "AEIOU" = 'A':replace
    | xs /= "" = x:replace
    | otherwise = x:""
    where replace = aLover xs

isPrime :: Integral a => a -> Bool
divs n = [ x | x <- [1..n], mod n x == 0]
isPrime x
    | x <= 1 = (length(divs x) /= 2)
    | x > 0 = (length(divs x) == 2)

isComposite :: Integral a => a -> Bool
isComposite x
    | x <= 1 = (length(divs x) /= 2)
    | x > 1 = (length(divs x) >= 2)


