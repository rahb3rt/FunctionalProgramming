bigWords :: Foldable t => [t a] -> [t a]
bigWords [] = []
bigWords (x:xs)
    | length x >= 8 = x : y
    | length x  < 8 = y
    | otherwise = []
    where  y = bigWords xs

range :: (Num a, Ord a) => a -> a -> [a]  
range a b
    | a < b = a : range (a + 1) b
    | a > b = b : range a (b + 1)
    | a == b = return(a)
    | otherwise = []

exclaim :: [Char] -> [Char]
exclaim [] = "!"
exclaim (x:xs)
    | elem x " " = "! " ++ exclaim xs
    | otherwise = x: exclaim xs

findInt x 
    | x /= 'a' = add:findInt (pred x)
    | x == 'a' = add:[]
    where   count = 0
            add = count + 1;

numerology :: Num a => Char -> a
numerology x = sum (findInt x)

lengthOfList :: Foldable t => [t a] -> [Int]
lengthOfList x = map (length) x 


