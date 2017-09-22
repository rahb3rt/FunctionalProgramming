divs n = [ x | x <- [1..n], mod n x == 0]
sixDivs = [ x | x <-[1..99], length(divs x) == 6] 

sixDivs :: Integral a => [a]

abcs = [take y [ x | x <- ['a'..'z']] | y <-[1..26]]

abcs :: [[Char]]

divs n = [ x | x <- [1..n], mod n x == 0]
divPairs = [(x,length(divs x))| x <- [1..100]]

divPairs :: Integral a => [(a, Int)]

divs n = [ x | x <- [1..n], mod n x == 0]
perfects = [x | x<-[1..], sum(divs x) - x == x]

Integral a => [a]

evenRange x y = [z | z<-[x..y], even(z)]

evenRange :: Integral a => a -> a -> [a]

notDivBy x = [ y | y<-[1..], mod y x /= 0]

notDivBy :: Integral a => a -> [a]

increaser x = [ y+1 | y<-x]

increaser :: Num a => [a] -> [a]

listofLengths y = [length(x)| x<-y]

listofLengths :: Foldable t => [t a] -> [Int]

isAPrefix x y = take (length(x)) y == x

isAPrefix :: Eq a => [a] -> [a] -> Bool



