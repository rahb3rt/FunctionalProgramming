isDivBy x y = (x `mod` y == 0)

isEven x = ( x `mod` 2 == 0)

isOdd x = ( x `mod` 2 /= 0) 

collatz x = if( x `mod` 2 == 0) then div x 2 else 3*x+1

bro x = 
    if (head x < 'g') 
        then "Dude!" 
        else if (head x >= 'h' || head x <= 'r' ) 
            then "Sweet!" 
            else "Bummer"

upToDouble x = take (x+1) [x..]

mirrorRangeNoZero x = take (x) [-x..] ++ [1..x]

evenRange x y = filter even [x..y]

chopper x  = iterate tail (iterate init x !! 3) !! 3

takeTwice n l = takeTwice n l =  (take n l) ++ (take n l) 
    --length(takeTwice n l) = 2n
