f x
    | x > 5 = "nickelback"
    | x < 5 = "creed"
    | otherwise = "hootie"
    | x == 5 = "dmb"

g (2:as) = 4
g (3:as) = 8
g (a:as) = a+3
