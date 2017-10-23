f (x:xs)
    | x <= 4 = 1 + f xs
    | otherwise = 2 * f xs

g 0 = 1
g x = x^2 + g (x-1)


