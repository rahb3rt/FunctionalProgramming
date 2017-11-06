summa :: Integral a => [a] -> a
summa xs =  foldr (\x y-> x+y) 0 ((map (\x->if even x then x else (-1)*x)) xs)

wordCount :: String -> String -> Int
wordCount x = length . words

exclaim :: String -> [Char]
exclaim x  = concat (map (++ "! ") (words x))

ratMult :: Rat -> Rat -> Rat
ratMult (Over a b) (Over c d) = Over (a*c) (b*d)

ratLegal :: Rat -> Bool 
ratLegal (Over _ b) = (b /= 0)

ratReduce :: Rat -> Rat
ratReduce (Over a b) = Over (div a (gcd a b)) (gcd a b)

ratLeq :: Rat -> Rat -> Bool
ratLeq (Over a b) (Over c d)= ((fromIntegral a :: Float) / (fromIntegral b :: Float)) <= ((fromIntegral c :: Float) / (fromIntegral d :: Float))

quadEval :: Quad -> Float -> Float 
quadEval (Coeffs a b c) d  = (a * (d)^2) + (b * d) + c 

quadAdd :: Quad -> Quad -> Quad
quadAdd (Coeffs a b c) (Coeffs d e f) = Coeffs (a + d) (b + e) (c + f) 

quadDerive :: Quad -> Quad 
quadDerive (Coeffs a b c) = Coeffs a*2 b 0

quadRoots :: Quad -> (Float, Float) 
quadRoots (Coeffs a b c) = (-b + sqrt (b^2 + 4 * a * c) / (2 * a)
