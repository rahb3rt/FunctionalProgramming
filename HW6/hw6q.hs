deVowel "" = ""
deVowel (c:cs) 
    | elem c "aeiou"   = deVowel cs
    | otherwise        = c : (deVowel cs)

collatz :: Int -> Int
collatz x = if (even x) then (div x 2) else (3*x+1)

collatzSequence :: Int -> [Int]
collatzSequence 1 = []
collatzSequence x = (collatz x) : (collatzSequence (collatz x))

